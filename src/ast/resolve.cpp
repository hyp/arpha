#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "resolve.h"
#include "interpret.h"
#include "analyze.h"
#include "../syntax/parser.h"
#include "../intrinsics/types.h"


//Typecheks an expression
Node* typecheck(Node* expression,Type* expectedType){
	if(auto assigns = expectedType->assignableFrom(expression,expression->returnType())){
		return assigns;
	}
	else {
		error(expression,"Expected an expression of type %s instead of %s of type %s",expectedType,expression,expression->returnType());
		return expression;
	}
}

namespace overloads {

struct SearchResult {
	data::ast::Search::Result result;
	Function* function;

	SearchResult(Function* f) : result(data::ast::Search::Found),function(f) {}
	SearchResult(data::ast::Search::Result kind) : result(kind) {}
	SearchResult(data::ast::Search::Result kind,Function* f) : result(kind),function(f) {}

	inline bool operator == (data::ast::Search::Result kind) const { return result == kind; }
};

OverloadRange::OverloadRange(Scope* scope,SymbolID function,bool dotSyntax){
	funcCurr        = nullptr;
	distance = 0;
	this->scope     = scope;
	adjacentScope = nullptr;
	importsCurr     = nullptr;
	this->functionName = function;
	this->dotSyntax = dotSyntax;
	getNextFuncIterators();
}
void OverloadRange::nextScope(){
	if(!importsCurr){
		distance++;
		//check imported scopes
		if(!scope->imports.size()){
			distance++;
			if(scope->parent2) adjacentScope = scope->parent2;
			scope = scope->parent;
		}
		else {
			importsCurr = scope->imports.begin()._Ptr;
			importsEnd  = scope->imports.end()._Ptr;
			prevScope = scope;
			scope = *importsCurr;
		}
	}
	else {
		importsCurr++;
		if(importsCurr >= importsEnd){
			distance++;
			assert(!prevScope->parent2);//NB: the generate function container doens't import anything
			if(!prevScope->parent && adjacentScope){
				scope = adjacentScope;
				adjacentScope = nullptr;
			}
			else scope = prevScope->parent;
			importsCurr = nullptr;
		}
		else scope = *importsCurr;
	}
}
void OverloadRange::getNextFuncIterators(){
	do {
		if(auto overloadset = scope->containsOverloadset(functionName)){
			funcCurr = overloadset->functions.begin()._Ptr;
			funcEnd  = overloadset->functions.end()._Ptr;
			return;
		}
		nextScope();
	} while(scope);
}
void OverloadRange::advance(){
	funcCurr++;
	if(funcCurr >= funcEnd){
		nextScope();
		if(scope) getNextFuncIterators();
	}
}

/**
  Tries to find a function that matches the pointer type from all the possible overloads from the perspective of a given scope.
*/
SearchResult findFunctionByType(Scope* scope,SymbolID name,FunctionPointer* type){
	Function* match = nullptr;
	for(OverloadRange overloads(scope,name,false);!overloads.isEmpty();overloads.advance()){
		auto f = overloads.currentFunction();
		if(!f->isResolved()) return SearchResult(data::ast::Search::NotAllElementsResolved);

		if( f->argumentType()->isSame(type->parameter()) &&
			f->returns()->isSame(type->returns()) &&
			f->callingConvention() == type->callingConvention() ){

			if(match){
				return SearchResult(data::ast::Search::FoundMultiple,match);
			}
			match = f;
		}
	}
	return match? SearchResult(match) : SearchResult(data::ast::Search::NotFound);
}


}

Resolver::Resolver(CompilationUnit* compilationUnit) : _compilationUnit(compilationUnit),isRHS(false),reportUnevaluated(false),expectedTypeForEvaluatedExpression(nullptr) {
	unresolvedExpressions = 0;
	treatUnresolvedTypesAsResolved = false;
	currentFunction = nullptr;
	currentTrait    = nullptr;
	_currentParent  = nullptr;

	//TODO
	inliningThreshold[0] = 10;
	inliningThreshold[1] = 20;
}


Node* Resolver::resolve(Node* node){
	if(node->isResolved()) return node;
	auto result = node->resolve(this);
	if(!result->isResolved()) markUnresolved(result);
	else if(result->returnType()->isReference()){
		result = result->copyLocationSymbol(new PointerOperation(result,PointerOperation::DEREFERENCE));

		//NB: speed optimization
		//result->setFlag(Node::RESOLVED);
		result = resolve(result);
	}
	return result;
}

//Resolves children and returns true if all are resolved!
static bool resolveChildren(NodeList* node,Resolver* resolver){
	bool allResolved = true;
	for(auto i = node->begin();i!=node->end();i++){
		*i = resolver->resolve(*i);
		if(!(*i)->isResolved()) allResolved = false;
	}
	return allResolved;
}

/**
* Resolving expressions
*/
Node* Node::resolve(Resolver* resolver){
	resolver->markResolved(this);
	return this;
}

Node* ArrayLiteral::resolve(Resolver* resolver){
	if(resolveChildren(this,resolver)){
		resolver->markResolved(this);
		bool allConst = true;
		for(auto i = begin();i!=end();i++){ if(!(*i)->isConst()) allConst = false; }
		if(allConst) setFlag(CONSTANT);
	}
	return this;
}

Node* VariableReference::resolve(Resolver* resolver){
	if(variable->asConstantSubstitute() && !resolver->isRHS){
		DuplicationModifiers mods(resolver->currentScope());
		return copyLocationSymbol(variable->asConstantSubstitute()->duplicate(&mods));//transforms constant pi into 3.14
	}
	if(variable->isResolved()) resolver->markResolved(this);
	return this;
}

Node* TypeReference::resolve(Resolver* resolver){
	if(type->isResolved() || resolver->treatUnresolvedTypesAsResolved){
		resolver->markResolved(this);
		setFlag(CONSTANT);
	}
	return this;
}
 
Node* TupleExpression::resolve(Resolver* resolver){
	assert(size() > 1);
	if(resolveChildren(this,resolver)){
		resolver->markResolved(this);
		//find the tuple's type!
		std::vector<AnonymousAggregate::Field> fields;
		bool allTypes = true;
		bool allConst = true;
		for(auto i = begin();i!=end();i++){
			if(!(*i)->isConst()) allConst = false;
			auto returns = (*i)->returnType();
			if(returns->isVoid()){
				error((*i),"A tuple can't contain an expression returning Nothing!");
				return ErrorExpression::getInstance();
			} else if(!returns->isType() || !(*i)->isConst()) allTypes = false;
			AnonymousAggregate::Field field = { (*i)->label(),returns};
			fields.push_back(field);
		}
		if(allConst) setFlag(CONSTANT);
		//int32,int32 :: Type,Type -> anon-record(int32,int32) :: Type
		if(resolver->expectedTypeForEvaluatedExpression && resolver->expectedTypeForEvaluatedExpression->isType() && allTypes){
			auto children = childrenPtr();
			for(size_t i =0;i<size();i++){
				fields[i].type = children[i]->asTypeReference()->type;
			}
			return resolver->resolve( new TypeReference(AnonymousAggregate::create(&fields[0],fields.size())) );
		}
		else {
			type = AnonymousAggregate::create(&fields[0],fields.size());
		}
	}
	return this;
}

//Resolve foo.bar(...)
static Node* transformCallOnAccess(CallExpression* node,AccessExpression* acessingObject){
	//a.foo()
	if(node->arg->asUnitExpression())
		node->arg = acessingObject->object;
	//a.foo(bar)
	else{
		if(auto isArgRecord = node->arg->asTupleExpression())
			isArgRecord->children.insert(isArgRecord->children.begin(),acessingObject->object);
		else {
			auto tuple = new TupleExpression();
			tuple->addChild(acessingObject->object);
			tuple->addChild(node->arg);
			node->arg = tuple;
		}
	}
	node->object = new UnresolvedSymbol(node->location(),acessingObject->symbol);
	node->setFlag(CallExpression::DOT_SYNTAX);
	return node;
}

Node* evaluateConstantOperation(data::ast::Operations::Kind op,Node* parameter);

bool isSimple(Node* node){
	if(node->isFlagSet(Node::CONSTANT) || node->asVariableReference()) return true;
	return false;
}

/**
Inlines a function call.
Scenarios: 
	f(x,y) = x * y

	f(a(),b()) => 
	{
		var x = a(); var y = b()
		var ret
		{ ret = x * y }
		ret
	}

	f(1,y) => 
	{
		var ret
		{ ret = 1 * y }
		ret
	}
*/
Node* Resolver::inlineCall(Function* function,Node* parameters){

	BlockExpression* wrapper = new BlockExpression();
	wrapper->scope->parent   = currentScope();
	wrapper->setFlag(BlockExpression::RETURNS_LAST_EXPRESSION);
	wrapper->_location = parameters->location();

	DuplicationModifiers mods(wrapper->scope);

	if(function->arguments.size()){
		Node** parametersPointer;
		if(auto tuple = parameters->asTupleExpression()) parametersPointer = tuple->childrenPtr();
		else parametersPointer = &parameters;

		size_t j = 0;
		for(auto i = function->arguments.begin();i!=function->arguments.end();++i,++j){
			Node* expansion;
			if(isSimple(parametersPointer[j])) expansion = parametersPointer[j];
			else {
				auto var = new Variable((*i)->label(),parametersPointer[j]->location());
				var->type.specify((*i)->type.type());
				wrapper->addChild(new AssignmentExpression(var,parametersPointer[j]));
				expansion = new VariableReference(var);
				expansion->_location = parametersPointer[j]->location();
			} 
			mods.expandArgument((*i),expansion);
		}
	}
	if(!function->returns()->isVoid()){
		auto var = new Variable("A@ret",parameters->location());
		var->type.specify(function->returns());
		wrapper->addChild(var);
		mods.returnValueRedirector = var;
	}

	auto  block = &function->body;
	if(block->size()){
		wrapper->addChild(block->duplicateMixin(&mods));
	}

	wrapper->addChild(mods.returnValueRedirector? (Node*)new VariableReference(mods.returnValueRedirector) : new UnitExpression());
	return wrapper;	
}

static Node* potentiallyInline(Resolver* resolver,Function* function,Node* param){
	if(!function->intrinsicCTFEbinder && !function->isExternal() && !function->isIntrinsicOperation() && 
		function->inliningWeight < resolver->inliningThreshold[function->generatedFunctionParent? 1 : 0]){
		return resolver->inlineCall(function,param);
	}
	return nullptr;
}

Node* CallExpression::resolve(Resolver* resolver){
	arg  = resolver->resolve(arg);
	if(!arg->isResolved()) return this;

	// symbol(arg)
	if(auto callingOverloadSet = object->asUnresolvedSymbol()){
		auto scope = (callingOverloadSet->explicitLookupScope ? callingOverloadSet->explicitLookupScope : resolver->currentScope());
		
		if(auto os = scope->lookupPrefix(callingOverloadSet->symbol)){
			if(auto decl = os->asTypeDeclaration()){
				object= callingOverloadSet->copyLocationSymbol(resolver->resolve(decl->createReference()));
				if(!decl->type()->asTrait())
					resolver->markResolved(this);
				return this;
			}
			else assert(os->asOverloadset());
		}
		if(auto func =  resolver->resolveOverload(scope,callingOverloadSet->symbol,arg,isFlagSet(DOT_SYNTAX))){
			arg = resolver->resolve(resolver->constructFittingArgument(&func,arg));
			//macro
			if(func->isFlagSet(Function::MACRO_FUNCTION)){
				if(func->intrinsicCTFEbinder){
					CTFEintrinsicInvocation i(resolver->compilationUnit());
					i.invoke(func,arg);
					if(auto err = i.result()->asErrorExpression()) return err;
					return resolver->resolve(copyLocationSymbol(i.result()->asNodeReference()->node()));
				}
				else return resolver->executeAndMixinMacro(func,arg);
			}
			else if(func->isTypeTemplate() && !func->isIntrinsic()){
				debug("Type generation functions booyah!");
				return resolver->resolve(copyLocationSymbol(func->getTemplateTypeDeclaration()->createReference()));
			}
			else if(func->isFieldAccessMacro()){
				if(auto t = arg->asTupleExpression()){
					assert(t->size() == 2);
					return resolver->resolve(copyLocationSymbol(new AssignmentExpression( new FieldAccessExpression(*t->begin(),func->getField()) , *(t->begin()+1) )));
				}
				return resolver->resolve(copyProperties(new FieldAccessExpression(arg,func->getField())));
			} else if(func->isIntrinsicOperation() && arg->isConst()){
				return resolver->resolve(copyLocationSymbol(evaluateConstantOperation(func->getOperation(),arg)));
			}
			object = resolver->resolve(new FunctionReference(func));
			if(!func->isResolved()){
				assert(func->generatedFunctionParent);
				return this;
			}
			resolver->markResolved(this);
			if(func->intrinsicCTFEbinder && !func->isFlagSet(Function::INTERPRET_ONLY_INSIDE) && arg->isConst()){
				CTFEintrinsicInvocation i(resolver->compilationUnit());
				i.invoke(func,arg);
				return resolver->resolve(copyLocationSymbol(i.result()));
			}
			if(auto inl = potentiallyInline(resolver,func,arg)) return resolver->resolve(copyLocationSymbol(inl));
		}
	} 
	else if(auto callingFunc = object->asFunctionReference()){
		if(!callingFunc->function->isResolved()){
			/**
			NB: given the situation:
			    type Bar(T) { var x T; def foo(self) = self.x }
			    var bar Bar(int32)
			    bar.foo
			where the expansion of foo(x *Bar(int32)) can't be resolved straight away, we resolve the function ourselves
			*/
			assert(callingFunc->function->generatedFunctionParent);
			if(!resolver->resolveSpecialization(callingFunc->function))
				return this;
		}
		resolver->markResolved(this);
		auto func = callingFunc->function;
		if(func->intrinsicCTFEbinder && !func->isFlagSet(Function::INTERPRET_ONLY_INSIDE) && arg->isConst()){
			CTFEintrinsicInvocation i(resolver->compilationUnit());
			i.invoke(func,arg);
			return resolver->resolve(copyLocationSymbol(i.result()));
		}
		if(auto inl = potentiallyInline(resolver,func,arg)) return resolver->resolve(copyLocationSymbol(inl));
	}
	else if(auto callingAccess = object->asAccessExpression()){
		return resolver->resolve(transformCallOnAccess(this,callingAccess));
	}
	else if(auto type = object->asTypeReference()){
		if(!type->type->asTrait())
			resolver->markResolved(this);
	}
	else if(object->asVariableReference() && object->returnType()->isFunctionPointer()){
		resolver->markResolved(this);
	}
	else {
		error(object,"Invalid function call expression - %s(%s)!",object,arg);
		return ErrorExpression::getInstance();
	}

	return this;
}

Node* LogicalOperation::resolve(Resolver* resolver){
	parameters[0] = resolver->resolve(parameters[0]);
	parameters[1] = resolver->resolve(parameters[1]);
	if(parameters[0]->isResolved() && parameters[1]->isResolved()){
		resolver->markResolved(this); 
	}
	return this;
}

/**
* Maps Anonymous records directly to fields
*/
Node* accessingAnonymousRecordField(AccessExpression* node){
	auto returns = node->object->returnType();
	if(returns->isPointer()) returns = returns->next();
	if(auto record = returns->asAnonymousRecord()){
		auto fieldID = record->lookupField(node->symbol);
		if(fieldID != -1){
			//Field access
			if(auto tuple = node->object->asTupleExpression()) //simplify (x:1,y:1).x => 1
				return tuple->childrenPtr()[fieldID];
			else
				return new FieldAccessExpression(node->object,fieldID);
		}
	}
	return nullptr;
}

// TODO tuple destructuring
// TODO anonymous records a.foo

void inferVariablesType(Variable* variable,AssignmentExpression* assignment,Type* valuesType){
	if(variable->asArgument()) return;
	auto value = assignment->value;

	if(!variable->deduceType(valuesType)){
		error(assignment,"Failed to deduce variable's type -\n\tA variable '%s' is expected to have a type matching a pattern %s, which the type '%s' derived from the expression %s doesn't match!",
			variable->label(),variable->type.pattern,valuesType,assignment->value);
	}
	debug("Inferred type for %s",variable);
}

/**
Tuple destructuring:

var x,y = getTuple() => { var t = getTuple(); x = t[0]; y = t[1] }
var x,y = tuple      => { x = tuple[0]; y = tuple[1]             }
*/
void destructure(BlockExpression* wrapper,TupleExpression* object,Node* value,AnonymousAggregate* valuesType){
	DuplicationModifiers mods(nullptr);
	Variable* var;
	if(object->size() != valuesType->numberOfFields){
		error(value,"Can't destructure between tuples of different length!");
		return;
	}
	if(!isSimple(value)){
		var = new Variable("A@dest",object->location());
		var->type.specify(valuesType);
		wrapper->addChild(new AssignmentExpression(var,value));
	} else var = nullptr;

	auto ptr = object->childrenPtr();
	for(size_t i =0 ;i<valuesType->numberOfFields; ++i){
		wrapper->addChild(new AssignmentExpression(ptr[i],new FieldAccessExpression(var? new VariableReference(var) : value->duplicate(&mods) ,i)));
	}
}

Node* AssignmentExpression::resolve(Resolver* resolver){

	struct Splitter {
		BlockExpression* dest;
		Location location;

		void split(Node* object,Node* value){
			if(auto t1 = object->asTupleExpression()){
				if(auto t2 = value->asTupleExpression()){
					if(t1->size() == t2->size()){
						for(size_t i=0;i<t1->size();i++){
							split(t1->children[i],t2->children[i]);
						}
					}
					else error(t1,"Can't assign between tuples of different length");
					return;
				}
			}
			auto assignment= new AssignmentExpression(object,value);
			assignment->_location = object->location();
			dest->addChild(assignment);
		}
	};

	//split
	if(object->asTupleExpression() && value->asTupleExpression()){
		auto block = new BlockExpression();
		block->setFlag(BlockExpression::USES_PARENT_SCOPE);
		Splitter splitter = { block };
		splitter.split(object,value);
		return resolver->resolve(block);
	}

	//assign
	value = resolver->resolve(value);
	if(!value->isResolved()) return this;
	auto valuesType = value->returnType();

	//non tuple object
	
	object = resolver->resolve(object);

	//destructuring
	if(auto tuple = object->asTupleExpression()){
		if(auto record = valuesType->asAnonymousRecord()){
			auto block = new BlockExpression();
			block->setFlag(BlockExpression::USES_PARENT_SCOPE);
			destructure(block,tuple,value,record);
			return resolver->resolve(block);
		}
	}

	Variable* variable;
	if(auto var = object->asVariableReference()){
		variable = var->variable;
	}
	else {
		variable = object->asVariable();
	}
	if(!object->isResolved()){
		if(!variable || !variable->type.isPattern())
			return this;
	}

	//Assigning values to variables
	if(variable){
		//type inferring
		if(variable->type.isPattern()){
			inferVariablesType(variable,this,valuesType);
			object = resolver->resolve(object);
		}
		
		if(!variable->type.isResolved()) return this;

		//def - dissallow assignments to references
		if(variable->isFlagSet(Variable::IS_IMMUTABLE)){
			if(!object->asVariable()) error(value,"Can't assign %s to a constant variable %s!",value,object);
		}

		//perform the actual assignment
		Type* expectedType = variable->type.type();

		if(auto canBeAssigned = expectedType->assignableFrom(value,valuesType)){
			value = resolver->resolve(canBeAssigned);
			if(variable->isFlagSet(Variable::IS_IMMUTABLE) && value->isConst()){
				variable->setImmutableValue(value);
			}
			resolver->markResolved(this);
		} else {
			error(value,"Can't assign %s to %s - the types don't match!",value,object);
			return ErrorExpression::getInstance();
		}
	}
	else if(object->isResolved()) {
		Type* expectedType;

		if(auto access = object->asFieldAccessExpression())
			expectedType = access->fieldsType();
		else if(object->asPointerOperation() && object->asPointerOperation()->kind == PointerOperation::DEREFERENCE)
			expectedType = object->returnType();
		else {
			error(object,"Can't perform an assignment to %s - only variables, fields, derefernced pointers and references are assignable!",object);
			return ErrorExpression::getInstance();
		}

		if(auto canBeAssigned = expectedType->assignableFrom(value,valuesType)){
			value = resolver->resolve(canBeAssigned);
			resolver->markResolved(this);
		}
		else {
			error(value,"Can't assign %s to %s - the types don't match!",value,object);
			return ErrorExpression::getInstance();
		}
	}
	
	return this;
}

Node* PointerOperation::resolve(Resolver* resolver){
	expression = resolver->resolve(expression);
	if(expression->isResolved()){
		if(isDereferenceOrType()){
			if(auto tref = expression->asTypeReference()){
				if(tref->type->isTrait()) return this;
				tref->type = Type::getPointerType(tref->type);
				return copyLocationSymbol(tref);
			}
			else {
				kind = DEREFERENCE;
			}
		}
		resolver->markResolved(this);
		if(isDereference()){
			auto ret = expression->returnType();
			if(!(ret->isPointer() || ret->isReference() )){
				error(this,"Can't dereference a non-pointer expression!");
				return ErrorExpression::getInstance();
			}
		}
	}
	return this;
}

Node* ReturnExpression::resolve(Resolver* resolver){
	auto func = resolver->currentFunction;
	if(!func) return this; //Error reported during analysis

	func->setFlag(Function::CONTAINS_RETURN);
	expression = resolver->resolve(expression);
	if(expression->isResolved()){

		//Type checking..
		if(func->_returnType.isPattern()){
			auto valRet = expression->returnType();
			if(!func->_returnType.deduce(valRet,func->body.scope)){
					error(this,"Failed to deduce function's return type -\n\tA function %s is expected to return a type matching a pattern %s, which the type %s derived from the expression %s doesn't match!",
					func->label(),func->_returnType.pattern,valRet,expression);
			}
			resolver->markResolved(this);
			debug("Inferred return type %s for function %s",valRet,func->label());
		}
		else if(func->_returnType.isResolved()){
			expression = resolver->resolve(typecheck(expression,func->_returnType.type()));
			resolver->markResolved(this);
		}
	}
	return this;
}

Node* ThrowExpression::resolve(Resolver* resolver){
	expression = expression->resolve(resolver);
	if(expression->isResolved()) resolver->markResolved(this);
	return this;
}

// { 1 } => 1
static Node* simplifyBlock(BlockExpression* block){
	if(block->size() == 1 && block->scope->numberOfDefinitions() == 0) return *(block->begin());
	return block;
}

Node* IfExpression::resolve(Resolver* resolver){
	condition   = resolver->resolve(condition);
	if(condition->isResolved()){
		condition   = resolver->resolve(typecheck(condition,intrinsics::types::boolean));	
		consequence = resolver->resolve(consequence);
		alternative = resolver->resolve(alternative);
		if(consequence->isResolved() && alternative->isResolved()){
			if(auto block = consequence->asBlockExpression()) consequence = simplifyBlock(block);
			if(auto block = alternative->asBlockExpression()) alternative = simplifyBlock(block);
			resolver->markResolved(this);
		}
	}
	return this;
}

Node* LoopExpression::resolve(Resolver* resolver){
	body = resolver->resolve(body);
	if(body->isResolved()) resolver->markResolved(this);
	return this;
}

Node* evaluateConstantCast(Node* expression,Type* givenType);

Node* CastExpression::resolve(Resolver* resolver){
	object = resolver->resolve(object);
	if(object->isResolved()){
		resolver->markResolved(this);
		auto returns = object->returnType();
		if(type->isSame(returns)) return object;
		else if(auto expression = type->assignableFrom(object,returns)){
			return copyLocationSymbol(resolver->resolve(expression));
		}
		else if(returns->canCastTo(type)){
			if(object->isConst()) return copyLocationSymbol(resolver->resolve(evaluateConstantCast(object,type)));
		}
		else {
			error(this,"Can't cast %s to %s!",returns,type);
			return ErrorExpression::getInstance();
		}
	}
	return this;
}

Node* BlockExpression::resolve(Resolver* resolver){
	scope->_functionOwner = resolver->currentFunction;
	parentNode = resolver->currentParentNode();
	

	auto oldScope = resolver->currentScope();
	resolver->currentScope(scope);
	auto oldParent = resolver->currentParentNode();
	resolver->currentParentNode(this);

	setFlag(ITERATING);
	bool allResolved = true;
	size_t size = this->size();
	for(size_t i = 0;i<size;i++){
		children[i] = resolver->resolve(children[i]);
		if(!children[i]->isResolved()) allResolved = false;
		//Uglyness in it's glory :(
		if(isFlagSet(ITERATION_MODIFIED_CHILDREN)){
			size = this->size();
			flags &= (~ITERATION_MODIFIED_CHILDREN);
		}
	}
	flags &= (~ITERATING);

	if(allResolved) resolver->markResolved(this); 
	resolver->currentParentNode(oldParent);
	resolver->currentScope(oldScope);
	return this;
}

Node* UnresolvedSymbol::resolve(Resolver* resolver){
	//TODO fix
	//{ Foo/*Should be type Foo */; var Foo int32 } type Foo <-- impossibru	
	if(auto def = (explicitLookupScope ? explicitLookupScope : resolver->currentScope())->lookupPrefix(symbol)){
		if(auto ref = def->createReference()){
			return resolver->resolve(copyLocationSymbol(ref));
		}
	}
	return this;
}

Node* AccessExpression::resolve(Resolver* resolver){
	object = resolver->resolve(object);
	if(object->isResolved()){
		if(auto tref = object->asTypeReference()){
			if(auto variant = tref->type->asVariant()){
				auto decl   = variant->declaration;
				if(decl->optionalStaticBlock){
					if(auto def = decl->optionalStaticBlock->scope->containsPrefix(symbol)){
						return resolver->resolve(copyLocationSymbol(def->createReference()));
					}
				}
			}
		}
		if(auto f = accessingAnonymousRecordField(this)){
			return resolver->resolve(copyLocationSymbol(f));
		}
		// a.foo => foo(a)
		auto result = new CallExpression(new UnresolvedSymbol(location(),symbol),object);
		result->setFlag(CallExpression::DOT_SYNTAX);
		return resolver->resolve(copyLocationSymbol(result));
	}
	return this;
}

Node* MatchResolver::resolve(Resolver* resolver){
	object = resolver->resolve(object);
	if(object->isResolved()){
		//yes!
		if(auto type = object->asTypeReference()){
			for(auto i = begin();i!=end();i+=2){
				TypePatternUnresolvedExpression pattern;
				pattern.kind = TypePatternUnresolvedExpression::UNRESOLVED;
				pattern.unresolvedExpression = *i;
				pattern.resolve(resolver);
				bool matches = pattern.isResolved() ? pattern.type()->isSame(type->type) : false;
				if(pattern.isPattern()){
					auto scope = (*(i+1))->asBlockExpression()->scope;
					TypePatternUnresolvedExpression::PatternMatcher matcher(scope,resolver);
					if(matcher.match(type->type,pattern.pattern)){
						matches = true;
						matcher.defineIntroducedDefinitions();
					}
					else if(matcher.notAllResolvedMatch){
						return this;
					}
				}
				if(matches){
					auto result = resolver->resolve(*(i+1));
					if(auto block = result->asBlockExpression()) result = simplifyBlock(block);
					return result;
				}
			}
			return this; //Can't match..
		}
		//Integers and booleans
		auto returns = object->returnType();
		if(returns->isBool() || returns->isInteger()){
			//| pattern => if(object == pattern)
			Node* firstBranch;
			IfExpression* lastBranch = nullptr;
			for(auto i = begin();i!=end();i+=2){
				if(auto sym = (*i)->asUnresolvedSymbol()){
					if(sym->symbol == "_"){
						if(i != (end() - 2)){
							error(sym,"The \"_\" default match must be the last matching branch!");
							return ErrorExpression::getInstance();
						}
						if(lastBranch) lastBranch->alternative = *(i+1); //no conditional needed
						else firstBranch = *(i+1);
						break;
					}
				}
				IfExpression* newBranch = new IfExpression(new CallExpression(new UnresolvedSymbol((*i)->location(),"equals"),new TupleExpression(object,*i)),*(i+1),nullptr);
				newBranch->setFlag(IfExpression::MATCH_SYNTAX);
				if(lastBranch) lastBranch->alternative = newBranch;
				else firstBranch = newBranch;
				lastBranch = newBranch;
			}
			if(lastBranch && lastBranch->alternative == nullptr) lastBranch->alternative = new UnitExpression();
			return resolver->resolve(copyLocationSymbol(firstBranch));
		}
		else {
			error(object,"Can't resolve a match on an object %s of type %s!",object,returns);
			return ErrorExpression::getInstance();
		}
	}
	return this;
}

#include "../base/system.h"

// TODO better error reporting!
void Resolver::reportUnresolvedNode(Node* node){
	if(!node->asBlockExpression() && !node->asErrorExpression() && !node->asTupleExpression()){
		std::string error;
		if(auto unr = node->asUnresolvedSymbol()){
			error = format("Can't resolve the symbol '%s'",unr->symbol);
		} 
		else if(auto call = node->asCallExpression()){

			if(auto unr = call->object->asUnresolvedSymbol()) {
				if(call->isFlagSet(CallExpression::DOT_SYNTAX)) 
					 error = format("Can't find the matching overload for the call %s.(%s)",unr->symbol,call->arg);
				else error = format("Can't find the matching overload for the call %s(%s)",unr->symbol,call->arg);
				compiler::subError(node->location(),error);
				auto scope = (unr->explicitLookupScope ? unr->explicitLookupScope : currentScope());
				

				bool header = false;
				for(overloads::OverloadRange overloads(scope,unr->symbol,call->isFlagSet(CallExpression::DOT_SYNTAX));!overloads.isEmpty();overloads.advance()){
					if(!header){
						System::print(format("\tThe available overloads for the function '%s' are:",unr->symbol));
						header = true;
					}
					std::stringstream def;
					auto f = overloads.currentFunction();
					def<<"\n\t  def "<<f->label()<<"(";
					for(auto i = f->arguments.begin();i!=f->arguments.end();++i){
						def<<(*i)->label()<<" "<<(*i)->type;
						if((*i)->defaultValue()) def<<" = "<<(*i)->defaultValue();
						if((i+1) != f->arguments.end()) def<<",";
					}
					def<<") "<<f->_returnType;
					System::print(def.str());
				}
				if(!header)
					System::print(format("\tThere are no available overloads for the function '%s'! Perphaps you've misspelled the function?\n",unr->symbol));
				else System::print("\n\n");
				return;
			}
		} 
		else if(auto var = node->asVariable()){
			error = format("Can't resolve the variable '%s'",var->label());
		}
		else if(auto func = node->asFunction()){
			error = format("Can't resolve the function '%s'",func->label());
		}
		else error = format("Can't resolve expression %s",node);
		compiler::subError(node->location(),error);
	}
}

void Resolver::markUnresolved(Node* node){
	unresolvedExpressions++;
	if(reportUnevaluated) reportUnresolvedNode(node);
}

bool TypePatternUnresolvedExpression::resolve(Resolver* resolver,PatternMatcher* patternMatcher){
	assert(kind == UNRESOLVED);
	auto oldSetting = resolver->expectedTypeForEvaluatedExpression;
	resolver->expectedTypeForEvaluatedExpression = intrinsics::types::Type;
	unresolvedExpression = resolver->resolve(unresolvedExpression);
	resolver->expectedTypeForEvaluatedExpression = oldSetting;
	
	if(auto isTypeRef = unresolvedExpression->asTypeReference()){
		if(!isTypeRef->type->isTrait()){
			if(isTypeRef->isResolved()){
				kind  = TYPE;
				_type = isTypeRef->type;
				return true;
			}
			return false;
		}
	} 

	//pattern type?
	if(patternMatcher){
		auto oldSize = patternMatcher->introducedDefinitions.size();
		if(patternMatcher->check(unresolvedExpression,resolver->currentTrait)){
			kind = PATTERN;
			pattern = unresolvedExpression;
			return true;
		} else if(oldSize != patternMatcher->introducedDefinitions.size()) 
			patternMatcher->introducedDefinitions.erase(patternMatcher->introducedDefinitions.begin() + oldSize,patternMatcher->introducedDefinitions.end());
	} else {
		PatternMatcher matcher(resolver->currentScope(),resolver);
		if(matcher.check(unresolvedExpression)){
			kind = PATTERN;
			pattern = unresolvedExpression; //NB: not really necessary because they are in one union together
			return true;
		}
	}

	return false;
}

/**
* Resolving definition nodes
*/
Node* Variable::resolve(Resolver* resolver){
	_owner = resolver->currentScope();
	parentNode = resolver->currentParentNode();

	auto _resolved = true;
	if(type.isResolved()) _resolved = true;
	else if(type.isPattern()) _resolved = false;
	else _resolved = type.resolve(resolver) && type.isResolved();

	if(_resolved) {
		setFlag(RESOLVED);
		if(!type.type()->isValidTypeForVariable()) error(this,"A variable '%s' can't have a type %s",label(),type.type());
	}
	return this;
}

Node* Argument::resolve(Resolver* resolver){
	//Type (NB: allow patterns)
	auto _resolved = type.kind == TypePatternUnresolvedExpression::UNRESOLVED ? type.resolve(resolver,&functionOwner()->allArgMatcher) : true;

	if(!_resolved) return false;
	//Default value
	if(_defaultValue){
		_defaultValue = resolver->resolve(_defaultValue);
		if(_defaultValue->isResolved() && _defaultValue->isConst()){
			//infer type when ... ,x = 1, ...
			if(type.isPattern() && type.pattern == nullptr){
				type.deduce(_defaultValue->returnType(),_owner);
			}
			_resolved = true;
		} 
		else {
			if(_defaultValue->isResolved()) // Implies non constant default value!
				error(_defaultValue,"The default value to function's parameter must resolve to a constant expression!");
			_resolved = false;
		}
	}

	if(_resolved){
		setFlag(RESOLVED);
		if(type.isResolved() && !type.type()->isValidTypeForArgument())
			error(this,"The type '%s' can't be used as a parameter's type",type.type());
		//TODO What about x Arithmetic = false..?
		if(_defaultValue && type.isResolved()) _defaultValue = ::typecheck(_defaultValue,type.type());
	}
	return this;
}

bool TypeDeclaration::resolveExtendedConcepts(Resolver* resolver){
	for(auto i = extendedConcepts.begin();i!=extendedConcepts.end();++i){
		Node* pattern = nullptr;

		if((*i).isResolved()){
			if((*i).isPattern()) pattern = (*i).pattern;
		}
		else  {
			if((*i).resolve(resolver)){
				if((*i).isPattern()) pattern = (*i).pattern;
			}
			else return false;
		}

		if(pattern){
			if(auto tref = pattern->asTypeReference() ){
				if(tref->type->isTrait()) continue;
			}
		}

		error(pattern? pattern->location() : location(),"Invalid type extension: The type '%s' can only be extended with concept or interface!",label());
		return false;
	}
	return true;
}

//Record analysis
//Collects all the extenders field from the record extender hierarchy
static bool insertUniqueExtender(std::vector<Type*>& collection,Type* extender){
	for(auto i = collection.begin();i!=collection.end();i++){
		if((*i)->isSame(extender)){
			collection.push_back(extender);
			return false;
		}
	}
	collection.push_back(extender);
	return true;
}
static bool traverseExtenderHierarchy(Record* record,std::vector<Type*>& collection){
	for(auto i = record->fields.begin();i!=record->fields.end();i++){
		if((*i).type.isResolved() && (*i).isExtending){
			if(!insertUniqueExtender(collection,(*i).type.type())) return false;
		}
		if((*i).type.isResolved() && (*i).type.type()->isRecord())
			if(!traverseExtenderHierarchy((*i).type.type()->asRecord(),collection)) return false;
	}
	return true;
}

DeclaredType* Record::resolve(Resolver* resolver){
	auto allResolved = true;
	//Pretend unresolved types are resolved, so that expressions like type Foo { var x *Foo } can be evaluated
	resolver->treatUnresolvedTypesAsResolved = true;
	for(auto i = fields.begin();i!=fields.end();++i){
		if(!(*i).type.isResolved()){
			(*i).type.resolve(resolver);
			if(!(*i).type.isResolved() || !(*i).type.type()->isPartiallyResolved()) allResolved = false;
		} else if(!(*i).type.type()->isPartiallyResolved()) allResolved = false;
	}
	resolver->treatUnresolvedTypesAsResolved = false;
	if(!allResolved) return this;
	if(!declaration->resolveExtendedConcepts(resolver)) return this;

	//analyze
	std::vector<Type*> collection;
	if(!traverseExtenderHierarchy(this,collection)){
		error(declaration,"Faulty type extension hierarchy - The type %s features multiple path to type %s",declaration->label(),collection.back());
		return this;
	}
	setFlag(IS_RESOLVED);
	debug("Successfully resolved record type %s",declaration->label());
	return this;
}
// TODO getter self pointer type const and local, setter self pointer type local
std::pair<Function*,Function*> Record::createFieldGetterSetter(Location location,int fieldID){
	auto field = fields.begin() + fieldID;

	//getter
	auto getter = new Function(field->name,location);
	auto gthis = new Argument("self",location,getter);
	gthis->type.unresolvedExpression = new TypeReference(new Type(Type::POINTER,this));
	gthis->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
	getter->addArgument(gthis);
	getter->makeFieldAccess(fieldID);
	
	//setter
	auto setter = new Function(field->name,location);
	auto sthis  = new Argument("self",location,setter);
	sthis->type.unresolvedExpression = new TypeReference(new Type(Type::POINTER,this));
	sthis->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
	setter->addArgument(sthis);
	auto value = new Argument("value",location,setter);
	setter->addArgument(value);
	setter->makeFieldAccess(fieldID);

	if(field->isPrivate)                      getter->visibilityMode(data::ast::PRIVATE);
	if(field->isPrivate || field->isReadonly) setter->visibilityMode(data::ast::PRIVATE);

	return std::make_pair(getter,setter);
}
void Record::onTemplateSpecialization(Resolver* resolver){
	//introduce field getters and setters!
	auto block = resolver->currentParentNode()->asBlockExpression();

	for(size_t i =0;i < fields.size();i++){
		auto f = createFieldGetterSetter(this->declaration->location(),i);

		block->addChildPotentiallyDisturbingIteration(f.first);
		block->addChildPotentiallyDisturbingIteration(f.second);
		block->scope->defineFunction(f.first);
		block->scope->defineFunction(f.second);
	}
}

void generateDestructorBody(Record* type,Variable* selfObject,BlockExpression* body){
	auto size= type->fields.size();
	for(size_t i = 0; i<size;i++ ){
		if(type->fields[i].type.type()->requiresDestructorCall())
			body->addChild(new CallExpression(new UnresolvedSymbol(Location(),"destroy"),new PointerOperation(new FieldAccessExpression(new VariableReference(selfObject),i),PointerOperation::ADDRESS)));
	}
}

/**
	Every requirement must containt self in parameters or return type.
	Allowed:
		def f(self)
		def f() self
	Not allowed:
		def f()

	All trait parameters must be represented in requirements.
	Allowed:
		concept Foo(T) { def go(self) T }
	Not allowed:
		concept Foo(T) { def go(self) Nothing }
*/
bool Trait::verify(){
	std::vector<bool> parametersPresent;
	if(templateDeclaration){
		parametersPresent.resize(numberOfTemplateParameters(),false);
	}

	struct PatternMatcher {
		std::vector<bool>& parametersPresent;
		Trait* trait;
		bool hasSelf;

		bool match(Node* pattern){
			if(auto ptr = pattern->asPointerOperation()){
				pattern = ptr->expression;
			}
			if(auto tref = pattern->asTypeReference()){
				auto concept = tref->type->asTrait();
				if(concept == trait){
					hasSelf = true;
					return true;
				}
			}
			else if(auto tpref = pattern->asTraitParameterReference()){
				parametersPresent[tpref->index] = true;
				return true;
			}
			return false;
		}
	};


	PatternMatcher matcher = { parametersPresent,this,false };
	for(auto i = methods.begin();i!=methods.end();++i){
		matcher.hasSelf = false;

		for(auto arg = (*i)->arguments.begin();arg!=(*i)->arguments.end();++arg){
			if((*arg)->type.isResolved()) continue;
			else {
				assert((*arg)->type.isPattern());
				if(matcher.match((*arg)->type.pattern)) continue;
				error((*arg)->type.pattern,"Concept function requirement '%s' isn't allowed to have this type pattern for parameter '%s'",(*i)->label(),(*arg)->label());
			}
			return false;
		}
		
		if(!(*i)->_returnType.isResolved()){
			assert((*i)->_returnType.isPattern());
			if(!matcher.match((*i)->_returnType.pattern)){
				error((*i)->_returnType.pattern,"Concept function requirement '%s' isn't allowed to have this type pattern for the return value",(*i)->label());
			}
		}

		if(!matcher.hasSelf){
			error((*i),"Concept function requirement '%s' must have the concept as one of its parameters or as a return type",(*i)->label());
		}

	}

	if(templateDeclaration){
		for(size_t i = 0; i< parametersPresent.size();++i){
			if(!parametersPresent[i]){
				SymbolID paramName;
				for(auto defs = templateDeclaration->prefixDefinitions.begin();defs != templateDeclaration->prefixDefinitions.end();++defs){
					//NB: unecessary creation!
					if(auto node = defs->second->createReference()){
						auto p = node->asTraitParameterReference();
						if(p->index == i){
							paramName = defs->first;
							break;
						}
					}
				}
				error(this->declaration,"Concept '%s' can't deduce the value of the parameter '%s' from its requirements!",this->declaration->label(),paramName);
			}
		}
	}


	return true;
}

DeclaredType* Trait::resolve(Resolver* resolver){
	bool allResolved = true;
	resolver->currentTrait = this;

	if(templateDeclaration){
		templateDeclaration->parent = resolver->currentScope();
		resolver->currentScope(templateDeclaration);
	}

	for(auto i = methods.begin();i!=methods.end();++i){
		if(!(*i)->isResolved()){
			(*i)->resolve(resolver);
			if(!(*i)->isResolved()) allResolved = false;
		}
	}
	if(templateDeclaration)
		resolver->currentScope(templateDeclaration->parent);
	resolver->currentTrait = nullptr;

	if(allResolved && (declaration->optionalStaticBlock == nullptr || declaration->optionalStaticBlock->isResolved())){
		if(verify())
			setFlag(IS_RESOLVED);
	}
	return this;
}

DeclaredType* Variant::resolve(Resolver* resolver){
	if(!declaration->optionalStaticBlock || declaration->optionalStaticBlock->isResolved()){
		setFlag(IS_RESOLVED);
	}
	return this;
}

//static block parent
Node* TypeDeclaration::resolve(Resolver* resolver){
	parentNode = resolver->currentParentNode();

	if(optionalStaticBlock){
		optionalStaticBlock->scope->setParent(resolver->currentScope());
		if(!optionalStaticBlock->isResolved()){
			auto oldParent = resolver->currentParentNode();
			resolver->currentParentNode(this);
			optionalStaticBlock->resolve(resolver);
			resolver->currentParentNode(oldParent);
		}
	}
	if(!_type->isResolved())
		_type = _type->resolve(resolver);
	if(_type->isResolved() && (optionalStaticBlock? optionalStaticBlock->isResolved() : true)){
		setFlag(RESOLVED);
	}
	return this;
}

template<typename T>
struct ScopedStateChange {
	T  oldValue;
	T* dest;
	ScopedStateChange(T* old,T newValue) : oldValue(*old) , dest(old) {
		*old = newValue;
	}
	~ScopedStateChange(){
		*dest = oldValue;
	}
};


Node* Function::resolve(Resolver* resolver){
	parentNode = resolver->currentParentNode();
	ScopedStateChange<Function*> _(&resolver->currentFunction,this);

	//Resolve parameters!
	for(auto i = arguments.begin();i!=arguments.end();++i){
		if(!(*i)->isResolved()){
			(*i)->resolve(resolver);
			if(!(*i)->isResolved()) return this;
		}

		//inspect an argument
		if((*i)->isResolved() && !isIntrinsic()){
			if((*i)->type.isResolved()){
				//Type => if it is Type then we can expand this argument
				if((*i)->type.type()->isType() && !(*i)->isFlagSet(Argument::HIDDEN_TYPE) && !isFlagSet(CONSTRAINT_FUNCTION)){
					(*i)->setFlag(Argument::IS_EXPENDABLE);
					setFlag(HAS_EXPENDABLE_ARGUMENTS);
				}
			}
			else {
				//pattern
				if(!(*i)->isFlagSet(Argument::HIDDEN_TYPE)) setFlag(HAS_PATTERN_ARGUMENTS);
			}
		}
	}

	//If this is a generic or expendable function don't resolve body and return type!
	if( isFlagSet(HAS_EXPENDABLE_ARGUMENTS) || isFlagSet(HAS_PATTERN_ARGUMENTS) || isFlagSet(FIELD_ACCESS_FUNCTION)){
		if(resolver->currentTrait){
			if(_returnType.isPattern() && !_returnType.pattern) _returnType.specify(intrinsics::types::Void);
			else if(!_returnType.resolve(resolver)) return this;
		}
		setFlag(Node::RESOLVED);
		debug("Function %s is partially resolved!",label());
		return this;
	}

	//resolve return type. (Don't quit yet because the body may infer it!)
	
	//NB: special case for intrinsic def foo(x Pointer(T:_)) T
	if(isIntrinsic() && arguments.size() && arguments[0]->type.isPattern() && _returnType.kind == TypePatternUnresolvedExpression::UNRESOLVED){

		if(auto sym = _returnType.unresolvedExpression->asUnresolvedSymbol()){
			if(allArgMatcher.lookupDefinition(sym->symbol)){
				makeIntrinsicReturningPattern();
			}
		}
		else if(auto ptr = _returnType.unresolvedExpression->asPointerOperation()){
			if(auto sym = ptr->expression->asUnresolvedSymbol()){
				if(allArgMatcher.lookupDefinition(sym->symbol)){
					makeIntrinsicReturningPattern();
				}
			}
		}
		else if(auto call = _returnType.unresolvedExpression->asCallExpression()){
			if(auto sym = call->arg->asUnresolvedSymbol()){
				if(allArgMatcher.lookupDefinition(sym->symbol)){
					makeIntrinsicReturningPattern();
				}
			}
		}
	}

	if(!this->isIntrinsicReturningPattern() && !_returnType.isResolved() && !_returnType.isPattern()){
		auto oldScope = resolver->currentScope();
		resolver->currentScope(body.scope);
		_returnType.resolve(resolver);
		resolver->currentScope(oldScope);
	}
	//resolve body.
	if(!body.isResolved()){
		auto oldParent = resolver->currentParentNode();
		resolver->currentParentNode(this);
		resolver->resolve(&body);
		resolver->currentParentNode(oldParent);
		if(!body.isResolved()) return this;
	}

	//Body has no return expression => return void
	if(!isFlagSet(CONTAINS_RETURN) && _returnType.isPattern() && _returnType.pattern == nullptr) _returnType.specify(intrinsics::types::Void);
	
	if(!this->isIntrinsicReturningPattern() && !_returnType.isResolved()){
		if(_returnType.isPattern() && isIntrinsic()){
			debug("Pattern return intrinsic!");
		}
		else return this;
	}

	//Everything was resolved!
	setFlag(Node::RESOLVED);
	//debug("Function %s is fully resolved!\n E : %s G : %s Ret : %s Body: %s",id,isFlagSet(HAS_EXPENDABLE_ARGUMENTS),isFlagSet(HAS_PATTERN_ARGUMENTS),_returnType.type(),&body);
	if(isIntrinsic()){
		debug("INTRINSIC MAP %s",label());
		//map to implementation!
		getIntrinsicFunctionBinder(this);
	}
	else analyze(&body,this);
	return this;
}

Node* PrefixMacro::resolve(Resolver* resolver){
	if(!function->isResolved()) function->resolve(resolver);
	if(function->isResolved()){
		setFlag(RESOLVED);
	}
	return this;
}

Node* InfixMacro::resolve(Resolver* resolver){
	if(!function->isResolved()) function->resolve(resolver);
	if(function->isResolved()){
		stickinessExpression = stickinessExpression->resolve(resolver);
		if(stickinessExpression->isResolved()){
			if(auto intLiteral = stickinessExpression->asIntegerLiteral()){
				this->stickiness = int(intLiteral->integer.u64);
				setFlag(RESOLVED);
			}
			else error(stickinessExpression,"The precedence for the infix macro '%s' is expected to be an integer literal, and not %s!",label(),stickinessExpression);
		}
	}
	return this;
}

/**
* Misc
*/
Node* Resolver::multipassResolve(Node* node){

	size_t prevUnresolvedExpressions;
	unresolvedExpressions = 0xDEADBEEF;
	do{
		prevUnresolvedExpressions = unresolvedExpressions;
		unresolvedExpressions = 0;
		node = resolve(node);
	}
	while(prevUnresolvedExpressions != unresolvedExpressions && unresolvedExpressions != 0);
	return node;
}

//Multi-pass module resolver
void  Resolver::resolveModule(BlockExpression* module){
	_currentParent = nullptr;

	size_t prevUnresolvedExpressions;
	unresolvedExpressions = 0xDEADBEEF;
	int pass = 1;
	do{
		prevUnresolvedExpressions = unresolvedExpressions;
		unresolvedExpressions = 0;
		resolve(module);
		debug("After extra pass %d(%d,%d) the module is %s",pass,prevUnresolvedExpressions,unresolvedExpressions,module);
		pass++;
	}
	while(prevUnresolvedExpressions != unresolvedExpressions && unresolvedExpressions != 0);
	if(unresolvedExpressions > 0){
		compiler::headError(module->location(),format("Can't resolve %s expressions and definitions:",unresolvedExpressions));
		//Do an extra pass gathering unresolved definitions
		reportUnevaluated = true;
		auto reportLevel = compiler::reportLevel;
		compiler::reportLevel = compiler::Silent;
		resolve(module);
		compiler::reportLevel = reportLevel;
	}
	analyze(module,nullptr);
}

Node* Resolver::resolveMacroAtParseStage(Node* macro){
	assert(_compilationUnit->parser);
	currentScope(_compilationUnit->parser->currentScope());
	currentParentNode(nullptr);
	return multipassResolve(macro);
}

Node* Resolver::executeAndMixinMacro(Function* function,Node* arg){
	CTFEinvocation i(compilationUnit(),function);
	if(i.invoke(arg)) return mixinMacro(&i,currentScope());
	error(arg,"Failed to interpret a macro '%s' at compile time!",function->label());
	return ErrorExpression::getInstance();
}

/**
* Mixining the expression inside the [> <] obtained from macro invocation.
* Scenarios: mixin( [> 1 <] ) => 
				1
             mixin( [> var x = 2 ; 2 } <] ) => 
				{ var x = 2 ; 2 } using parent scope, returning 2
				The mixined block will use the parent scope to define x, and will return the result of the last expression - i.e. 2
*/
Node* mixinMacro(CTFEinvocation* invocation,Scope* scope){
	DuplicationModifiers mods(scope);
	mods.expandedMacroOptimization = invocation;
	Node* resultingExpression;
	auto  noderef = invocation->result()->asNodeReference();
	auto  inside  = noderef->node();
	if(auto block = inside->asBlockExpression()){
		auto size = block->size();
		if(size == 0) resultingExpression = new UnitExpression;
		else if(size == 1) resultingExpression = (*block->begin())->duplicate(&mods);
		else resultingExpression = block->duplicateMixin(&mods);
	} else resultingExpression = inside->duplicate(&mods);
	return resultingExpression;
}

// Function overload resolving based on a function's type
// TODO
Function* Resolver::resolveOverload(Scope* scope,SymbolID function,Type* functionType){
	assert(functionType->isFunctionPointer());
	return nullptr;
}

// TODO Better error messages!
static Function* errorOnMultipleMatches(Function** functions,size_t count,Node* arg){
	//TODO
	compiler::onError(arg,format("Multiple function overloads found when resolving the call %s(%s):",functions[0]->label(),arg));
	Function** end = functions + count;
	for(auto i = functions;i!=end;i++){
		compiler::onError((*i)->location(),format("\tFunction %s",(*i)->label()));
	}
	return nullptr;
}

// Function overload resolving based on a given argument
// TODO import qualified foo; var x foo.Foo ; foo.method() <-- FIX use dot syntax
Function* Resolver::resolveOverload(Scope* scope,SymbolID function,Node* arg,bool dotSyntax){
	
	std::vector<Function*> results;
	//step 1 - check current scope for matching function
	if(auto hasDef = scope->containsPrefix(function)){
		if(auto os = hasDef->asOverloadset()){
			findMatchingFunctions(os->functions,results,arg);
			if(results.size() == 1) return results[0];
			else if(results.size()>1) return errorOnMultipleMatches(results.begin()._Ptr,results.size(),arg);
		}
	}
	//step 2 - check imported scopes for matching function
	if(scope->imports.size()){
		std::vector<Function*> overloads;
		for(auto i = scope->imports.begin();i!=scope->imports.end();++i){ 
			if(auto hasDef = (*i)->containsPrefix(function)){
				if(auto os = hasDef->asOverloadset()){
					if(overloads.size()>0 && os->isFlagSet(Overloadset::TYPE_GENERATOR_SET)){
						error(arg,"Function '%s' overloading abmiguity - it is either a type generation or a normal function!",function);
						return nullptr;//TODO better error message
					}
					else overloads.insert(overloads.end(),os->functions.begin(),os->functions.end());
				}
			}
		}
		findMatchingFunctions(overloads,results,arg,true);
		if(results.size() == 1) return results[0];
		else if(results.size()>1) return errorOnMultipleMatches(results.begin()._Ptr,results.size(),arg);
	}
	//step 3 - check parent scope
	if(scope->parent){
		if(auto result = resolveOverload(scope->parent,function,arg,dotSyntax)) return result;
	}
	if(scope->parent2){
		return resolveOverload(scope->parent2,function,arg,dotSyntax);
	}
	return nullptr;
}

/**
*  Generic function specialization with parameter type deduction and/or value expansion
*  The original function contains the set of the generated specializations.
*  Before we generate a new function, we check if original already created a matching specialization.
*  If the original didn't generate the given specialization yet, we generate one and add it to the original's set.
*
*  The generated functions will have access to the declaration scope, and the module scope of the user expansion.
*  TODO: identical specialization reduction!
*/
Function* Function::specializationExists(Type** specializedParameters,Node** passedExpressions,Scope* usageScope){
	auto numberOfParameters= arguments.size();
	size_t j = 0;
	for(auto i = generatedFunctions.begin();i!=generatedFunctions.end();i++){
		auto alreadyGenerated = *i;
		bool match = true;
		size_t expandedParameterOffset = 0;
		if(usageScope && alreadyGenerated->owner()->parent != usageScope) continue;
		for(j = 0; j<numberOfParameters; j++){
			if(arguments[j]->expandAtCompileTime()){
				//TODO Node::isSame
				if(!alreadyGenerated->expandedArguments[expandedParameterOffset]->asTypeReference()->type->isSame(passedExpressions[j]->asTypeReference()->type)){
					match = false;
					break;
				}
				expandedParameterOffset++;
			}
			else if(specializedParameters && !alreadyGenerated->arguments[j - expandedParameterOffset]->type.type()->isSame(specializedParameters[j])){
				match = false;
				break;
			}
		}
		if(match) return alreadyGenerated;
	}
	return nullptr;
}

Argument* Argument::specializedDuplicate(Function* dest,DuplicationModifiers* mods,Type* specializedType,Node* expandedValue){
	//value expansion
	if(this->expandAtCompileTime()){
		mods->expandArgument(this,expandedValue);
		return nullptr;
	}
	Argument* dup = new Argument(label(),location(),dest);
	
	//type specialization
	if(specializedType){
		assert(type.isPattern());
		dup->type.specify(specializedType);
	}
	else dup->type = type.duplicate(mods);
	//dup->ctfeRegisterID = ctfeRegisterID; //NB: no need, since original argument won't be analyzed yet. 
	dup->_defaultValue  = _defaultValue ? _defaultValue->duplicate(mods) : nullptr;
	dup->_hiddenType    = _hiddenType;
	mods->duplicateDefinition(this,dup);
	copyProperties(dup);
	return dup;
}

Function* Function::specializedDuplicate(DuplicationModifiers* mods,Type** specializedParameters,Node** passedExpressions) {	
	auto numberOfParameters= arguments.size();

	debug("Need to duplicate determined function %s!",label());
	auto func = new Function(label(),location());
	func->body.scope->parent = mods->target;
	mods->target = func->body.scope;
	
	//args
	for(size_t i = 0;i < numberOfParameters;++i){
		auto arg = arguments[i]->specializedDuplicate(func,mods,specializedParameters ? specializedParameters[i] : nullptr,passedExpressions ? passedExpressions[i] : nullptr);
		if(arg) func->addArgument(arg);
		else func->expandedArguments.push_back(passedExpressions[i]);//Give the specialized function the knowledge about what parameters where expanded to create it
	}

	duplicateReturnBody(mods,func);
	func->generatedFunctionParent = this;
	this->generatedFunctions.push_back(func);
	func->flags &= (~RESOLVED);
	func->flags &= (~HAS_PATTERN_ARGUMENTS);
	func->flags &= (~HAS_EXPENDABLE_ARGUMENTS);
	return func;
}

/**
NB: given the situation:
	type Bar(T) { var x T; def foo(self) = self.x }
	var bar Bar(int32)
	bar.foo
where the expansion of foo(x *Bar(int32)) can't be resolved straight away, resolve the function from the calling expression
*/
bool Resolver::resolveSpecialization(Function* function){
	auto original = function->generatedFunctionParent;

	auto oldScope = currentScope();
	auto oldParent = currentParentNode();
	currentScope(function->owner());
	currentParentNode(original->parentNode);
	resolve(function);
	currentScope(oldScope);
	currentParentNode(oldParent);
	return function->isResolved();
}


Scope* getSpecializationScope(Function* original,Resolver* resolver){
	if(original->owner()->moduleScope() != resolver->compilationUnit()->moduleBody->scope) return resolver->compilationUnit()->moduleBody->scope;
	else return original->owner();
}

Function* Resolver::specializeFunction(TypePatternUnresolvedExpression::PatternMatcher& patternMatcher,Function* original,Type** specializedParameters,Node** passedExpressions){
	size_t numberOfParameters = original->arguments.size();
	assert(original->isFlagSet(Function::HAS_PATTERN_ARGUMENTS) || original->isFlagSet(Function::HAS_EXPENDABLE_ARGUMENTS));	
	
	/**
	* Type templates - they need to be placed back to the defining scope.
	*/
	if(original->isTypeTemplate()){
		if(auto exists = original->specializationExists(specializedParameters,passedExpressions,nullptr)) return exists;

		DuplicationModifiers mods(original->owner());
		auto specialization = original->specializedDuplicate(&mods,specializedParameters,passedExpressions);
		//TODO better stuff here
		auto oldScope  = currentScope();
		auto oldParent = currentParentNode();
		currentScope(original->owner());
		currentParentNode(original->parentNode);
		multipassResolve(specialization);
		original->parentNode->asBlockExpression()->addChildPotentiallyDisturbingIteration(specialization);
		specialization->getTemplateTypeDeclaration()->type()->onTemplateSpecialization(this);
		currentScope(oldScope);
		currentParentNode(oldParent);		
		return specialization;
	}
	Scope* usageScope;
	Scope* declarationScope;
	bool definedInSameModule = false;
	if(original->owner()->moduleScope() != this->compilationUnit()->moduleBody->scope){
		usageScope = this->compilationUnit()->moduleBody->scope;
		declarationScope = original->owner();
	}
	else {
		usageScope = original->owner();
		declarationScope = nullptr;
		definedInSameModule = true;
	}

	if(auto exists = original->specializationExists(specializedParameters,passedExpressions,usageScope)) return exists;
	//Create a new block for specialization which will import the required scopes
	auto specializationWrapper = new BlockExpression();
	//specializationWrapper->scope->import(original->owner()); //import the scope in which the original function was defined.
	//if(usageScope) 
	specializationWrapper->scope->setParent(usageScope);//import the usage scope
	specializationWrapper->scope->parent2 = declarationScope;
	//duplicate the original
	DuplicationModifiers mods(specializationWrapper->scope);
	auto specialization = original->specializedDuplicate(&mods,specializedParameters,passedExpressions);
	assert(specialization->owner() == specializationWrapper->scope);
	specializationWrapper->addChild(specialization);
	//bring in the T in def foo(x T:_) into the function
	if(original->isFlagSet(Function::HAS_PATTERN_ARGUMENTS)){
		patternMatcher.container = specialization->body.scope;
		patternMatcher.defineIntroducedDefinitions();
	}
	//Resolve.. TODO error handling

	auto oldScope = currentScope();
	auto oldParent = currentParentNode();
	currentParentNode(original->parentNode);
	currentScope(specializationWrapper->scope);
	multipassResolve(specialization);
	if(!specialization->isResolved() && !definedInSameModule){
		error(original,"Can't resolve the specialization for the function '%s'",original->label());
	}
	currentScope(oldScope);
	currentParentNode(oldParent);
	
	assert(!(specialization->isFlagSet(Function::HAS_EXPENDABLE_ARGUMENTS) || specialization->isFlagSet(Function::HAS_PATTERN_ARGUMENTS)));
	//
	compiler::addGeneratedExpression(specializationWrapper);
	return specialization;
}

/**
* Adjusting the argument
TODO: rearranging args execution priority,
      patterned args assingFrom
*/
Node* Resolver::constructFittingArgument(Function** function,Node *arg,bool dependentChecker,int* weight){
	Function* func = *function;
	std::vector<Node*> result;
	result.resize(func->arguments.size(),nullptr);
	bool determinedFunction = false;
	std::vector<Type* > determinedArguments;//Boolean to indicate whether the argument was expanded at compile time and is no longer needed

	TypePatternUnresolvedExpression::PatternMatcher matcher(func->body.scope,this);//need to match the second time round to inject introduced definitions..

	//..
	size_t currentArg = 0;
	size_t currentExpr = 0;
	size_t lastNonLabeledExpr = 0;
	size_t resolvedArgs = 0;
	auto argsCount = func->arguments.size();
	Node* expr = arg;//nonTuple
	Node** exprBegin;
	size_t expressionCount;
	if(auto tuple = arg->asTupleExpression()){
		exprBegin = tuple->children.begin()._Ptr;
		expressionCount = tuple->children.size();
	}else if(arg->asUnitExpression()){
		expressionCount = 0;
	}
	else{
		exprBegin = &arg;
		expressionCount = 1;
	}

	if(argsCount < expressionCount){
		if(argsCount > 0 && func->arguments[argsCount-1]->type.isPattern()){
			argsCount--;
			auto tuple = new TupleExpression();
			for(auto i = argsCount;i<expressionCount;i++)
				tuple->children.push_back(exprBegin[i]);
			result[argsCount] = resolve(tuple);
			assert(result[argsCount]->isResolved());
			expressionCount = argsCount;

			determinedFunction = true;
			determinedArguments.resize(argsCount+1,nullptr);
			determinedArguments[argsCount] = result[argsCount]->returnType();
		}
		else assert(false);
	}

	while(currentExpr<expressionCount){
		auto label = exprBegin[currentExpr]->label();
		if(!label.isNull()){
			//Labeled
			for(currentArg =lastNonLabeledExpr ; currentArg < argsCount;currentArg++){
				if(func->arguments[currentArg]->label() == label)
					break;
			}
		}
		else{
			//NonLabeled
			lastNonLabeledExpr = currentExpr;
		}
		
		//Match given types to the parameter's type
		if( func->arguments[currentArg]->isDependent() ){
			result[currentArg] = exprBegin[currentExpr];
		}
		else if( func->arguments[currentArg]->type.isPattern() ){
			
			if(auto pattern = func->arguments[currentArg]->type.pattern){
				auto topMatchedType = matcher.matchWithSubtyping(exprBegin[currentExpr]->returnType(),pattern,getSpecializationScope(func,this));
				result[currentArg] = resolve(topMatchedType->assignableFrom(exprBegin[currentExpr],exprBegin[currentExpr]->returnType()));
			}
			else {
				result[currentArg] = resolve(exprBegin[currentExpr]);
			}
			if(!determinedFunction){
				determinedFunction = true;
				determinedArguments.resize(argsCount,nullptr);
			}
			
			determinedArguments[currentArg] = result[currentArg]->returnType();
		}
		else {
			result[currentArg] = func->arguments[currentArg]->type.type()->assignableFrom(exprBegin[currentExpr],exprBegin[currentExpr]->returnType());
		}
		currentArg++;resolvedArgs++;currentExpr++;	
	}

	//Default args
	for(currentArg = 0; currentArg < argsCount;currentArg ++){
		DuplicationModifiers mods(func->body.scope);
		if(!result[currentArg]) result[currentArg] = func->arguments[currentArg]->defaultValue()->duplicate(&mods);//NB: Duplication might not be even necessary!
	}

	if(dependentChecker){
		DuplicationModifiers mods(func->body.scope);
		for(size_t i = 0;i< result.size();i++){
			if(!func->arguments[i]->isDependent()){
				mods.redirectors[reinterpret_cast<void*>(static_cast<Variable*>(func->arguments[i]))] =
					std::make_pair(reinterpret_cast<void*>(result[i]),true);
			}
		}

		bool resolved = true;
		for(size_t i = 0;i< result.size();i++){
			if(func->arguments[i]->isDependent()){
				auto dup = func->arguments[i]->reallyDuplicate(func,&mods);
				//TODO resolve in scope of function
				if(dup->resolve(this)){ //TODO how about allowing this to be a constraint? >_>
					//typecheck
					auto ret = result[i]->returnType();
					auto w = dup->type.type()->canAssignFrom(result[i],ret);
					if(w == -1) resolved = false;
					else {
						*weight += w;
					}
				}
				else resolved = false;
					
			}
		}

		if(resolved) debug("Dependent args are resolved!");

		return resolved ? arg : nullptr;

	}

	if(!func->isFlagSet(Function::MACRO_FUNCTION) && !func->isIntrinsic() && !func->isFieldAccessMacro()){//Macro optimization, so that we dont duplicate unnecessary
		//Determine the function?
		if(determinedFunction || func->isFlagSet(Function::HAS_EXPENDABLE_ARGUMENTS)){
			*function = specializeFunction(matcher,func,determinedFunction ? &determinedArguments[0] : nullptr,&result[0]);
		}
	}

	//Wrap all parameters in [> <] for macro functions
	if(func->isFlagSet(Function::MACRO_FUNCTION)){
		for(auto i = result.begin();i!=result.end();i++){
			*i = (*i)->asNodeReference() ? *i : new NodeReference(*i);
		}
	}

	//Construct a proper argument
	if(result.size() == 0){
		if(auto u = arg->asUnitExpression()) return arg; //arg is ()
		//delete arg;
		else return new UnitExpression();
	}
	else if(result.size() == 1){
		if(auto u = arg->asUnitExpression()) delete arg;
		return result[0];
	}else{
		TupleExpression* tuple;
		if(!(tuple = arg->asTupleExpression())){
			if(auto u = arg->asUnitExpression()) delete arg;
			tuple = new TupleExpression();
		}

		tuple->children = result;	
		return tuple->resolve(this);
	}

}

/**
*Overload resolving:
*Scenario 1 - no arg:
*	f() matches f(), f(x T|Nothing)???, f(x = true,y = false)
*Scenario 2 - expression|tuple without labeled expressions:
*	f(1,2) matches f(a,b), f(a,b,x = true)
*Scenario 3 - expression|tuple with all expressions labeled:
*	f(a:1,b:2) matches f(a,b), f(a,b,c), f(a,b,x = true)
*Scenario 4 - tuple with expressions being non labeled and labeled:
*	f(1,a:5) matches f(b,a), f(b,a,x = true)
*	f(a:5,1) matches f(a,b), f(b,a,x = true)
*Also(should it?)
*	f(1,2) matches f(x)
*	f(1,2,3) matches f(x,y)
*/
bool match(Resolver* evaluator,Function* func,Node* arg,int& weight){
	//Weights
	enum {
		WILDCARD = 1,
		CONSTRAINED_WILDCARD, //Others in node.cpp via TypeExpression::canAssign
		//CONSTRAINED_WILDCARD_VALUE
	};
	weight = 0;

	//dependent args
	bool hasDependentArg = false;
	TypePatternUnresolvedExpression::PatternMatcher matcher(func->body.scope,evaluator);

	//
	std::vector<bool> checked;
	checked.resize(func->arguments.size());
	size_t currentArg = 0;
	size_t currentExpr = 0;
	size_t lastNonLabeledExpr = 0;
	size_t resolvedArgs = 0;
	auto argsCount = func->arguments.size();
	Node* expr = arg;//nonTuple
	Node** exprBegin;
	size_t expressionCount;
	if(auto tuple = arg->asTupleExpression()){
		exprBegin = tuple->children.begin()._Ptr;
		expressionCount = tuple->children.size();
	}else if(arg->asUnitExpression()){
		expressionCount = 0;
	}
	else{
		exprBegin = &arg;
		expressionCount = 1;
	}

	//Accounts for the case of - def f(x _) = 1 ; f(1,2,3) #ok
	if(argsCount < expressionCount){
		if(argsCount > 0){
			bool matches = false;
			if(func->arguments[argsCount-1]->isVararg() && func->arguments[argsCount-1]->type.isPattern()){
				if(auto pattern = func->arguments[argsCount-1]->type.pattern){
					TypePatternUnresolvedExpression::PatternMatcher matcher(func->body.scope,evaluator);
					//construct a record from the tailed parameters
					std::vector<AnonymousAggregate::Field> fields;
					for(auto i = argsCount - 1;i < expressionCount;i++){
						AnonymousAggregate::Field field = { exprBegin[i]->label(),exprBegin[i]->returnType() };
						fields.push_back(field);
					}
					auto record = AnonymousAggregate::create(&fields[0],fields.size());
					if(!matcher.match(record,pattern)) return false;
				}
			}
			else if(!func->arguments[argsCount-1]->type.type()->isSame(intrinsics::types::NodePointer)) return false;
			argsCount--;
			checked[argsCount] = true;
			expressionCount = argsCount;
			weight += WILDCARD;
		}
		else return false;
	}

	while(currentExpr<expressionCount){
		//Find the matching parameter
		auto label = exprBegin[currentExpr]->label();
		if(!label.isNull()){
			//Labeled
			//TODO same label multiple times error!
			bool foundMatch = false;
			for(currentArg =lastNonLabeledExpr ; currentArg < argsCount;currentArg++){
				if(func->arguments[currentArg]->label() == label){
					foundMatch = true;
					break;
				}
			}
			if(!foundMatch) return false;	
		}
		else{
			//NonLabeled
			lastNonLabeledExpr = currentExpr;
			if(!(currentArg < argsCount)) return false;//f(x:5,6) where x is the last arg
		}
		//Typecheck
		int w;
		if( func->arguments[currentArg]->isDependent() ){
			hasDependentArg = true;
		}
		else if( func->arguments[currentArg]->type.isPattern() ){
			if(auto pattern = func->arguments[currentArg]->type.pattern){
				if(!matcher.matchWithSubtyping(exprBegin[currentExpr]->returnType(),pattern,getSpecializationScope(func,evaluator))) return false;
				weight += WILDCARD + 1;
			}
			else weight += WILDCARD;
		}
		else {
			if((w = func->arguments[currentArg]->type.type()->canAssignFrom(exprBegin[currentExpr],exprBegin[currentExpr]->returnType() ))!= -1 ){
				weight += w;
			}
			else return false;
		}
		checked[currentArg] = true;
		currentArg++;resolvedArgs++;currentExpr++;	
	}

	//Check for default parameters
	auto result = true; //() matches ()
	if(resolvedArgs != argsCount){
		for(currentArg = 0; currentArg < argsCount;currentArg ++){
			if(!checked[currentArg] && !func->arguments[currentArg]->defaultValue()) result = false; //() doesn't match (x,y)
		}
	}

	//Try  to match dependent args by solving independent args and then resolving dependent ones
	//TODO remove
	if(result && hasDependentArg){
		debug("Trying to match dependent args");
		
		return evaluator->constructFittingArgument(&func,arg,true,&weight) != nullptr;
	}
	return result; 
}

void Resolver::findMatchingFunctions(std::vector<Function*>& overloads,std::vector<Function*>& results,Node* argument,bool enforcePublic){
	int weight = 0;
	int maxweight = -1;
	for(auto i=overloads.begin();i!=overloads.end();++i){
		if(enforcePublic && !(*i)->isPublic()) continue;
		if(!(*i)->isResolved()) continue; //TODO what if we need this
		if(match(this,(*i),argument,weight)){
			if(weight == maxweight){
				results.push_back(*i);
			}else if(weight >= maxweight){
				results.clear();
				results.push_back(*i);
				maxweight = weight;
			}
		}
	}
}
