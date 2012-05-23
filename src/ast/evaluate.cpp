#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "evaluate.h"
#include "../intrinsics/ast.h"
#include "../intrinsics/types.h"

//expression evaluation - resolving overloads, inferring types, invoking ctfe


Node* evaluateResolvedFunctionCall(Evaluator* evaluator,CallExpression* node){
	auto function = node->object->asFunctionReference()->function;

	if(function->mixinOnCall()) return evaluator->mixinFunction(node);
	//Try to expand the function
	else if(function->intrinsicEvaluator) 
		return function->intrinsicEvaluator(node,evaluator);
	return node;
}

//Typecheks an expression
Node* typecheck(Location& loc,Node* expression,TypeExpression* expectedType){
	if(auto assigns = expectedType->assignableFrom(expression,expression->_returnType())){
		return assigns;
	}
	else {/* TODO expression->location? */
		error(loc,"Expected an expression of type %s instead of %s of type %s",expectedType,expression,expression->_returnType());
		return expression;
	}
}



struct AstExpander: NodeVisitor {
	Evaluator* evaluator;
	AstExpander(Evaluator* ev) : evaluator(ev) {}


	Node* visit(ExpressionReference* node){
		if(evaluator->evaluateExpressionReferences){
			//delete node
			return node->expression->accept(this);
		}
		return node;
	}

	//on a.foo(...)
	static Node* transformCallOnAccess(CallExpression* node,AccessExpression* acessingObject){
		//a.foo()
		if(node->arg->asUnitExpression()){
			delete node->arg;
			node->arg  = acessingObject->object;
		}
		//a.foo(bar)
		else{
			if(auto isArgRecord = node->arg->asTupleExpression())
				isArgRecord->children.insert(isArgRecord->children.begin(),acessingObject->object);
			else
				node->arg = new TupleExpression(acessingObject->object,node->arg);
		}
		auto newCalleeObject = new UnresolvedSymbol(node->location,acessingObject->symbol);
		node->object = newCalleeObject;
		acessingObject->object = nullptr;
		delete acessingObject;
		return node;
	}
	//TODO Type call -> constructor.
	Node* evalTypeCall(CallExpression* node,TypeExpression* type){
		/*if(type == intrinsics::ast::Expression){
			debug("Expression of");
			auto r = ExpressionReference::create(node->arg);
			//delte node
			return r;
		}*/
		return node;
	}

	Node* visit(CallExpression* node){
		//evaluate argument
		node->arg = node->arg->accept(this);
		
		if(auto callingType = node->object->asTypeExpression()){
			return evalTypeCall(node,callingType);
		}

		if(!node->arg->isResolved()) return node;
		if(auto callingOverloadSet = node->object->asUnresolvedSymbol()){
			auto scope = (callingOverloadSet->explicitLookupScope ? callingOverloadSet->explicitLookupScope : evaluator->currentScope());
			auto func =  scope->resolveFunction(callingOverloadSet->symbol,node->arg);
			if(func){
				node->arg = evaluator->constructFittingArgument(&func,node->arg)->accept(this);
				node->object = new FunctionReference(func);
				node->_resolved = true;
				return evaluateResolvedFunctionCall(evaluator,node);
			}else{
				evaluator->markUnresolved(node);
			}
		}
		else if(auto callingFunc = node->object->asFunctionReference())
			return node;	//TODO eval?
		else if(auto callingAccess = node->object->asAccessExpression()){
			return transformCallOnAccess(node,callingAccess)->accept(this);
		}
		else
			error(node->object->location,"Can't perform a call on %s!",node->object);
		return node;
	}

	Node* visit(VariableReference* node){
		if(node->variable->expandMe && !evaluator->isRHS)
			return node->copyProperties(node->variable->value->duplicate());
		return node;
	}	

	Node* visit(PointerOperation* node){
		node->expression = node->expression->accept(this);
		if(node->expression->isResolved()){
			// *int32 => Pointer(int32)
			if(auto typeExpr = node->expression->asTypeExpression()){
				node->expression = nullptr;
				delete node;
				return new TypeExpression((PointerType*)nullptr,typeExpr);
			}
		}
		return node;
	}


	Node* fieldAccessFromAccess(AccessExpression* node){
		auto returns = node->object->_returnType();
		if(returns->type == TypeExpression::POINTER) returns = returns->argument;
		assert(returns->type == TypeExpression::RECORD);
		auto field = returns->record->lookupField(node->symbol);
		if(field != -1){
			auto expr = new FieldAccessExpression(node->object,field);
			delete node;
			return expr;
		}
		else return nullptr;
	}

	//TODO def x = 1;x = 1 => 1=1 on not first use?
	Node* assign(AssignmentExpression* assignment,Node* object,Node* value,bool* error){
		if(auto t1 = object->asTupleExpression()){
			if(auto t2 = value->asTupleExpression()){
				if(t1->children.size() == t2->children.size()){
					for(size_t i=0;i<t1->children.size();i++){
						auto newValue = assign(assignment,t1->children[i],t2->children[i],error);
						if(newValue) t2->children[i] = newValue;
						else assignment->_resolved = false;
					}
					
					return t2;
				}
				else{
					error(assignment->location,"Can't assign between tuples of different length");
					*error = true;
					return nullptr;
				}
			}
			else{
				error(assignment->location,"Can't assign a non-tuple to a tuple");
				*error = true;
				return nullptr;
			}
		}else{
			auto valuesType = value->_returnType();
			//Assigning values to variables
			if(auto var = object->asVariableReference()){
				if(var->variable->type.isInferred()){
					var->variable->type.infer(valuesType);
					debug("Inferred type %s for variable %s",valuesType,var->variable->id);
					if(var->variable->isMutable) return value;
				}
				if(var->variable->type.isResolved()){
					if(auto canBeAssigned = var->variable->type.type()->assignableFrom(value,valuesType)){
						if(!var->variable->isMutable) {
							//If variable has a constant type, assign only at place of declaration
							if(assignment->isInitializingAssignment){
								if(!var->variable->value)
									var->variable->setImmutableValue(value);
							}
							else{
								error(value->location,"Can't assign %s to %s - immutable variables can only be assigned at declaration!",value,object);
								*error = true;
								return nullptr;
							}
						}
						return canBeAssigned;
					}
					else {
						error(value->location,"Can't assign %s to %s - the types don't match!",value,object);
						*error = true;
						return nullptr;
					}
				}
				else return nullptr; //Trying to assign to a variable with unresolved type.. that's a no no!
			}
			else if(auto access = object->asAccessExpression()){
				//TODO
			}
			else{
				error(object->location,"Can't perform an assignment to %s - only variables and fields are assignable!",object);
				*error = true;
			}
			return nullptr;
		}		
	}
	Node* visit(AssignmentExpression* node){
		if(node->_resolved) return node;//Don't evaluate it again

		node->value = node->value->accept(this);
		if(!node->value->isResolved()) return node;//Don't bother until the value is fully resolved
		bool error = false;

		node->_resolved = true;
		auto newValue = assign(node,node->object,node->value,&error);
		if(newValue){
			node->value = newValue;
		}
		else node->_resolved = false;
		if(node->_resolved){
			auto oldRHS = evaluator->isRHS;
			evaluator->isRHS = true;
			node->object = node->object->accept(this); // Need to resolve object's tuple's type when some variable is inferred
			evaluator->isRHS = oldRHS;
		}

		if(error){
			//TODO delete tuple's children
			delete node;
			return ErrorExpression::getInstance();
		}
		else return node;
		
		/*auto valueReturns = node->value->_returnType();
		if(node->value->returnType() != compiler::Unresolved){
			//Assigning to variables
			if(auto var = node->object->asVariableReference()){
				if(var->variable->_type == nullptr){ //inferred
					var->variable->_type = node->value->_returnType();
				}
				else if(var->variable->_type->resolved()){
					auto varType = var->variable->_type;
					//if constant, assign only at place of declaration
					if(varType->type == TypeExpression::CONSTANT){
						if(var->isDefinedHere) varType = varType->next;//remove const
						else {
							error(node->location,"Can't assign to %s - constant variables can only be assigned at declaration!",node->value,node->object);
							return ErrorExpression::getInstance();
						}
					}
					if(auto canBeAssigned = varType->assignableFrom(node->value,valueReturns)) node->value = canBeAssigned;
					else {
						error(node->location,"Can't assign to %s - the types don't match!",node->value,node->object);
						delete node;
						return ErrorExpression::getInstance();
					}
				}
			}
			//Assigning to fields | properties
			else if(auto access = node->object->asAccessExpression()){
				//TODO a.foo = .. when foo is field
				//a.foo = 2 -> foo(a,2)
				auto args = new TupleExpression(access->object,node->value);
				return CallExpression::create(OverloadSetExpression::create(access->symbol,access->scope),args)->accept(this);
			}
			else error(node->location,"Can't assign to %s!",node->object);
		}
		return node;*/
	}


	Node* visit(TupleExpression* node){
		if(node->isResolved()) return node;//Don't evaluate it again
		if(node->children.size() == 1){
			auto child = node->children[0];
			delete node;
			return child;
		}else if(node->children.size() == 0){
			delete node;
			return new UnitExpression;
		}

		std::vector<Record::Field> fields;
	
		bool resolved = true;
		for(size_t i =0;i<node->children.size();i++){
			node->children[i] = node->children[i]->accept(this);

			if(node->children[i]->isResolved()){
				//Check that the child doesn't return void
				auto returns = node->children[i]->_returnType();
				if(returns == intrinsics::types::Void){
					error(node->children[i]->location,"A tuple can't contain an expression returning void!");
					resolved = false;
				}
				else fields.push_back(Record::Field(SymbolID(),returns->duplicate()->asTypeExpression()));
			}
			else resolved = false;
		}

		if(resolved){
			//int32,int32 :: Type,Type -> anon-record(int32,int32) :: Type
			if(evaluator->expectedTypeForEvaluatedExpression == intrinsics::types::Type){
				bool allTypes = true;
				for(auto i=fields.begin();i!=fields.end();i++){
					if((*i).type.type() != intrinsics::types::Type) allTypes = false;
				}
				if(allTypes){
					//int32,int32
					for(size_t i =0;i<node->children.size();i++){
						fields[i].type = node->children[i]->asTypeExpression();
					}
					delete node;
					return new TypeExpression(Record::findAnonymousRecord(fields));
				}
			}
			node->type = new TypeExpression(Record::findAnonymousRecord(fields));
		}
		return node;
	}
	//TODO simplify when 1 child and empty scope.. help for mixing and inling as well!
	Node* visit(BlockExpression* node){
		assert(!node->isResolved());
		auto scp = evaluator->currentScope();
		node->_resolved = true;
		if(!node->scope->isResolved()){
			if(!node->scope->resolve(evaluator)) node->_resolved = false;//Resolve unresolved definitions
		}
		evaluator->currentScope(node->scope);
		auto e = evaluator->mixinedExpression;
		evaluator->mixinedExpression = nullptr;
		for(size_t i =0;i<node->children.size();i++){
			if(!node->children[i]->isResolved()){	
				node->children[i] = node->children[i]->accept(this);
				if(!node->children[i]->isResolved()) node->_resolved = false;
				if(evaluator->mixinedExpression){
					debug("Mixin!");
					node->children.insert(node->children.begin()+i,evaluator->mixinedExpression);
					i++;
					evaluator->mixinedExpression = nullptr;
				}
			}	
		}
		evaluator->mixinedExpression = e;
		evaluator->currentScope(scp);
		return node;
	}
	Node* visit(ReturnExpression* node){
		auto func = evaluator->currentScope()->functionOwner();
		if(func){
			func->_hasReturnInside = true;
			node->value = node->value->accept(this);
			if(node->value->isResolved()){
				node->_resolved = true;
				if(func->_returnType.isInferred()){
					//TODO Don't allow to return local types
					auto valRet = node->value->_returnType();
					if(!valRet->hasLocalSemantics()){
						debug("Inferring return type %s for function %s",valRet,func->id);
						func->_returnType.infer(valRet);
					}
					else
						error(node->location,"Can't return %s because of local semantics!",node->value);
				}
				else if(func->_returnType.isResolved()){
					node->value = typecheck(node->value->location,node->value,func->_returnType.type());
				}
			}
		}else error(node->location,"Return statement is allowed only inside function's body!");
		return node;
	}

	Node* evaluateIntegerMatch(IntegerLiteral* value,MatchExpression* node){
		
		//TODO
		for(auto i =node->cases.begin();i!=node->cases.end();i++){
			auto intLiteral = (*i).pattern->asIntegerLiteral();
			assert(intLiteral);
			if(value->integer == intLiteral->integer) return (*i).consequence;
		}
		return node;
	}

	Node* visit(MatchExpression* node){
		node->object = node->object->accept(this);
		if(!node->object->isResolved()) return node;
		//Resolve cases
		bool allCasesResolved = true;
		for(auto i =node->cases.begin();i!=node->cases.end();i++){
			(*i).pattern = (*i).pattern->accept(this);
			if(!(*i).pattern->isResolved()) allCasesResolved = false;
			if((*i).consequence){
				(*i).consequence = (*i).consequence->accept(this);
				if(!(*i).consequence->isResolved()) allCasesResolved = false;
			}
		}

		//Typecheck
		
		//Evaluate constant match
		if(allCasesResolved){
			if(auto intLiteral = node->object->asIntegerLiteral()) return evaluateIntegerMatch(intLiteral,node);
		}

		return node;
	}

	Node* visit(WhileExpression* node){
		node->condition = node->condition->accept(this);
		node->body = node->body->accept(this);

		if(node->condition->isResolved()) node->condition = typecheck(node->location,node->condition,intrinsics::types::boolean);
		return node;
	}

	/**
	* Resolving temporary nodes
	*/
	Node* visit(ExpressionVerifier* node){
		node->expression = node->expression->accept(this);
		if(node->expression->isResolved()){
			auto result = typecheck(node->location,node->expression,node->expectedType);
			node->expression = nullptr;
			delete node;
			return result;
		}
		//NB No need for unresolved marking
		return node;
	}

	Node* visit(UnresolvedSymbol* node){
		//TODO fix
		//{ Foo/*Should be type Foo */; var Foo int32 } type Foo <-- impossibru
		auto def = (node->explicitLookupScope ? node->explicitLookupScope : evaluator->currentScope())->lookupPrefix(node->symbol);
		if(def){
			if(auto ref = def->createReference()){
				delete node;
				return ref;
			};
		}
		evaluator->markUnresolved(node);
		return node;
	}

	Node* visit(AccessExpression* node){
		node->object = node->object->accept(this);
		if(node->passedFirstEval){
			if(node->object->isResolved()){
				if(auto fa = fieldAccessFromAccess(node)) return fa;
				//TODO type field access & expression '.' call notation
				else return (new CallExpression(new UnresolvedSymbol(node->location,node->symbol),node->object))->accept(this);
			}
		}
		else node->passedFirstEval = true;
		evaluator->markUnresolved(node);
		return node;
	}

};

void Evaluator::markUnresolved(Node* node){
	unresolvedExpressions++;
}

void Evaluator::markUnresolved(PrefixDefinition* node){
	unresolvedExpressions++;
	if(reportUnevaluated){
		//TODO more progressive error message
		error(node->location,"Can't resolve definition %s!",node->id);
	}
}

bool InferredUnresolvedTypeExpression::resolve(Evaluator* evaluator){
	assert(kind == Unresolved);
	auto oldSetting = evaluator->expectedTypeForEvaluatedExpression;
	evaluator->expectedTypeForEvaluatedExpression = intrinsics::types::Type;
	unresolvedExpression = evaluator->eval(unresolvedExpression);
	evaluator->expectedTypeForEvaluatedExpression = oldSetting;

	if(auto isTypeExpr = unresolvedExpression->asTypeExpression()){
		if(isTypeExpr && isTypeExpr->isResolved()){
			kind = Type;
			_type = isTypeExpr;
			return true;
		}
	}
	
	return false;
}

Node* Evaluator::eval(Node* node){
	AstExpander expander(this);
	return node->accept(&expander);
}

//TODO ignore unresolved functions which cant be resolved
void Evaluator::evaluateModule(BlockExpression* module){
	
	if(unresolvedExpressions == 0) return;
	unresolvedExpressions = 0;
	size_t prevUnresolvedExpressions;
	int pass = 1;
	do{
		prevUnresolvedExpressions = unresolvedExpressions;
		unresolvedExpressions = 0;
		eval(module);
		debug("After extra pass %d the module is %s",pass,module);
		pass++;
	}
	while(prevUnresolvedExpressions > unresolvedExpressions);
	if(unresolvedExpressions > 0){
		error(module->location,"Can't resolve %s expressions and definitions:",unresolvedExpressions);
		//Do an extra pass gathering unresolved definitions
		reportUnevaluated = true;
		eval(module);
	}
}

/**
*How inlining and mixining should work:

*def f(x Type){ var y x = 0; return y + 1; }
*mixin(f(int32)) =>
*	def r04903
*	var y int32
*	{
*		r04903 = y + 1
*	}
*	r04903
*inline(f(int32)) =>
*	def r04903
*	{
*		var y int32
*		r04903 = y + 1
*	}
*	r04903
*/
Node* Evaluator::mixinFunction(CallExpression* node,bool inlined){
	Node* result;
	assert(node->isResolved());
	auto func = node->object->asFunctionReference()->function;
	if(!func->body.children.size()){
		error(node->location,"Can't mixin a function %s without body!",func->id);
		return ErrorExpression::getInstance();
	}
	debug("<<M");
	DuplicationModifiers mods;
	//Replace arguments
	std::vector<Node*> inlinedArguments;
	inlinedArguments.resize(func->arguments.size());
	auto argsBegin = node->arg->asTupleExpression() ? node->arg->asTupleExpression()->children.begin()._Ptr : &(node->arg);
	for(size_t i =0;i<func->arguments.size();i++){
		mods.redirectors[reinterpret_cast<void*>(static_cast<Variable*>(func->arguments[i]))] = std::make_pair(reinterpret_cast<void*>(argsBegin[i]),true);
	}
	//Simple form of f(x) = x
	if(func->body.scope->numberOfDefinitions() <= func->arguments.size() && func->body.children.size() == 1){
		if(auto ret = func->body.children[0]->asReturnExpression()){
			return eval(ret->value->duplicate(&mods));
		}
	}
	//Mixin definitions
	auto target = inlined ? new Scope(currentScope()) : currentScope();
	mods.location = node->location;
	mods.target = target;
	func->body.scope->duplicate(&mods);
	//inline body
	if(!func->_returnType.isResolved() || !func->returnType()->isSame(intrinsics::types::Void)){
		auto varName = std::string(inlined ? "_inlined_" : "_mixined_") + std::string(func->id.ptr());
		auto v = new Variable(SymbolID(varName.begin()._Ptr,varName.length()),node->location,currentScope()->functionOwner() ? true : false);
		v->isMutable = false;
		//v->type.infer(func->returnType());
		currentScope()->define(v);
		result = new VariableReference(v);
		mods.returnValueRedirector = v;
	}else result = new UnitExpression();
	//NB when not inlined still need to create a new empty scope anyway!!
	auto block = new BlockExpression(inlined ? target : new Scope(currentScope()));
	func->body._duplicate(block,&mods);
	mixinedExpression = eval(block);
	return eval(result);
}

//TODO function duplication with certain wildcard params - which scope to put in generated functions?
Node* Evaluator::constructFittingArgument(Function** function,Node *arg){
	Function* func = *function;
	std::vector<Node*> result;
	result.resize(func->arguments.size());
	bool determinedFunction = false;
	std::vector<TypeExpression* > determinedArguments;//Boolean to indicate whether the argument was expanded at compile time and is no longer needed

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
		if(argsCount > 0 && func->arguments[argsCount-1]->type.isWildcard()){
			argsCount--;
			auto tuple = new TupleExpression();
			for(auto i = argsCount;i<expressionCount;i++)
				tuple->children.push_back(exprBegin[i]);
			result[argsCount] = eval(tuple);
			assert(result[argsCount]->isResolved());
			expressionCount = argsCount;

			determinedFunction = true;
			determinedArguments.resize(argsCount+1,nullptr);
			determinedArguments[argsCount] = result[argsCount]->_returnType();
		}
		else assert(false);
	}

	while(currentExpr<expressionCount){
		auto label = exprBegin[currentExpr]->label();
		if(!label.isNull()){
			//Labeled
			for(currentArg =lastNonLabeledExpr ; currentArg < argsCount;currentArg++){
				if(func->arguments[currentArg]->id == label){
					if(func->arguments[currentArg]->type.isWildcard()){
						result[currentArg] = exprBegin[currentExpr];
						if(!determinedFunction){
							determinedFunction = true;
							determinedArguments.resize(argsCount,nullptr);
						}
						determinedArguments[currentArg] = result[currentArg]->_returnType();
					}
					else result[currentArg] = func->arguments[currentArg]->type.type()->assignableFrom(exprBegin[currentExpr],exprBegin[currentExpr]->_returnType());
					break;
				}
			}		
		}
		else{
			//NonLabeled
			if(func->arguments[currentArg]->type.isWildcard()){
				result[currentArg] = exprBegin[currentExpr];
				if(!determinedFunction){
					determinedFunction = true;
					determinedArguments.resize(argsCount,nullptr);
				}
				determinedArguments[currentArg] = result[currentArg]->_returnType();
			}
			else result[currentArg] = func->arguments[currentArg]->type.type()->assignableFrom(exprBegin[currentExpr],exprBegin[currentExpr]->_returnType());
			lastNonLabeledExpr = currentExpr;
		}
		currentArg++;resolvedArgs++;currentExpr++;	
	}

	//Default args at end
	for(currentArg = resolvedArgs; currentArg < argsCount;currentArg ++){
		result[currentArg] = func->arguments[currentArg]->defaultValue()->duplicate();
	}

	//Determine the function?
	if(determinedFunction && !func->intrinsicEvaluator){
		DuplicationModifiers mods;
		mods.target = currentScope();
		mods.location = arg->location;
		auto f = func->specializedDuplicate(&mods,determinedArguments);
		f->resolve(this);
		*function = f;
	}

	if((*function)->canExpandAtCompileTime() && !(*function)->intrinsicEvaluator){
		DuplicationModifiers mods;
		mods.target = currentScope();
		mods.location = arg->location;
		auto f = (*function)->expandedDuplicate(&mods,result);
		f->resolve(this);
		*function = f;
	}

	//Construct a proper argument
	if(result.size() == 0){
		if(auto u = arg->asUnitExpression()) return arg; //arg is ()
		//delete arg;
		return new UnitExpression();
	}
	else if(result.size() == 1){
		if(auto u = arg->asUnitExpression()) delete arg;
		return result[0];
	}else{
		TupleExpression* tuple;
		if(!(tuple = arg->asTupleExpression())){
			if(auto u = arg->asUnitExpression()) delete arg;
			tuple = new TupleExpression();
		}else{
			tuple->type = nullptr;
		}
		tuple->children = result;
		return tuple;
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
*Also:
*	f(1,2) matches f(x)
*	f(1,2,3) matches f(x,y)
*TODO weight system for f(x) and f(x int32)
*/
bool match(Function* func,Node* arg){
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
		if(argsCount > 0 && func->arguments[argsCount-1]->type.isWildcard()){
			argsCount--;
			expressionCount = argsCount;
		}
		else return false;
	}

	while(currentExpr<expressionCount){
		auto label = exprBegin[currentExpr]->label();
		if(!label.isNull()){
			//Labeled
			//TODO same label multiple times error!
			bool foundMatch = false;
			for(currentArg =lastNonLabeledExpr ; currentArg < argsCount;currentArg++){
				if(func->arguments[currentArg]->id == label){
					if( func->arguments[currentArg]->type.isWildcard() ||
						func->arguments[currentArg]->type.type()->canAssignFrom(exprBegin[currentExpr],exprBegin[currentExpr]->_returnType()) ){
						foundMatch = true;
						break;
					}else{
						return false;	
					}
				}
			}
			if(!foundMatch) return false;	
		}
		else{
			//NonLabeled
			if(!(currentArg < argsCount)) return false;//f(x:5,6) where x is the last arg
			if( func->arguments[currentArg]->type.isWildcard() ||
				func->arguments[currentArg]->type.type()->canAssignFrom(exprBegin[currentExpr],exprBegin[currentExpr]->_returnType()) ){
				lastNonLabeledExpr = currentExpr;
			}
			else return false;
		}
		currentArg++;resolvedArgs++;currentExpr++;	
	}

	//Ending
	if(resolvedArgs == argsCount) return true;//() matches ()
	for(currentArg = resolvedArgs; currentArg < argsCount;currentArg ++){
		if(!func->arguments[currentArg]->defaultValue()) return false; //() doesn't match (x,y)
	}
	return true; //() matches (x = 1,y = false)
}

void Evaluator::findMatchingFunctions(std::vector<Function*>& overloads,std::vector<Function*>& results,Node* argument,bool enforcePublic){
	for(auto i=overloads.begin();i!=overloads.end();++i){
		if(enforcePublic && (*i)->visibilityMode != Visibility::Public) continue;
		if(!(*i)->_argsResolved) continue; //TODO what if we need this
		if(match((*i),argument)) results.push_back(*i);
	}
}
