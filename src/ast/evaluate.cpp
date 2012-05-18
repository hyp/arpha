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

namespace {
	std::map<Function*,Node* (*)(Scope*,CallExpression*,Node*)> functionBindings;
	Function* realAssert;
}

void Evaluator::init(Scope* arphaScope){
	#define _HANDLE(module,func,type,body)  { \
		struct Handler { \
			static Node* handle(Scope* scope,CallExpression* node,Node* argument) body \
	    }; \
		functionBindings[module->resolve(func,type)] = &(Handler::handle); }

	#define HANDLE(func,type,body) _HANDLE(compilerScope,func,type,body)
		
/*	std::vector<std::pair<SymbolID,Type*>> r(2,std::make_pair(SymbolID(),arpha::constantString));
	r[1] = std::make_pair(SymbolID(),compiler::type);
	auto string_type = Type::tuple(r);
	HANDLE("resolve",string_type,{ 
		
		//auto r = argument->asTupleExpression();
		//auto f = scope->resolve(r->children[0]->asConstantExpression()->string.ptr(),r->children[1]->asTypeReference()->type());
		//System::print(format("compiler.Resolved %s %s!\n",argument,f->id));
		//return FunctionReference::create(f);
		return node;
	});
	HANDLE("error",arpha::constantString,{ 
		//error(node->location,argument->asConstantExpression()->string.ptr());
		return argument; 
	});
	HANDLE("constexpr",compiler::expression,{ 
		/*if(!argument->asConstantExpression()){
			error(node->location,"Expected a constant expression instead of %s!",argument);
		}
		return argument; 
	});
	HANDLE("dumpAST",compiler::expression,{ 
		System::print(format("------------------- AST dump: ------------------------------\n%s\n\n",argument));
		return argument; 
	});
	HANDLE("dumpDEF",compiler::expression,{ 
		if(auto typeRef = argument->asTypeReference()){
			System::print(format("------------------- DEF dump: ------------------------------\nType %s (sizeof %s)\n",typeRef->type()->id,typeRef->type()->size()));
			for(auto i = typeRef->type()->fields.begin();i!=typeRef->type()->fields.end();++i){
				System::print(format("  field %s of type %s\n",(*i).id,(*i).type->id));
			}
			System::print("\n");
		}
		else if(auto funcRef = argument->asFunctionReference()){
			auto f = funcRef->function();
			System::print(format("------------------- DEF dump: ------------------------------\nFunction %s type %s -> %s \n",
				f->id,f->argument->id,f->returnType->id));
			System::print(format("%s",f->body));
			System::print("\n");
		}
		return argument; 
	});

	#undef HANDLE
	#define HANDLE(func,type,body) _HANDLE(arphaScope,func,type,body)

	HANDLE("typeof",compiler::expression,{ 
		return TypeReference::create(argument->returnType()); 
	});
	HANDLE("sizeof",compiler::expression,{ 
		/*auto size = ConstantExpression::create(arpha::uint64);//TODO arpha.natural
		auto typeRef = argument->asTypeReference();
		assert(typeRef ? typeRef->type() != compiler::Unresolved : true);
		size->u64  = uint64( (typeRef?typeRef->type():argument->returnType())->size() );
		//TODO isLiteral?
		return size;
		return node;
	});
	//TODO - implement
	//realAssert = arphaScope->resolve("assert",arpha::boolean);
	//TODO - implement in Arpha
	HANDLE("assert",compiler::expression,{
		//auto cnst = argument->asConstantExpression();
		//if(cnst && cnst->type == arpha::boolean && cnst->u64==1){
		//	return node;		
		//}
		error(argument->location,"Test error - Assertion failed");
		return node;
	});

	//
	std::vector<std::pair<SymbolID,Type*>> record(2,std::make_pair(SymbolID(),compiler::type));
	auto type_type = Type::tuple(record);
	HANDLE("equals",type_type,{
		/*auto twoTypes = argument->asTupleExpression();
		auto t1 = twoTypes->children[0]->asTypeReference()->type();
		auto t2 = twoTypes->children[1]->asTypeReference()->type();
		auto result = ConstantExpression::create(arpha::boolean);
		result->u64 =  t1 == t2 ? 1 : 0; //TODO tuple comparsion as well
		return result;
		return node;
	});*/

	#undef HANDLE
	#undef _HANDLE
}


Node* evaluateResolvedFunctionCall(Evaluator* evaluator,CallExpression* node){
	auto function = node->object->asFunctionReference()->function;

	//Try to expand the function
	auto handler = functionBindings.find(function);
	if(handler != functionBindings.end()){
		debug("Expanding a function call %s with %s",function->id,node->arg);
		return handler->second(evaluator->currentScope(),node,node->arg);
	}else if(function->intrinsicEvaluator) 
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

	Node* inlineFunction(CallExpression* node){
		assert(node->isResolved());
		auto func = node->object->asFunctionReference()->function;
		if(func->body.children.size() == 1){
			if(auto ret = func->body.children[0]->asReturnExpression() ){
				if(node->arg->asUnitExpression()){
					delete node;
					return ret->value->duplicate();
				}else{
					//TODO proper body Dup and replace arguments!
					//auto bodyDup = ret->value->duplicate();
					//replace arguments
					return nullptr;
				}
			}
		}
		return nullptr;
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
				node->object = new FunctionReference(func);
				//TODO function->adjustArgument
				if(func == intrinsics::ast::mixin){
					auto oldSetting = evaluator->evaluateExpressionReferences;
					evaluator->evaluateExpressionReferences = true;
					auto e = node->arg->accept(this);
					evaluator->evaluateExpressionReferences = oldSetting;
					return e;
				}
				node->_resolved = true;
				//if(auto inlined = inlineFunction(node)) return inlined;
				//else
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
		if(node->variable->expandMe)
			return node->variable->value->duplicate();//TODO is duplicate really needed?
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

	//TODO def x = 1;x = 1 => 1=1
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
		if(node->_resolved) node->object = node->object->accept(this); // Need to resolve object's tuple's type when some variable is inferred

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
	Node* visit(BlockExpression* node){
		assert(!node->isResolved());
		auto scp = evaluator->currentScope();
		debug("RSLV");
		node->_resolved = true;
		if(!node->scope->isResolved()){
			if(!node->scope->resolve(evaluator)) node->_resolved = false;//Resolve unresolved definitions
		}
		evaluator->currentScope(node->scope);
		for(size_t i =0;i<node->children.size();i++){
			if(!node->children[i]->isResolved()){
				node->children[i] = node->children[i]->accept(this);
				if(!node->children[i]->isResolved()) node->_resolved = false;
			}
			
		}
		evaluator->currentScope(scp);
		return node;
	}
	Node* visit(ReturnExpression* node){
		auto func = evaluator->currentScope()->functionOwner();
		if(func){
			func->_hasReturnInside = true;
			if(node->value){
				node->value = node->value->accept(this);
				if(node->value->isResolved()){
					if(func->_returnType.isInferred()){
						//TODO Don't allow to return local types
						auto valRet = node->value->_returnType();
						if(!valRet->hasLocalSemantics())
							func->_returnType.infer(valRet);
						else
							error(node->location,"Can't return %s because of local semantics!",node->value);
					}
					else if(func->_returnType.isResolved()){
						node->value = typecheck(node->value->location,node->value,func->_returnType.type());
					}
				}
			}else {
				if(func->_returnType.isInferred()) func->_returnType.infer(intrinsics::types::Void);
				else if(func->_returnType.isResolved()){
					//TODO proper typecheck?
					if(!func->_returnType.type()->isSame(intrinsics::types::Void)){
						error(node->location,"This function is expected to return void, not %s!",func->_returnType.type());
					}
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
