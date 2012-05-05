#include "../common.h"
#include "../scope.h"
#include "../ast/declarations.h"
#include "node.h"
#include "visitor.h"
#include "../compiler.h"
#include "../arpha.h"
#include "evaluate.h"
#include "../intrinsics/ast.h"
#include "../intrinsics/types.h"

//expression evaluation - resolving overloads, inferring types, invoking ctfe

namespace {
	std::map<Function*,Node* (*)(Scope*,CallExpression*,Node*)> functionBindings;
	Function* realAssert;
}

void Evaluator::init(Scope* compilerScope,Scope* arphaScope){
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

Node* evaluateResolvedFunctionCall(Scope* scope,CallExpression* node){
	auto function = node->object->asFunctionReference()->function();

	//Try to expand the function
	auto handler = functionBindings.find(function);
	if(handler != functionBindings.end()){
		debug("Expanding a function call %s with %s",function->id,node->arg);
		return handler->second(scope,node,node->arg);
	}else if(function->intrinsicEvaluator) 
		return function->intrinsicEvaluator(node);
	return node;
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
	static Node* transformCallOnAccess(CallExpression* node,TypeExpression* argumentType,AccessExpression* acessingObject){
		/*debug("calling on access! %s with %s",acessingObject,node->arg);
		//a.foo()
		if(argumentType == arpha::Nothing){
			//TODO delete node->arg;
			node->arg  = acessingObject->object;
		}
		//a.foo(bar)
		else{
			if(auto isArgRecord = node->arg->asTupleExpression())
				isArgRecord->children.insert(isArgRecord->children.begin(),acessingObject->object);
			else
				node->arg = TupleExpression::create(acessingObject->object,node->arg);
		}
		auto newCalleeObject = OverloadSetExpression::create(acessingObject->symbol,acessingObject->scope);
		//TODO delete_no_children node->object
		node->object = newCalleeObject;
		return node;*/
		return node;
	}
	//TODO Type call -> constructor.
	Node* evalTypeCall(CallExpression* node,TypeExpression* type){
		assert(type != intrinsics::types::Unresolved);
		/*if(type == intrinsics::ast::Expression){
			debug("Expression of");
			auto r = ExpressionReference::create(node->arg);
			//delte node
			return r;
		}*/
		if(node->arg->_returnType() == intrinsics::types::Void) error(node->arg->location,"Can't perform type call onto a statement!");
		return node;
	}

	Node* visit(CallExpression* node){
		//evaluate argument
		node->arg = node->arg->accept(this);
		auto argumentType = node->arg->_returnType();

		if(auto callingType = node->object->asTypeExpression()){
			return evalTypeCall(node,callingType);
		}

		if(argumentType == intrinsics::types::Void) error(node->arg->location,"Can't perform function call on a statement!");
		else if(argumentType != intrinsics::types::Unresolved){
			if(auto callingOverloadSet = node->object->asOverloadSetExpression()){
				auto func = callingOverloadSet->scope->resolveFunction(callingOverloadSet->symbol,node->arg);
				if(func){
					node->object = new FunctionReference(func);
					//TODO function->adjustArgument
					debug("Overload successfully resolved as %s: %s",func->id,func->argument);
					if(func == intrinsics::ast::mixin){
						auto oldSetting = evaluator->evaluateExpressionReferences;
						evaluator->evaluateExpressionReferences = true;
						auto e = node->arg->accept(this);
						evaluator->evaluateExpressionReferences = oldSetting;
						return e;
					}
					return evaluateResolvedFunctionCall(evaluator->currentScope(),node);
				}
			}
			else if(auto callingFunc = node->object->asFunctionReference())
				return node;	//TODO eval?
			else if(auto callingAccess = node->object->asAccessExpression()){
				return transformCallOnAccess(node,argumentType,callingAccess)->accept(this);
			}
			else
				error(node->object->location,"Can't perform a function call %s!",node->object);
		}

		return node;
	}

	
	Node* assign(Node* object,Node* value,bool* error){
		auto valuesType = value->_returnType();
		if(valuesType  == intrinsics::types::Unresolved) return nullptr;
		//Assigning values to variables
		if(auto var = object->asVariableReference()){
			auto variablesType = var->variable->_type;
			if(variablesType == intrinsics::types::Inferred){ //inferred
				var->variable->_type = valuesType;
				debug("Inferred %s for variable %s",valuesType,var->variable->id);
				return value;
			}
			else if(variablesType->resolved()){
				//If variable has a constant type, assign only at place of declaration
				if(variablesType->type == TypeExpression::CONSTANT){
					if(var->isDefinedHere){
						variablesType = variablesType->next;//remove const
						//check again for inferred %_%
						if(variablesType == intrinsics::types::Inferred){
							var->variable->_type->next = valuesType;
							debug("Inferred %s for variable %s",valuesType,var->variable->id);
							return value;
						}
					}
					else {
						error(value->location,"Can't assign %s to %s - constant variables can only be assigned at declaration!",value,object);
						*error = true;
						return nullptr;
					}
				}
				if(auto canBeAssigned = variablesType->assignableFrom(value,valuesType)) return canBeAssigned;
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
	Node* visit(AssignmentExpression* node){
		node->value = node->value->accept(this);
		if(node->value->_returnType()  == intrinsics::types::Unresolved) return node;//Don't bother until the value is fully resolved
		bool error = false;

		//TODO flatten tuples
		if(auto t1 = node->object->asTupleExpression()){
			if(auto t2 = node->value->asTupleExpression()){
				if(t1->children.size() == t2->children.size()){
					for(size_t i=0;i<t1->children.size();i++){
						auto newValue = assign(t1->children[i],t2->children[i],&error);
						if(newValue) t2->children[i] = newValue;
					}
				}
				else{
					error(node->location,"Can't assign between tuples of different length");
					error = true;
				}
			}
			else{
				error(node->location,"Can't assign a non-tuple to a tuple");
				error = true;
			}
		}else{
			//A non-tuple assignment or a tuple to a single variable assignment
			auto newValue = assign(node->object,node->value,&error);
			if(newValue) node->value = newValue;
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
	
	Node* visit(AccessExpression* node){
		node->object = node->object->accept(this);
		if(node->passedFirstEval){
			auto objectType = node->object->_returnType();
			//TODO type field access & expression '.' call notation
			/*if(auto field = objectType->lookupField(node->symbol)){
				//TODO This may need to be done only on 2nd iteration, because there might be setters/getters defined on a later on in the module
				//TODO - how to mark it??
				return node;
			}
			else*/ return CallExpression::create(OverloadSetExpression::create(node->symbol,node->scope),node->object)->accept(this);
		}
		else node->passedFirstEval = true;
		return node;
	}
	Node* visit(TupleExpression* node){
		if(node->children.size() == 1){
			auto child = node->children[0];
			delete node;
			return child;
		}else if(node->children.size() == 0){
			delete node;
			return UnitExpression::getInstance();
		}

		std::vector<Record::Field> fields;
	
		node->type = nullptr;
		TypeExpression* returns;
		for(size_t i =0;i<node->children.size();i++){
			node->children[i] = node->children[i]->accept(this);
			returns = node->children[i]->_returnType();

			if(returns == intrinsics::types::Void){
				error(node->children[i]->location,"A tuple can't contain an expression returning void!");
				node->type = intrinsics::types::Unresolved;
			}
			else if(returns == intrinsics::types::Unresolved) node->type = intrinsics::types::Unresolved;
			else fields.push_back(Record::Field(SymbolID(),returns));
		}
		if(!node->type){
			if(evaluator->evaluateTypeTuplesAsTypes){
				bool allTypes = true;
				for(auto i=fields.begin();i!=fields.end();i++){
					if((*i).type != intrinsics::types::Type) allTypes = false;
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
		for(size_t i =0;i<node->children.size();i++)
			node->children[i] = node->children[i]->accept(this);
		return node;
	}
	Node* visit(ReturnExpression* node){
		//TODO function return type inferring
		return node;
	}

	Node* visit(WhileExpression* node){
		node->condition = node->condition->accept(this);
		node->body = node->body->accept(this);
		return node;
	}

};
Node* Evaluator::eval(Node* node){
	AstExpander expander(this);
	return node->accept(&expander);
}
