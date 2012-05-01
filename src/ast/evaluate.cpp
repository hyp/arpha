#include "../common.h"
#include "../scope.h"
#include "../declarations.h"
#include "node.h"
#include "visitor.h"
#include "../compiler.h"
#include "../arpha.h"
#include "evaluate.h"

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
		
	std::vector<std::pair<SymbolID,Type*>> r(2,std::make_pair(SymbolID(),arpha::constantString));
	r[1] = std::make_pair(SymbolID(),compiler::type);
	auto string_type = Type::tuple(r);
	HANDLE("resolve",string_type,{ 
		
		auto r = argument->asTupleExpression();
		auto f = scope->resolve(r->children[0]->asConstantExpression()->string.ptr(),r->children[1]->asConstantExpression()->refType);
		System::print(format("compiler.Resolved %s %s!\n",argument,f->id));
		return ConstantExpression::createFunctionReference(f);
	});
	HANDLE("error",arpha::constantString,{ 
		error(node->location,argument->asConstantExpression()->string.ptr());
		return argument; 
	});
	HANDLE("constexpr",compiler::expression,{ 
		if(!argument->asConstantExpression()){
			error(node->location,"Expected a constant expression instead of %s!",argument);
		}
		return argument; 
	});
	HANDLE("dumpAST",compiler::expression,{ 
		System::print(format("------------------- AST dump: ------------------------------\n%s\n\n",argument));
		return argument; 
	});
	HANDLE("dumpDEF",compiler::expression,{ 
		auto cnst = argument->asConstantExpression();
		if(cnst->type == compiler::type){
			System::print(format("------------------- DEF dump: ------------------------------\nType %s (sizeof %s)\n",cnst->refType->id,cnst->refType->size));
			for(auto i = cnst->refType->fields.begin();i!=cnst->refType->fields.end();++i){
				System::print(format("  field %s of type %s\n",(*i).id,(*i).type->id));
			}
			System::print("\n");
		}
		else if(cnst->type == compiler::function){
			System::print(format("------------------- DEF dump: ------------------------------\nFunction %s type %s -> %s \n",
				cnst->refFunction->id,cnst->refFunction->argument->id,cnst->refFunction->returnType->id));
			System::print(format("%s",cnst->refFunction->body));
			System::print("\n");
		}
		else if(cnst->type == compiler::scopeRef){
			System::print(format("------------------- DEF dump: ------------------------------\nScope \n"));
			System::print("\n");
		}
		return argument; 
	});

	#undef HANDLE
	#define HANDLE(func,type,body) _HANDLE(arphaScope,func,type,body)

	HANDLE("typeof",compiler::expression,{ 
		return ConstantExpression::createTypeReference(argument->returnType()); 
	});
	HANDLE("sizeof",compiler::expression,{ 
		auto size = ConstantExpression::create(arpha::uint64);//TODO arpha.natural
		auto cnstArgument = argument->asConstantExpression();
		size->u64  = uint64( ( cnstArgument->type == compiler::type ? cnstArgument->refType : argument->returnType() )->size );
		return size;
	});
	//TODO - implement
	//realAssert = arphaScope->resolve("assert",arpha::boolean);
	//TODO - implement in Arpha
	HANDLE("assert",compiler::expression,{
		auto cnst = argument->asConstantExpression();
		if(cnst && cnst->type == arpha::boolean && cnst->u64==1){
			return node;		
		}
		error(argument->location,"Test error - Assertion failed");
		return node;
	});

	//
	std::vector<std::pair<SymbolID,Type*>> record(2,std::make_pair(SymbolID(),compiler::type));
	auto type_type = Type::tuple(record);
	HANDLE("equals",type_type,{
		auto twoTypes = argument->asTupleExpression();
		auto t1 = twoTypes->children[0]->asConstantExpression()->refType;
		auto t2 = twoTypes->children[1]->asConstantExpression()->refType;
		auto result = ConstantExpression::create(arpha::boolean);
		result->u64 =  t1 == t2 ? 1 : 0; //TODO tuple comparsion as well
		return result;
	});

	#undef HANDLE
	#undef _HANDLE
}

Node* evaluateResolvedFunctionCall(Scope* scope,CallExpression* node){
	auto function = node->object->asConstantExpression()->refFunction;

	//Try to expand the function
	auto handler = functionBindings.find(function);
	if(handler != functionBindings.end()){
		debug("Expanding a function call %s with %s",function->id,node->arg);
		return handler->second(scope,node,node->arg);
	}
	return node;
}

struct AstExpander: NodeVisitor {
	Evaluator* evaluator;
	AstExpander(Evaluator* ev) : evaluator(ev) {}

	Node* visit(VariableExpression* node){
		return node->variable->value ? node->variable->value : node;
	}

	//on a.foo(...)
	static Node* transformCallOnAccess(CallExpression* node,Type* argumentType,AccessExpression* acessingObject){
		debug("calling on access! %s with %s",acessingObject,node->arg);
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
		return node;
	}
	//TODO Type call -> constructor.
	Node* evalTypeCall(Node* node){
		return node;
	}

	Node* visit(CallExpression* node){
		//evaluate argument
		node->arg = node->arg->accept(this);
		auto argumentType = node->arg->returnType();
	
		if(argumentType == compiler::Nothing) error(node->arg->location,"Can't perform function call on a statement!");
		else if(argumentType != compiler::Unresolved){
			if(auto callingOverloadSet = node->object->asOverloadSetExpression()){
				auto func = callingOverloadSet->scope->resolveFunction(callingOverloadSet->symbol,node->arg);
				if(func){
					node->object = ConstantExpression::createFunctionReference(func);
					//TODO function->adjustArgument
					debug("Overload successfully resolved as %s: %s",func->id,func->argument->id);
					return evaluateResolvedFunctionCall(evaluator->currentScope(),node);
				}else{
					//TODO mark current block as unresolved!
				}
			}
			else if(auto callingCnst = node->object->asConstantExpression()){
				if(callingCnst->type == compiler::function) return node;//TODO eval function?
				else if(callingCnst->type == compiler::type) return evalTypeCall(node); //->accept(this) ???
				else error(node->object->location,"Can't perform a function call %s!",node->object);
			}
			else if(auto callingAccess = node->object->asAccessExpression()){
				return transformCallOnAccess(node,argumentType,callingAccess)->accept(this);
			}
			else
				error(node->object->location,"Can't perform a function call %s!",node->object);
		}

		return node;
	}
	Node* visit(AssignmentExpression* node){
		node->value = node->value->accept(this);

		if(node->value->returnType() != compiler::Unresolved){
			if(auto var = node->object->asVariableExpression()){
				if(var->returnType() == compiler::Unresolved) var->variable->inferType(node->value->returnType()); //Infer types for variables
			}else if(auto access = node->object->asAccessExpression()){
				//TODO a.foo = .. when foo is field
				//a.foo = 2 -> foo(a,2)
				auto args = TupleExpression::create(access->object,node->value);
				return CallExpression::create(OverloadSetExpression::create(access->symbol,access->scope),args)->accept(this);
			}
			else error(node->location,"Can't assign to %s!",node->object);
		}
		return node;
	}
	Node* visit(AccessExpression* node){
		node->object = node->object->accept(this);
		if(node->passedFirstEval){
			auto objectType = node->object->returnType();
			//TODO type field access & expression '.' call notation
			if(auto field = objectType->lookupField(node->symbol)){
				//TODO This may need to be done only on 2nd iteration, because there might be setters/getters defined on a later on in the module
				//TODO - how to mark it??
				return node;
			}
			else return CallExpression::create(OverloadSetExpression::create(node->symbol,node->scope),node->object)->accept(this);
		}
		else node->passedFirstEval = true;
		return node;
	}
	Node* visit(TupleExpression* node){
		if(node->children.size() == 0){ node->type= arpha::Nothing; return node; }
	
		std::vector<std::pair<SymbolID,Type*>> fields;
	
		node->type = nullptr;
		Type* returns;
		for(size_t i =0;i<node->children.size();i++){
			node->children[i] = node->children[i]->accept(this);
			returns = node->children[i]->returnType();

			if(returns == compiler::Nothing){
				error(node->children[i]->location,"a tuple can't have a statement member");
				node->type = compiler::Error;
			}
			else if(returns == compiler::Unresolved) node->type = compiler::Unresolved;
			else fields.push_back(std::make_pair(SymbolID(),returns));
		}

		if(!node->type) node->type = Type::tuple(fields);
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
	Node* visit(IfExpression* node){
		node->condition = node->condition->accept(this);
		node->consequence = node->consequence->accept(this);
		if(node->alternative) node->alternative = node->alternative->accept(this);
		auto constantCondition = node->condition->asConstantExpression();
		if(constantCondition && constantCondition->type == arpha::boolean){ //TODO interpret properly
			return constantCondition->u64 ? node->consequence : node->alternative;
		}
		return node;
	}
	Node* visit(WhileExpression* node){
		node->condition = node->condition->accept(this);
		node->body = node->body->accept(this);
		return node;
	}
	Node* visit(VariableDeclaration* node){
		node->unresolvedTypeExpression = node->unresolvedTypeExpression->accept(this);
		ConstantExpression* resolved;
		if((resolved = node->unresolvedTypeExpression->asConstantExpression()) && resolved->type == compiler::type){
			auto type= resolved->refType;
			debug("Resolved type %s for variables %s",type->id,node->variables);
			node->resolveType(type);
			//delete node->unresolvedTypeExpression
			//delete node
			return node->variables;
		}
		return node;
	}
	Node* visit(TypeDeclaration* node){
		bool allSolved = true;
		for(auto i = node->unresolvedTypeExpressions.begin();i!=node->unresolvedTypeExpressions.end();i++){
			(*i).first = (*i).first->accept(this);
			ConstantExpression* resolved;
			if((resolved = (*i).first->asConstantExpression()) && resolved->type == compiler::type){
				debug("Resolved type %s for %s's fields %s..%s",resolved->refType,node->type,(*i).second.first,(*i).second.second);
				for(int j=(*i).second.first;j<(*i).second.second;j++) node->type->fields[j].type = resolved->refType;
			}
			else allSolved = false;
		}
		if(allSolved){
			node->type->resolved = true;
			node->type->updateState();
			auto ref = ConstantExpression::createTypeReference(node->type);
			//delete all node->unresolvedTypeExpression.first
			//delete node
			return ref;
		}
		return node;
	}
	Node* visit(UnresolvedDeclaration* node){
		if(node->unresolvedType->resolved){
			auto ref = ConstantExpression::createTypeReference(node->unresolvedType);
			//delete node
			return ref;
		}
		return node;
	}
};
Node* Evaluator::eval(Node* node){
	AstExpander expander(this);
	return node->accept(&expander);
}
