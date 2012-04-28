#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "interpreter.h"
#include "syntax/ast.h"
#include "syntax/astvisitor.h"
#include "compiler.h"
#include "arpha.h"

//expression evaluation - resolving overloads, inferring types, invoking ctfe

namespace {
	std::map<Function*,Node* (*)(Parser*,CallExpression*,Node*)> functionBindings;
	Function* realAssert;
}

void Interpreter::init(Scope* compilerScope,Scope* arphaScope){
	#define _HANDLE(module,func,type,body)  { \
		struct Handler { \
			static Node* handle(Parser* parser,CallExpression* node,Node* argument) body \
	    }; \
		functionBindings[module->resolve(func,type)] = &(Handler::handle); }

	#define HANDLE(func,type,body) _HANDLE(compilerScope,func,type,body)
		
	//TODO HANDLE("resolve")

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
		error(parser->currentLocation(),"Test error - Assertion failed");
		/*node->object = parser->expressionFactory->makeFunctionReference(realAssert);
		auto args= parser->expressionFactory->makeTuple();
		args->children.push_back(argument);
		auto line = parser->expressionFactory->makeConstant();
		line->u64 = parser->currentLocation().line();
		line->type = arpha::uint64;
		args->children.push_back(line);
		node->arg = parser->evaluate(args);*/
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

Node* evaluateResolvedFunctionCall(Parser* parser,CallExpression* node){
	auto function = node->object->asConstantExpression()->refFunction;

	//Try to expand the function
	auto handler = functionBindings.find(function);
	if(handler != functionBindings.end()){
		debug("Expanding a function call %s with %s",function->id,node->arg);
		return handler->second(parser,node,node->arg);
	}
	/*
	bool interpret = false;

	if(function->argument == compiler::expression) interpret = true;
	//else if(node->arg->is<TypeExpression>()) interpret = true;

	
	if(!interpret) return node;
	debug("Interpreting function call %s with %s",function->id,node->arg);
	Interpreter interpreter;
	interpreter.expressionFactory = parser->expressionFactory;
	return interpreter.interpret(node);*/
	return node;
}

struct AstExpander: NodeVisitor {
	Parser* parser;

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
					return evaluateResolvedFunctionCall(parser,node);
				}else{
					//TODO mark current block as unresolved!
				}
			}else
				error(node->object->location,"Can't perform a function call onto %s!",node->object);
		}

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
	Node* visit(IfExpression* node){
		node->condition = node->condition->accept(this);
		node->consequence = node->consequence->accept(this);
		node->alternative = node->alternative->accept(this);
		auto constantCondition = node->condition->asConstantExpression();
		if(constantCondition && constantCondition->type == arpha::boolean){ //TODO interpret properly
			return constantCondition->u64 ? node->consequence : node->alternative;
		}
		return node;
	}
};
Node* Parser::evaluate(Node* node){
	AstExpander expander;
	expander.parser = this;
	return node->accept(&expander);
}
