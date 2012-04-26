#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "interpreter.h"
#include "ast.h"
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
		return parser->expressionFactory->makeTypeReference(returnType(argument)); 
	});
	HANDLE("sizeof",compiler::expression,{ 
		auto size = parser->expressionFactory->makeConstant();
		auto isTypeAlready = argument->is<ConstantExpression>() && ((ConstantExpression*)argument)->type == compiler::type;
		size->u64  = uint64( ( isTypeAlready ? ((ConstantExpression*)argument)->refType : returnType(argument) )->size );
		size->type = arpha::uint64;//TODO arpha.natural
		return size;
	});
	//TODO - implement
	realAssert = arphaScope->resolve("assert",arpha::boolean);
	//TODO - implement in Arpha
	HANDLE("assert",compiler::expression,{
		node->object = parser->expressionFactory->makeFunctionReference(realAssert);
		auto args= parser->expressionFactory->makeTuple();
		args->children.push_back(argument);
		auto line = parser->expressionFactory->makeConstant();
		line->u64 = parser->currentLocation().line();
		line->type = arpha::uint64;
		args->children.push_back(line);
		node->arg = parser->evaluate(args);
		return node;
	});

	#undef HANDLE
	#undef _HANDLE
}

Node* evaluateResolvedFunctionCall(Parser* parser,CallExpression* node){
	auto function = ((ConstantExpression*)node->object)->refFunction;

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

inline Node* evaluate(Parser* parser,CallExpression* node){
#define CASE(t) case t::__value__
	node->arg = parser->evaluate(node->arg);
	auto argumentType = returnType(node->arg);
	
	if(argumentType == compiler::Nothing) error(node->arg->location,"Can't perform function call on a statement!");
	else if(argumentType != compiler::Unresolved){
		if(node->object->is<OverloadSetExpression>()){
			auto func = ((OverloadSetExpression*)node->object)->scope->resolveFunction(((OverloadSetExpression*)node->object)->symbol,node->arg);
			if(func){
				node->object = parser->expressionFactory->makeFunctionReference(func);
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
#undef CASE
}

inline Node* evaluate(Parser* parser,TupleExpression* node){
	if(node->children.size() == 0){ node->type= arpha::Nothing; return node; }
	
	std::vector<std::pair<SymbolID,Type*>> fields;
	
	node->type = nullptr;
	Type* returns;
	for(size_t i =0;i<node->children.size();i++){
		node->children[i] = parser->evaluate( node->children[i] );
		returns = returnType(node->children[i]);

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

inline Node* evaluate(Parser* parser,BlockExpression* node){
	for(size_t i =0;i<node->children.size();i++)
		node->children[i] = parser->evaluate( node->children[i] );
	return node;
}

Node* evaluate(Parser* parser,IfExpression* node){
	node->condition = parser->evaluate(node->condition);
	node->consequence = parser->evaluate(node->consequence);
	node->alternative = parser->evaluate(node->alternative);
	if(node->condition->is<ConstantExpression>() && ((ConstantExpression*)node->condition)->type == arpha::boolean){ //TODO interpret properly
		return ((ConstantExpression*)node->condition)->u64 ? node->consequence : node->alternative;
	}
	return node;
}

#define CASE(t) case t::__value__: node = ::evaluate(this,(t*)node); break

Node* Parser::evaluate(Node* node){

	switch(node->__type){
		CASE(CallExpression);
		CASE(TupleExpression);
		CASE(BlockExpression);
		CASE(IfExpression);
	}

	return node;
}

#undef CASE