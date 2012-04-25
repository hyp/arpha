#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "interpreter.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

namespace {
	std::map<Function*,Node* (*)(Interpreter*,CallExpression*,Node*)> functionBindings;
}

void Interpreter::init(Scope* arphaScope){
	#define HANDLE(func,type,body)  { \
		struct Handler { \
			static Node* handle(Interpreter* interpreter,CallExpression* node,Node* argument) body \
	    }; \
		functionBindings[arphaScope->resolve(func,type)] = &(Handler::handle); }

	HANDLE("typeof",compiler::expression,{ 
		return interpreter->expressionFactory->makeTypeReference(returnType(argument)); 
	});
	HANDLE("sizeof",compiler::expression,{ 
		auto size = interpreter->expressionFactory->makeConstant();
		auto isTypeAlready = argument->is<ConstantExpression>() && ((ConstantExpression*)argument)->type == compiler::type;
		size->u64  = uint64( ( isTypeAlready ? ((ConstantExpression*)argument)->refType : returnType(argument) )->size );
		size->type = arpha::uint64;
		return size;
	});

	#undef HANDLE
}

Node* interpret(Interpreter* interpreter,CallExpression* node){
	assert(node->object->is<ConstantExpression>() && ((ConstantExpression*)node->object)->type == compiler::function);
	auto function = ((ConstantExpression*)node->object)->refFunction;
	auto handler = functionBindings.find(function);
	if(handler != functionBindings.end()) return handler->second(interpreter,node,node->arg);
	return node;
}

#define CASE(t) case t::__value__: node = interpret(interpreter,(t*)node); break

Node* interpret(Interpreter* interpreter,Node* node){
	switch(node->__type){
		CASE(CallExpression);
	}

	return node;	
}

#undef CASE

Node* Interpreter::interpret(Node* node){
	return ::interpret(this,node);
}