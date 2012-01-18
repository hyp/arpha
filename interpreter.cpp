#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "interpreter.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

Node* interpret(Interpreter* interpreter,CallExpression* node){
	/*assert(node->object->is<FunctionExpression>());
	auto function = ((FunctionExpression*)node->object)->function;
	
	if(function == arpha::typeof) return interpreter->expressionFactory->makeType(returnType(node->arg));
	else if(function == arpha::_sizeof){
		auto c = interpreter->expressionFactory->makeConstant();
		c->u64  = uint64( ( node->arg->is<TypeExpression>() ? ((TypeExpression*)node->arg)->type : returnType(node->arg) )->size );
		c->type = arpha::uint64;
		return c;
	}*/
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