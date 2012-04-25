#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "interpreter.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"



Node* interpret(Interpreter* interpreter,CallExpression* node){

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