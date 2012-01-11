#include "common.h"
#include "parser.h"
#include "ast.h"
#include "arpha.h"

//expression evaluation - resolving overloads, inferring types, invoking ctfe

#define CASE(t) case t::__value__

inline void evaluate(CallExpression* node){
	switch( node->object->__type){
		CASE(OverloadSetExpression):
					break;
		CASE(TypeExpression):
					break;
		CASE(FunctionExpression):
					break;
	default:
		error(node->location,"Can't perform a function call onto %s",node->object);
	}
}

Node* evaluate(Node* node){

	switch(node->__type){

		CASE(CallExpression):
			//resolve a overloadset or type call to a function call
			switch( ((CallExpression*) node )->object->__type){
				CASE(OverloadSetExpression):
					break;
				CASE(TypeExpression):
					break;
				CASE(FunctionExpression):
					break;
				default:
			}
			break;

		CASE(TupleExpression):
			break;
	}

	return node;
}

#undef CASE