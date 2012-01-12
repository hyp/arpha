#include "common.h"
#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

//expression evaluation - resolving overloads, inferring types, invoking ctfe

inline Node* evaluate(Parser* parser,CallExpression* node){
#define CASE(t) case t::__value__
	node->arg = parser->evaluate(node->arg);
	auto argumentType = returnType(node->arg);
	Definition* def;

	if(argumentType == arpha::Nothing) error(node->arg->location,"Can't perform function call on nothing!");
	else if(argumentType != arpha::Unresolved){
		switch( node->object->__type){
		CASE(OverloadSetExpression):
			/*def = ((OverloadSetExpression*)node->object)->scope->lookup(((OverloadSetExpression*)node->object)->symbol);	
			if(def){
				assert(def->isOverloadSet());//TO
				//expr = def->resolve(expr);
				//need check before evaluation to avoid infinite recursion when resolve fails
				//if(expr->flags != Expression::Unresolved) expr = evaluate( expr ); 
			}*/		
			break;
		CASE(TypeExpression):
			error(node->object->location,"Type call not implemented");
			break;
		CASE(FunctionExpression):
			break;
		default:
			error(node->object->location,"Can't perform a function call onto %s!",node->object);
		}
	}

	return node;
#undef CASE
}

inline Node* evaluate(Parser* parser,TupleExpression* node){
	if(node->children.size() == 0){ node->type= arpha::Nothing; return node; }
	
	std::vector<Type::Field> fields;
	
	node->type = nullptr;
	Type* returns;
	for(size_t i =0;i<node->children.size();i++){
		node->children[i] = parser->evaluate( node->children[i] );
		returns = returnType(node->children[i]);

		if(returns == arpha::Nothing){
			error(node->children[i]->location,"a tuple can't have an expression which returns nothing!");
			node->type = arpha::Error;
		}
		else if(returns == arpha::Unresolved) node->type = arpha::Unresolved;
		else fields.push_back(Type::Field(returns,SymbolID()));
	}

	if(!node->type) node->type = Type::tuple(fields);
	return node;
}

inline Node* evaluate(Parser* parser,BlockExpression* node){
	for(size_t i =0;i<node->children.size();i++)
		node->children[i] = parser->evaluate( node->children[i] );
	return node;
}

#define CASE(t) case t::__value__: node = ::evaluate(this,(t*)node); break

Node* Parser::evaluate(Node* node){

	switch(node->__type){
		CASE(CallExpression);
		CASE(TupleExpression);
		CASE(BlockExpression);
	}

	return node;
}

#undef CASE