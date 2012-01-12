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

	if(argumentType == compiler::Nothing) error(node->arg->location,"Can't perform function call on a statement!");
	else if(argumentType != compiler::Unresolved){
		switch( node->object->__type){
		CASE(OverloadSetExpression):
			def = ((OverloadSetExpression*)node->object)->scope->lookup(((OverloadSetExpression*)node->object)->symbol);	
			if(def){
				assert(def->isOverloadSet());//TO
				std::vector<Function*> functions;
				((OverloadSet*)def)->findMatches(functions,node->arg);
				if(functions.size() == 1){
					node->object = parser->expressionFactory->makeFunction(functions[0]);
					debug("Overload successfully resolved as %s: %s",functions[0]->id,functions[0]->argument->id);
				}
				else if(functions.size() > 1){
					error(node->location,"multiple overloads possible");
				}
			}	
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

		if(returns == compiler::Nothing){
			error(node->children[i]->location,"a tuple can't have a statement member");
			node->type = compiler::Error;
		}
		else if(returns == compiler::Unresolved) node->type = compiler::Unresolved;
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