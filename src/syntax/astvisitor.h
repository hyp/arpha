/**
*  This module contains the abstract visitor class which can walk over the ast.
*/
#ifndef ARPHA_AST_VISITOR_H
#define ARPHA_AST_VISITOR_H

#include "ast.h"

struct NodeVisitor {
#define VISIT(T) virtual Node* visit(T* node){ return node; }
	NODE_LIST(VISIT)
#undef VISIT
};

#endif