/**
* This module contains the heart of arpha - THE Parser.
*/
#ifndef ARPHA_PARSER_H
#define ARPHA_PARSER_H

#include "token.h"
#include "lexer.h"

struct Node;
struct Scope;

struct Parser : Lexer {

	Parser(const char* src,Scope* scope);

	void expect(SymbolID token);
	SymbolID expectName();
	int expectInteger();

	bool match(SymbolID token);
	bool match(int tokenType);

	bool isEndExpressionNext();

	Node* parse(int stickiness =  0);
	Node* parseModule();

	Type* parseOptionalType();

	/// Resolve symbols, find the matching function call overloads, constant fold
	Node* evaluate(Node* node);

	//Current parsing state
	Token  lookedUpToken;
	Scope* _currentScope;
	size_t unresolvedExpressions,solvedExpressions;

	inline Scope* currentScope() const { return _currentScope; }
};


#endif