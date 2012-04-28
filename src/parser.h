#ifndef ARPHA_PARSER_H
#define ARPHA_PARSER_H

#include "syntax/token.h"
#include "syntax/lexer.h"

//parsing
struct Node; //ast node(expression)
struct Expression;
struct Scope;
struct Definition;
struct OverloadSet;
struct ExpressionFactory;

struct Parser : Lexer {

	Parser(const char* src,Scope* scope);

	void expect(SymbolID token);
	SymbolID expectName();
	int expectInteger();

	bool match(SymbolID token);
	bool match(int tokenType);

	bool isEndExpressionNext();

	Node* _parse(int stickiness =  0);
	Node* _parseModule();

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