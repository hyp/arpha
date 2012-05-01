/**
* This module contains the heart of arpha - THE Parser.
*/
#ifndef ARPHA_PARSER_H
#define ARPHA_PARSER_H

#include "token.h"
#include "lexer.h"
#include "../ast/evaluate.h"

struct Node;
struct Scope;

struct Parser : Lexer {

	Parser(const char* src);

	void expect(SymbolID token);
	SymbolID expectName();
	int expectInteger();

	bool match(SymbolID token);
	bool match(int tokenType);

	Type* expectType(int stickiness);
	Type* matchType(int stickiness);
	std::pair<Type*,Node*> matchTypeOrUnresolved(int stickiness);

	Node* parse(int stickiness =  0);

	/// Resolve symbols, find the matching function call overloads, constant fold
	Node* evaluate(Node* node);

	//Current parsing state
	Token  lookedUpToken;
private:
	Scope* _currentScope;
	size_t unresolvedExpressions,solvedExpressions;
	Evaluator firstRoundEvaluator;
public:

	inline Scope* currentScope() const { return _currentScope; }
	void currentScope(Scope* scope);
};


#endif