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

	struct State {
		const char* src;
		Location location;
	};

	Parser(const char* src,Evaluator* evaluator);
	void saveState(State *state);
	void restoreState(State *state);

	void expect(SymbolID token);
	SymbolID expectName();

	bool match(SymbolID token);
	bool match(int tokenType);

	Node* parse(int stickiness =  0);

	/// Resolve symbols, find the matching function call overloads, constant fold
	Node* evaluate(Node* node);


	//Current parsing state
	Token  lookedUpToken;
private:
	Scope* _currentScope;
	Evaluator* _evaluator;
	
public:
	SymbolID labelForNextNode;

	inline Scope* currentScope() const { return _currentScope; }
	inline Evaluator* evaluator() const { return _evaluator; }
	void currentScope(Scope* scope);
};


#endif