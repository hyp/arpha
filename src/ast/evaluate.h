/**
* This module implemements various AST expansion and evaluation layers.
* It includes type inferrer, type checker, '.' notation node transformation, constant folding and other things.
*/
#ifndef ARPHA_AST_EVAL_H
#define ARPHA_AST_EVAL_H

struct Node;
struct Scope;

struct Evaluator {
	Node* eval(Node* node);
	

	static void init(Scope* arphaScope);
private:
	Scope* _currentScope;
	
public:
	bool evaluateExpressionReferences;
	bool evaluateTypeTuplesAsTypes;
	size_t unresolvedExpressions;

	Evaluator() : evaluateExpressionReferences(false),evaluateTypeTuplesAsTypes(false),unresolvedExpressions(0) {}

	inline Scope* currentScope() const { return _currentScope; }
	void currentScope(Scope* scope){ _currentScope = scope; }
};

#endif