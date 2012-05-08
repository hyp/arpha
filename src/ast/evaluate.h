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
	bool evaluateExpressionReferences; // = false

	// Sometimes we know that we want an expression of certain type at a given place e.g. var x Foo <- we expect Foo to be TypeExpression
	// This knowledge can be used to resolve certain ambiguties: 
	//		e.g. int32,int32 will become a anonymous record type of 2 ints rather than a record containing two types!
	TypeExpression* expectedTypeForEvaluatedExpression; // = nullptr

	size_t unresolvedExpressions;

	Evaluator() : evaluateExpressionReferences(false),expectedTypeForEvaluatedExpression(nullptr),unresolvedExpressions(0) {}

	inline Scope* currentScope() const { return _currentScope; }
	void currentScope(Scope* scope){ _currentScope = scope; }

	void markUnresolved(Node* node);
};

#endif