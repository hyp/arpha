/**
* This module implemements various AST expansion and evaluation layers.
* It includes type inferrer, type checker, '.' notation node transformation, constant folding and other things.
*/
#ifndef ARPHA_AST_EVAL_H
#define ARPHA_AST_EVAL_H

struct Node;
struct Scope;

struct Evaluator {
private:
	Scope* _currentScope;
	bool reportUnevaluated;
	
public:
	bool evaluateExpressionReferences; // = false
	bool isRHS; // = false
	bool forcedToEvaluate; // = false

	// Sometimes we know that we want an expression of certain type at a given place e.g. var x Foo <- we expect Foo to be TypeExpression
	// This knowledge can be used to resolve certain ambiguties: 
	//		e.g. int32,int32 will become a anonymous record type of 2 ints rather than a record containing two types!
	TypeExpression* expectedTypeForEvaluatedExpression; // = nullptr

	size_t unresolvedExpressions;

	Evaluator() : forcedToEvaluate(false),evaluateExpressionReferences(false),isRHS(false),reportUnevaluated(false),expectedTypeForEvaluatedExpression(nullptr),mixinedExpression(nullptr),unresolvedExpressions(0) {}

	Node* eval(Node* node);
	// Resolves expressions and definitions in a module using multiple passes
	void evaluateModule(BlockExpression* module);
	
	Node* mixinedExpression;
	Node* mixinFunctionCall(CallExpression* node,bool inlined = false);
	Node* mixinFunction(Location &location,Function* func,Node* arg,bool inlined = false);

	inline Scope* currentScope() const { return _currentScope; }
	void currentScope(Scope* scope){ _currentScope = scope; }

	void markUnresolved(Node* node);
	void markUnresolved(PrefixDefinition* node);

	//function overloads resolving

	Node* constructFittingArgument(Function** function,Node *arg,bool dependentChecker = false,int* weight = nullptr);

	void findMatchingFunctions(std::vector<Function*>& overloads,std::vector<Function*>& results,Node* argument,bool enforcePublic = false);

};

#endif