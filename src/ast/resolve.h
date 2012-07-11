/**
* This module implemements various AST expansion and resolving layers.
* It includes type inferrer, type checker, '.' notation node transformation, constant folding and other things.
*/
#ifndef ARPHA_AST_RESOLVE_H
#define ARPHA_AST_RESOLVE_H

struct Node;
struct Scope;

struct Interpreter;
struct CompilationUnit;

struct Resolver {
private:
	CompilationUnit* _compilationUnit;
	Scope* _currentScope;
	Node* _currentParent;
	bool reportUnevaluated;
	size_t unresolvedExpressions;
public:
	bool treatUnresolvedTypesAsResolved; //Used to resolve circular type definitions or type definitions which depend on self
	bool isRHS; // = false
	Function* currentFunction; // The function we are currently resolving. Can be null.

	// Sometimes we know that we want an expression of certain type at a given place e.g. var x Foo <- we expect Foo to be TypeExpression
	// This knowledge can be used to resolve certain ambiguties: 
	//		e.g. int32,int32 will become a anonymous record type of 2 ints rather than a record containing two types!
	Type* expectedTypeForEvaluatedExpression; // = nullptr

	Resolver(CompilationUnit* compilationUnit);

	CompilationUnit* compilationUnit() const { return _compilationUnit; }



	Node* resolve(Node* node);

	Node* multipassResolve(Node* node);

	// Resolves expressions and definitions in a module using multiple passes
	void resolveModule(BlockExpression* module);

	//Attempt to resolve macroes when they are defined, so that we can use them straight away
	Node* resolveMacroAtParseStage(Node* macro);

	//Function-like macro
	Node* executeAndMixinMacro(Function* function,Node* arg);

	inline Scope* currentScope() const        { return _currentScope;  }
	inline void currentScope(Scope* scope)    { _currentScope = scope; }

	inline void currentParentNode(Node* node) { _currentParent = node; }
	inline Node* currentParentNode() const    { return _currentParent; }

	inline void markResolved(Node* node){ node->setFlag(Node::RESOLVED); }
	void markUnresolved(Node* node);

	//function overloads resolving

	Function* resolveOverload(Scope* scope,SymbolID function,Type* functionType);
	Function* resolveOverload(Scope* scope,SymbolID function,Node* arg,bool dotSyntax = false);

	Function* specializeFunction(TypePatternUnresolvedExpression::PatternMatcher& patternMatcher,Function* original,Type** specializedParameters,Node** passedExpressions);
	Node* constructFittingArgument(Function** function,Node *arg,bool dependentChecker = false,int* weight = nullptr);

	void findMatchingFunctions(std::vector<Function*>& overloads,std::vector<Function*>& results,Node* argument,bool enforcePublic = false);

};

Node* mixinMacro(CTFEinvocation* invocation,Scope* scope);

#endif