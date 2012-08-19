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
	Node* _prevNode;
	size_t unresolvedExpressions;

public:
	int   _pass;
	bool _reportUnresolved;
	bool treatUnresolvedTypesAsResolved; //Used to resolve circular type definitions or type definitions which depend on self
	bool isRHS; // = false
	Trait* currentTrait;       // Required for the trait prerequisuite.
	Function* currentFunction; // The function we are currently resolving. Can be null.
	data::ast::VisibilityMode currentVisibilityMode;
	std::vector<ScopedCommand*> whereStack;

	//Varios stages.
	inline bool isReportingUnresolvedNodes() { return _reportUnresolved; }
 

	// Sometimes we know that we want an expression of certain type at a given place e.g. var x Foo <- we expect Foo to be TypeExpression
	// This knowledge can be used to resolve certain ambiguties: 
	//		e.g. int32,int32 will become a anonymous record type of 2 ints rather than a record containing two types!
	Type* expectedTypeForEvaluatedExpression; // = nullptr

	Resolver(CompilationUnit* compilationUnit);

	CompilationUnit* compilationUnit() const { return _compilationUnit; }

	void applyCurrentVisibilityMode(DefinitionNode* node);
	void possiblyApplyWhere(Argument* argument);


	Node* resolve(Node* node);
	Node* resolve(Node* node,Node* previous);

	Node* multipassResolve(Node* node,bool quasi = false);

	// Resolves expressions and definitions in a module using multiple passes
	void resolveModule(BlockExpression* module);

	//Attempt to resolve macroes when they are defined, so that we can use them straight away
	Node* resolveMacroAtParseStage(Node* macro);

	//Function-like macro
	Node* executeAndMixinMacro(Function* function,Node* arg);

	Node* inlineCall(Function* function,Node* parameters);

	inline Scope* currentScope() const        { return _currentScope;  }
	inline void currentScope(Scope* scope)    { _currentScope = scope; }

	void makeDeclarationVisible(PrefixDefinition* node);

	inline void currentParentNode(Node* node) { _currentParent = node; }
	inline Node* currentParentNode() const    { return _currentParent; }

	inline Node* previousNode() const { return _prevNode; }

	inline void markResolved(Node* node){ node->setFlag(Node::RESOLVED); }

	//function overloads resolving


	
	
	Function* specializeFunction(TypePatternUnresolvedExpression::PatternMatcher& patternMatcher,Function* original,Type** specializedParameters,Node** passedExpressions);
	bool resolveSpecialization(Function* function);
	Node* constructFittingArgument(Function** function,Node *arg,bool dependentChecker = false,int* weight = nullptr);

	Function* resolveFunctionCall(Scope* scope,SymbolID function,Node** parameter,bool dotSyntax = false,bool reportMultipleOverloads = false);



private:
	void onFirstMultipleOverload(Function* function,int distance,Node* arg);
	void onMultipleOverload(Function* function);

	//Unresolved node error reporting
	void reportUnresolved(UnresolvedSymbol* node);
	void reportUnresolved(CallExpression* node);
	void reportUnresolved(Function* node);
	Node* reportUnresolvedNode(Node* node);

	void reportUnresolvedNodes(Node* root);
};

namespace overloads {
/**
  This range iterates over all the possible overloads(as visible from the given scope) for the given function.
*/
struct OverloadRange {

	OverloadRange(Scope* scope,SymbolID function,bool dotSyntax);
private:
	Function** funcCurr;
	Function** funcEnd;
	int distance;
	Scope*  scope;
	Scope*  prevScope;
	Scope*  adjacentScope;
	Scope** importsCurr; //import iterators
	Scope** importsEnd;
	SymbolID functionName;
	bool    dotSyntax;
	
	void nextScope();
	void getNextFuncIterators();
public:

	inline bool isEmpty(){ return scope == nullptr; }
	inline Function* currentFunction(){ return *funcCurr; }
	void   advance();
	inline int currentDistance(){ return distance; }

};

}

Node* mixinMacro(CTFEinvocation* invocation,Scope* scope);

#endif