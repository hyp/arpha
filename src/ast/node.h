/**
* This module contains the definitions for all AST nodes.
*/
#ifndef ARPHA_AST_NODE_H
#define ARPHA_AST_NODE_H

#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/memory.h"
#include "../syntax/location.h"

#include "scope.h"

struct Variable;
struct IntegerType;
struct IntrinsicType;
struct PointerType;
struct Record;
struct Overloadset;
struct Function;
struct ImportedScope;

struct NodeVisitor;
struct Evaluator;

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE(T) \
	virtual Node* duplicate(DuplicationModifiers* mods = nullptr) const; \
	virtual Node* accept(NodeVisitor* visitor);  \
	private:             \
	virtual T* as##T();  \

#define DECLARE_TEMPNODE(T) DECLARE_NODE(T)

//This is a list of node types. TODO refactor into NODETYPE_LIST
#define NODE_LIST(X) \
	X(IntegerLiteral)    \
	X(StringLiteral) \
	X(UnitExpression)    \
	X(ErrorExpression)       \
	X(WildcardExpression)       \
	X(TypeExpression)         \
	X(ExpressionReference)   \
	X(ImportedScopeReference)   \
	X(VariableReference) \
	X(FunctionReference)     \
	X(TupleExpression)       \
	X(CallExpression)        \
	X(FieldAccessExpression)      \
	X(AccessExpression)      \
	X(AssignmentExpression)  \
	X(ReturnExpression)      \
	X(PointerOperation)      \
	X(MatchExpression)      \
	X(BlockExpression)       \
	X(WhileExpression)         \
	\
	X(ExpressionVerifier) \
	X(UnresolvedSymbol) \

//Forward declaration of node types
struct Node;
#define NODE_FORWARD_DECLARATION(X) struct X;
	NODE_LIST(NODE_FORWARD_DECLARATION)
#undef NODE_FORWARD_DECLARATION

//TODO
struct DuplicationModifiers {
	Location location;
	Scope* target;

	//The bool indicates whether the redirector is expression(true) or a definition(false)
	std::map<void*,std::pair<void*,bool> > redirectors;//Used to redirect references for duplicated definitions
	
	Variable* returnValueRedirector;//The variable to which the return value is assigned in inlined and mixined functions

	DuplicationModifiers() : returnValueRedirector(nullptr) {}
};

//An AST node
struct Node {
	Location location;
	SymbolID _label; //optional lable e.g. x:1 => 1 will have label x

	virtual TypeExpression* _returnType() const;

	//Accepts an ast visitor
	virtual Node* accept(NodeVisitor* visitor) = 0;

	virtual Node* duplicate(DuplicationModifiers* mods = nullptr) const  = 0;
	Node* copyProperties(Node* dest) const;

	virtual bool isResolved() const { return true; }

	virtual bool isConst() const { return false; }

	virtual bool isLocal() const { return false; }

	inline SymbolID label() const { return _label; }

	//Dynamic casts
#define CAST(T) virtual T* as##T() { return nullptr; }
	NODE_LIST(CAST)
#undef CAST
};

//Node to string
std::ostream& operator<< (std::ostream& stream,Node* node);

//(0..9)+ : integer
struct IntegerLiteral : Node {
	IntegerLiteral(const BigInt& integer);
	TypeExpression* _returnType() const;
	bool isConst() const;
	
	
	BigInt integer;
	TypeExpression* _type;//optional
	DECLARE_NODE(IntegerLiteral);
};

struct StringLiteral : Node {
	StringLiteral(memory::Block& block);
	TypeExpression* _returnType() const;
	bool isConst() const;
	
	
	memory::Block block;
	DECLARE_NODE(StringLiteral);
};

//():void
struct UnitExpression : Node {
	UnitExpression(){}
	TypeExpression* _returnType() const;

	DECLARE_NODE(UnitExpression);
private:
	UnitExpression(const UnitExpression& other){}
};

//_:void
struct WildcardExpression : Node {
	WildcardExpression(){}
	TypeExpression* _returnType() const;

	DECLARE_NODE(WildcardExpression);
private:
	WildcardExpression(const WildcardExpression& other){}
};

//: intrinsics::types::Expression TODO rm
struct ExpressionReference : Node {
	ExpressionReference(Node* node);
	
	TypeExpression* _returnType() const;

	Node* expression;
	DECLARE_NODE(ExpressionReference);
};

//: intrinsics::types::Scope
struct ImportedScopeReference : Node {
	ImportedScopeReference(ImportedScope* scope);

	TypeExpression* _returnType() const;

	ImportedScope* scope;
	DECLARE_NODE(ImportedScopeReference);
};

// Inferred [i.e. no type expression given] | Unresolved expression | valid type expression | wildcard type with possible constraint
struct InferredUnresolvedTypeExpression {
	enum {
		Inferred,
		Unresolved,
		Type,
		Wildcard
	};
	int kind;
	union {
		TypeExpression* _type;
		Node* unresolvedExpression;
	};

	inline InferredUnresolvedTypeExpression() : kind(Inferred) {}
	inline InferredUnresolvedTypeExpression(TypeExpression* expr) : kind(Type),_type(expr) {}
	inline bool isResolved() const { return kind == Type; }
	TypeExpression* type();

	InferredUnresolvedTypeExpression duplicate(DuplicationModifiers* mods);

	void infer(TypeExpression* type);
	bool resolve(Evaluator* evaluator);
	void parse(Parser* parser,int stickiness);

	inline bool isInferred(){ return kind == Inferred; }
	inline bool isWildcard(){ return kind == Wildcard; }
};

//(type ...): intrinsics::types::Type
struct TypeExpression : Node {

	enum {
		RECORD,
		INTEGER,
		INTRINSIC,
		FUNCTION,
		POINTER,
	};

	TypeExpression(IntrinsicType* intrinsic);
	TypeExpression(IntegerType* integer);
	TypeExpression(Record* record);
	TypeExpression(PointerType* pointer,TypeExpression* next);
	TypeExpression(TypeExpression* argument,TypeExpression* returns);//function

	bool isResolved() const;
	TypeExpression* _returnType() const;
	size_t size() const;

	bool isSame(TypeExpression* other);

	bool hasLocalSemantics() const { return _localSemantics; }

	/**
	* This is the one of the key functions of the type system.
	* Given an expression and its type, this function will check if the 'this' type can be assigned from expression's type.
	* If such an assignment is possible, it will return the resulting expression with possible conversions.
	* If not, it will return null.
	*/
	Node* assignableFrom(Node* expression,TypeExpression* type);
	int canAssignFrom(Node* expression,TypeExpression* type);

	DECLARE_NODE(TypeExpression);
public:
	int type;
	bool _localSemantics;
	union {
		IntrinsicType* intrinsic;
		Record* record;
		IntegerType* integer;
		TypeExpression* argument;
	};
	TypeExpression* returns;
	friend std::ostream& operator<< (std::ostream& stream,TypeExpression* node);
};
std::ostream& operator<< (std::ostream& stream,TypeExpression* node);

// Type checks the expression, returning an expression which fits the expectedType or null if the types don't match
Node* typecheck(Location& loc,Node* expression,TypeExpression* expectedType);

//: variable->type
struct VariableReference : Node {
	VariableReference(Variable* variable);

	TypeExpression* _returnType() const;
	bool isResolved() const;
	bool isLocal() const;

	Variable* variable;
	DECLARE_NODE(VariableReference);
};

//: record
struct TupleExpression : Node {
	TupleExpression();
	TupleExpression(Node* a,Node* b);

	TypeExpression* _returnType() const;
	bool isResolved() const;

	std::vector<Node*> children;
	TypeExpression* type; // = nullptr

	DECLARE_NODE(TupleExpression);
};

struct FunctionReference : Node {
	FunctionReference(Function* func);

	TypeExpression* _returnType() const;
	bool isResolved() const;

	Function* function;
	DECLARE_NODE(FunctionReference);
};

struct CallExpression : Node {
	CallExpression(Node* object,Node* argument);

	TypeExpression* _returnType() const;
	bool isResolved() const;

	Node* object;
	Node* arg;
	bool _resolved;
	DECLARE_NODE(CallExpression);
};

// Record.field
// Pointer(Record).field
struct FieldAccessExpression : Node {
	FieldAccessExpression(Node* object,int field);

	TypeExpression* _returnType() const;

	// Returns record T when object is of type T or pointer T 
	Record* objectsRecord() const;
	bool isLocal() const;

	Node* object;
	int field;
	DECLARE_NODE(FieldAccessExpression);
};

struct AssignmentExpression : Node {
	AssignmentExpression(Node* object,Node* value);

	TypeExpression* _returnType() const;
	bool isResolved() const;

	Node* object;
	Node* value;
	bool isInitializingAssignment;// = false
	bool _resolved;
	DECLARE_NODE(AssignmentExpression);
};

// : intrinsics::types::Void
struct ReturnExpression : Node {
	ReturnExpression(Node* expression);

	bool isResolved() const;

	Node* value;
	bool _resolved;
	DECLARE_NODE(ReturnExpression);
};

//A unary expression, which is either an adress or a dereference
//: Pointer(expression.typeof)
struct PointerOperation : Node {
	PointerOperation(Node* expression,int type);
	TypeExpression* _returnType() const;
	bool isResolved() const;

	enum {
		ADDRESS,     //&
		DEREFERENCE, //*
	};	
	Node* expression;
	int kind; // = ADDRESS
	DECLARE_NODE(PointerOperation);
};

struct MatchExpression : Node {
	MatchExpression(Node* object);

	TypeExpression* _returnType() const;
	bool isResolved() const;

	Node* object;
	struct Case {	
		Node* pattern;
		Node* consequence;
		bool fallThrough; // = false

		Case(Node* pat,Node* consq,bool fallthrough) : pattern(pat),consequence(consq),fallThrough(fallthrough) {}
	};
	std::vector<Case> cases;
	DECLARE_NODE(MatchExpression);
};

// : intrinsics::types::Void
struct BlockExpression : Node {
	BlockExpression(Scope* scope);

	bool isResolved() const;//TODO true or false?

	void _duplicate(BlockExpression* dest,DuplicationModifiers* mods) const;

	std::vector<Node*> children;
	Scope* scope;
	bool _resolved;
	DECLARE_NODE(BlockExpression);
};

// : intrinsics::types::Void
// Status - syntax[90%],evaluation[100%],optimization[0%],generation[0%]
struct WhileExpression : Node {
	WhileExpression(Node* condition,Node* body);

	bool isResolved() const;

	Node* condition;//expectedType = intrinsics::types::boolean
	Node* body;
	DECLARE_NODE(WhileExpression);
};

/*****
* These nodes are temporary utility nodes which are resolved into proper nodes.
*****/

struct AlwaysUnresolved : Node {
	bool isResolved() const { return false; }
};

// Used for typetesting - verifies that a given expression is compatible to a certain type
struct ExpressionVerifier : AlwaysUnresolved {
	ExpressionVerifier(const Location& loc,Node* child,TypeExpression* typeExpected);

	Node* expression;
	TypeExpression* expectedType;
	DECLARE_TEMPNODE(ExpressionVerifier);
};

// An unresolved symbol
struct UnresolvedSymbol :AlwaysUnresolved {
	UnresolvedSymbol(const Location& loc,SymbolID sym,Scope* scope = nullptr);

	//Scope in which to look for resolving. 
	//Leave it null to search in the current scope.
	//NB: This doesn't have to be the scope that the expression was created in, so don't use it as a current scope indicator!
	Scope* explicitLookupScope; // = nullptr
	SymbolID symbol;
	DECLARE_TEMPNODE(UnresolvedSymbol);
};

// An expression representing a symbolic query to an object in a form of object '.' symbol
// Resolved into a call when a suitable matching function exists
// Otherwise resolved into field access expression when querying a field
// N.B. Don't try to resolve this at first time, because a.x(..) and a.x = .. need to be resolved from their respecitve parents
struct AccessExpression : AlwaysUnresolved {
	AccessExpression(Node* object,SymbolID symbol);
	
	Node* object;
	SymbolID symbol;
	bool passedFirstEval; //On first evaluation don't touch this node!!
	DECLARE_TEMPNODE(AccessExpression);
};

// A dummy expression representing an error and never resolving
// N.B. use getInstance instead of new for creation!
struct ErrorExpression : AlwaysUnresolved {
	static ErrorExpression* getInstance(); //avoid multiple creations

	DECLARE_TEMPNODE(ErrorExpression);
private:
	ErrorExpression(){}
	ErrorExpression(const ErrorExpression& other){}
};

#endif