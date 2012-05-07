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
struct Record;
struct Overloadset;
struct Function;
struct ImportedScope;

struct NodeVisitor;
struct Evaluator;

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE(T) \
	virtual Node* accept(NodeVisitor* visitor);  \
	private:             \
	virtual T* as##T();  \

//This is a list of node types. TODO refactor into NODETYPE_LIST
#define NODE_LIST(X) \
	X(IntegerLiteral)    \
	X(UnitExpression)    \
	X(ErrorExpression)       \
	X(WildcardExpression)       \
	X(TypeExpression)         \
	X(ExpressionReference)   \
	X(ImportedScopeReference)   \
	X(VariableReference) \
	X(FunctionReference)     \
	X(TupleExpression)       \
	X(OverloadSetExpression) \
	X(CallExpression)        \
	X(FieldAccessExpression)      \
	X(AccessExpression)      \
	X(AssignmentExpression)  \
	X(ReturnExpression)      \
	X(PointerOperation)      \
	X(MatchExpression)      \
	X(BlockExpression)       \
	X(WhileExpression)          

//Forward declaration of node types
struct Node;
#define NODE_FORWARD_DECLARATION(X) struct X;
	NODE_LIST(NODE_FORWARD_DECLARATION)
#undef NODE_FORWARD_DECLARATION

//An AST node
struct Node {
	Location location;

	virtual TypeExpression* _returnType() const;

	//Accepts an ast visitor
	virtual Node* accept(NodeVisitor* visitor) = 0;

	virtual Node* duplicate() const { return 0; } //TODO = 0

	virtual bool isConst() const { return false; }

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
	Node* duplicate() const;
	bool isConst() const;
	
	BigInt integer;
	TypeExpression* _type;//optional
	DECLARE_NODE(IntegerLiteral);
};

//(error):unresolved
struct ErrorExpression : Node {
	TypeExpression* _returnType() const;

	Node* duplicate() const;
	static ErrorExpression* getInstance(); //avoid multiple creations

	DECLARE_NODE(ErrorExpression);
private:
	ErrorExpression(){}
	ErrorExpression(const ErrorExpression& other){}
};

//():void
struct UnitExpression : Node {
	TypeExpression* _returnType() const;
	
	Node* duplicate() const;
	static UnitExpression* getInstance(); //avoid multiple creations

	DECLARE_NODE(UnitExpression);
private:
	UnitExpression(){}
	UnitExpression(const UnitExpression& other){}
};

//_:void
struct WildcardExpression : Node {
	TypeExpression* _returnType() const;
	
	Node* duplicate() const;
	static WildcardExpression* getInstance(); //avoid multiple creations

	DECLARE_NODE(WildcardExpression);
private:
	WildcardExpression(){}
	WildcardExpression(const WildcardExpression& other){}
};

//: intrinsics::types::Expression
struct ExpressionReference : Node {
	ExpressionReference(Node* node);
	
	TypeExpression* _returnType() const;
	Node* duplicate() const;

	Node* expression;
	DECLARE_NODE(ExpressionReference);
};

//: intrinsics::types::Scope
struct ImportedScopeReference : Node {
	ImportedScopeReference(ImportedScope* scope);

	TypeExpression* _returnType() const;
	Node* duplicate() const;

	ImportedScope* scope;
	DECLARE_NODE(ImportedScopeReference);
};

// Inferred [i.e. no type expression given] | Unresolved expression | valid type expression
struct InferredUnresolvedTypeExpression {
	enum {
		Inferred,
		Unresolved,
		Type
	};
	int kind;
	union {
		TypeExpression* _type;
		Node* unresolvedExpression;
	};

	inline InferredUnresolvedTypeExpression() : kind(Inferred) {}
	inline InferredUnresolvedTypeExpression(TypeExpression* expr) : kind(Type),_type(expr) {}
	inline bool resolved(){ return kind == Type; }
	TypeExpression* type();

	void infer(TypeExpression* type);
	bool resolve(Evaluator* evaluator);
	void parse(Parser* parser,int stickiness);

	inline bool isInferred(){ return kind == Inferred; }
};

//(type ...): intrinsics::types::Type | intrinsics::types::Unresolved
struct TypeExpression : Node {

	enum {
		RECORD,
		INTEGER,
		INTRINSIC,
		FUNCTION,
	};

	TypeExpression(IntrinsicType* intrinsic);
	TypeExpression(IntegerType* integer);
	TypeExpression(Record* record);

	bool resolved() const;
	TypeExpression* _returnType() const;
	Node* duplicate() const;
	size_t size() const;

	bool isSame(TypeExpression* other);

	/**
	* This is the one of the key functions of the type system.
	* Given an expression and its type, this function will check if the 'this' type can be assigned from expression's type.
	* If such an assignment is possible, it will return the resulting expression with possible conversions.
	* If not, it will return null.
	*/
	Node* assignableFrom(Node* expression,TypeExpression* type);

	DECLARE_NODE(TypeExpression);
public:
	int type;
	union {
		IntrinsicType* intrinsic;
		Record* record;
		IntegerType* integer;
	};
	friend std::ostream& operator<< (std::ostream& stream,TypeExpression* node);
};
std::ostream& operator<< (std::ostream& stream,TypeExpression* node);

//: variable->type
struct VariableReference : Node {
	VariableReference(Variable* variable);

	TypeExpression* _returnType() const;
	Node* duplicate() const;

	Variable* variable;
	DECLARE_NODE(VariableReference);
};

//: record
struct TupleExpression : Node {
	TupleExpression();
	TupleExpression(Node* a,Node* b);

	TypeExpression* _returnType() const;
	Node* duplicate() const;

	std::vector<Node*> children;
	TypeExpression* type;

	DECLARE_NODE(TupleExpression);
};

struct FunctionReference : Node {
	FunctionReference(Function* function);

	TypeExpression* _returnType() const;
	Function* function() const;

	Function* _function;
	DECLARE_NODE(FunctionReference);
};

//: intrinsics::types::Unresolved
struct OverloadSetExpression : Node {
	static OverloadSetExpression* create(SymbolID symbol,Scope* scope);

	//Scope in which to look for resolving. 
	//NB: This doesn't have to be the scope that the expression was created in, so don't use it as a current scope indicator!
	Scope* scope;
	SymbolID symbol;
	DECLARE_NODE(OverloadSetExpression);
};

struct CallExpression : Node {
	static CallExpression* create(Node* object,Node* argument);

	TypeExpression* _returnType() const;

	Node* object;
	Node* arg;
	DECLARE_NODE(CallExpression);
};

struct FieldAccessExpression : Node {
	FieldAccessExpression(Node* object,int field);

	TypeExpression* _returnType() const;
	Node* duplicate() const;

	Node* object;
	int field;
	DECLARE_NODE(FieldAccessExpression);
};

struct AccessExpression : Node {
	AccessExpression(Node* object,SymbolID symbol);

	TypeExpression* _returnType() const;
	Node* duplicate() const;
	
	Node* object;
	SymbolID symbol;
	bool passedFirstEval; //On first evaluation don't touch this node!!
	DECLARE_NODE(AccessExpression);
};

struct AssignmentExpression : Node {
	AssignmentExpression(Node* object,Node* value);

	TypeExpression* _returnType() const;
	Node* duplicate() const;

	Node* object;
	Node* value;
	bool isInitializingAssignment;// = false
	DECLARE_NODE(AssignmentExpression);
};

// : intrinsics::types::Void
struct ReturnExpression : Node {
	ReturnExpression(Node* expression);
	Node* duplicate() const;

	Node* value; //can be null
	DECLARE_NODE(ReturnExpression);
};

//A unary expression, which is either an adress or a dereference
//: Pointer(expression.typeof)
struct PointerOperation : Node {
	PointerOperation(Node* expression,int type);
	TypeExpression* _returnType() const;
	Node* duplicate() const;

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
	Node* duplicate();

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
	Node* duplicate() const;

	std::vector<Node*> children;
	Scope* scope;
	DECLARE_NODE(BlockExpression);
};

// : intrinsics::types::Void
struct WhileExpression : Node {
	WhileExpression(Node* condition,Node* body);

	Node* duplicate() const;

	Node* condition;
	Node* body;
	DECLARE_NODE(WhileExpression);
};

#endif