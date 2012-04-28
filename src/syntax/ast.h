/**
* This module contains the definitions for all AST nodes.
*/
#ifndef ARPHA_AST_H
#define ARPHA_AST_H

#include "../base/base.h"
#include "../base/location.h"
#include "../base/memory.h"

#include "../scope.h"
#include "../declarations.h"

struct NodeVisitor;

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE(T) \
	virtual Node* accept(NodeVisitor* visitor);  \
	private:             \
	virtual T* as##T();  \

//This is a list of node types. TODO refactor into NODETYPE_LIST
#define NODE_LIST(X) \
	X(ConstantExpression)    \
	X(VariableExpression)    \
	X(TupleExpression)       \
	X(OverloadSetExpression) \
	X(CallExpression)        \
	X(AccessExpression)      \
	X(AssignmentExpression)  \
	X(ReturnExpression)      \
	X(IfExpression)          \
	X(BlockExpression)       

//Forward declaration of node types
struct Node;
#define NODE_FORWARD_DECLARATION(X) struct X;
	NODE_LIST(NODE_FORWARD_DECLARATION)
#undef NODE_FORWARD_DECLARATION

//An AST node
struct Node {
	Location location;

	//Returns expressions return type
	virtual Type* returnType() const;

	//Accepts an ast visitor
	virtual Node* accept(NodeVisitor* visitor) = 0;

	//Dynamic casts
#define CAST(T) virtual T* as##T() { return nullptr; }
	NODE_LIST(CAST)
#undef CAST
};

//Node to string
std::ostream& operator<< (std::ostream& stream,Node* node);

struct ConstantExpression : Node {
	//constructors
	static ConstantExpression* create(Type* constantType);
	static ConstantExpression* createScopeReference(Scope* scope);
	static ConstantExpression* createFunctionReference(Function* func);
	static ConstantExpression* createTypeReference(Type* type);

	Type* returnType() const;

	//
	union {
		int64   i64;
		uint64  u64;
		double  f64;
		Scope*  refScope;
		Type*   refType;
		Function* refFunction;
		memory::Block string;
	};
	Type* type;
	bool _isLiteral;

	inline const bool isLiteral() const { return _isLiteral; }

	DECLARE_NODE(ConstantExpression);

};

struct VariableExpression : Node {
	static VariableExpression* create(Variable* variable);

	Type* returnType() const;

	Variable* variable;
	DECLARE_NODE(VariableExpression);
};

struct TupleExpression : Node {
	static TupleExpression* create();
	static TupleExpression* create(Node* a,Node* b);

	Type* returnType() const;

	std::vector<Node*> children;
	Type* type;

	DECLARE_NODE(TupleExpression);
};

struct OverloadSetExpression : Node {
	static OverloadSetExpression* create(SymbolID symbol,Scope* scope);

	Scope* scope;
	SymbolID symbol;
	DECLARE_NODE(OverloadSetExpression);
};

struct CallExpression : Node {
	static CallExpression* create(Node* object,Node* argument);

	Type* returnType() const;

	Node* object;
	Node* arg;
	DECLARE_NODE(CallExpression);
};

struct AccessExpression : Node {
	static AccessExpression* create(Node* object,SymbolID symbol,Scope* scope);
	
	Node* object;
	SymbolID symbol;
	Scope* scope;
	DECLARE_NODE(AccessExpression);
};

struct AssignmentExpression : Node {
	static AssignmentExpression* create(Node* object,Node* value);

	Type* returnType() const;

	Node* object;
	Node* value;
	DECLARE_NODE(AssignmentExpression);
};

struct ReturnExpression : Node {
	static ReturnExpression* create(Node* expression);

	Node* value;
	//Scope* currentBlock;
	DECLARE_NODE(ReturnExpression);
};

struct IfExpression : Node {
	static IfExpression* create(Node* condition,Node* consequence,Node* alternative);

	Type* returnType() const;

	Node* condition;
	Node* consequence;
	Node* alternative;
	DECLARE_NODE(IfExpression);
};

struct BlockExpression : Node {
	static BlockExpression* create();

	std::vector<Node*> children;
	DECLARE_NODE(BlockExpression);
};


#endif