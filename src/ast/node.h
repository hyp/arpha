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
struct Node;
struct NodeList;
struct TypePatternUnresolvedExpression;
struct Type;
struct CTFEinvocation;
struct Resolver;
struct Argument;



//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE(T) \
	Node* duplicate(DuplicationModifiers* mods) const; \
	Node* accept(NodeVisitor* visitor);    \
	private:             \
	T* as##T();  \

#define DECLARE_TEMPNODE(T) DECLARE_NODE(T)

//This is a list of node types. TODO refactor into NODETYPE_LIST
#define NODE_LIST(X)  \
	X(IntegerLiteral) \
	X(FloatingPointLiteral) \
	X(BoolExpression) \
	X(StringLiteral)  \
	X(ArrayLiteral)   \
	X(UnitExpression) \
	X(ErrorExpression)        \
	X(TypeReference)          \
	X(ImportedScopeReference) \
	X(VariableReference)     \
	X(FunctionReference)     \
	X(TupleExpression)       \
	X(CallExpression)        \
	X(UnaryOperation) \
	X(BinaryOperation) \
	X(FieldAccessExpression) \
	X(AccessExpression)      \
	X(AssignmentExpression)  \
	X(ReturnExpression)      \
	X(ControlFlowExpression) \
	X(PointerOperation)  \
	X(IfExpression)      \
	X(BlockExpression)   \
	X(LoopExpression)    \
	X(CastExpression)    \
	\
	X(ExpressionVerifier) \
	X(UnresolvedSymbol)   \
	X(MatchResolver)      \
	X(NodeReference)      \
	\
	X(InfixMacro) \
	X(PrefixMacro) \
	X(Variable) \
	X(Function) \
	X(Record)   \
	X(TypeDeclaration)

//Forward declaration of node types
#define NODE_FORWARD_DECLARATION(X) struct X;
	NODE_LIST(NODE_FORWARD_DECLARATION)
#undef NODE_FORWARD_DECLARATION

//TODO
struct DuplicationModifiers {
	Location location;
	Scope* target;
	CTFEinvocation* expandedMacroOptimization;//when a macro returns [> $x <] we replace x with a value during mixining into the caller's body

	//The bool indicates whether the redirector is expression(true) or a definition(false)
	std::map<void*,std::pair<void*,bool> > redirectors;//Used to redirect references for duplicated definitions
	
	Variable* returnValueRedirector;//The variable to which the return value is assigned in inlined and mixined functions

	DuplicationModifiers(Scope* target) : returnValueRedirector(nullptr),expandedMacroOptimization(nullptr) { this->target = target; }

	void expandArgument(Argument* original,Node* value);
	void duplicateDefinition(Argument* original,Argument* duplicate);
	void duplicateDefinition(Variable* original,Variable* duplicate);
	void duplicateDefinition(Function* original,Function* duplicate);
	void duplicateDefinition(Record* original,Record* duplicate);
	void duplicateDefinition(TypeDeclaration* original,TypeDeclaration* duplicate);
	void duplicateDefinition(PrefixMacro* original,PrefixMacro* duplicate);
	void duplicateDefinition(InfixMacro* original,InfixMacro* duplicate);
};

//An AST node
struct Node {
protected:	
	SymbolID _label; //optional label e.g. x:1 => 1 will have label x
public:
	Location _location;
protected:	
	uint16   flags;

	friend struct Parser;
	
public:
	enum {
		RESOLVED = 0x1,
		CONSTANT = 0x2, //marks a constant expression
	};

	inline Node() : flags(0) {}

	void setFlag  (uint16 id);
	bool isFlagSet(uint16 id) const ;
	inline bool isResolved() const { return isFlagSet(RESOLVED); }
	inline bool isConst()    const { return isFlagSet(CONSTANT); }

	virtual Type* returnType() const;

	//Accepts an AST visitor
	virtual Node* accept(NodeVisitor* visitor) = 0;

	virtual Node* duplicate(DuplicationModifiers* mods) const  = 0;
protected:
	Node* copyProperties(Node* dest) const; 
public:
	Node* copyLocationSymbol(Node* dest) const;//Doesn't copy the flags.. use when resolving between nodes of different types

	virtual Node* resolve(Resolver* resolver);

	inline  SymbolID label()   const { return _label; }
    Location location() const { return _location; }
	virtual bool applyProperty(SymbolID name,Node* value){ return false; }

	//Dynamic casts
	virtual bool isDefinitionNode(){ return false; }
#define CAST(T) virtual T* as##T() { return nullptr; }
	NODE_LIST(CAST)
#undef CAST
};
std::ostream& operator<< (std::ostream& stream,Node* node);

/**
* These nodes are definition nodes.
* A definition node provides a mapping from a symbol in the source file into an custom expression for both parser and resolver.
*/
struct DefinitionNode : Node { 

	uint8 visibilityMode() const;
	bool  isPublic() const;
//protected:
	void visibilityMode(uint8 mode);

	void* generatorData; //extra data used by generator
private:
	bool isDefinitionNode(){ return true; }
	enum { IS_PRIVATE = 0x2 };//NB: it overrides the const flag, which is irrelevant for declarations
};

struct PrefixDefinition : DefinitionNode {

	PrefixDefinition(SymbolID name,Location& location);
	virtual Node* parse(Parser* parser) = 0;

	virtual Overloadset* asOverloadset(){ return nullptr; }

	//For resolving symbols after parsing
	virtual Node* createReference(){ return nullptr; }
};

struct InfixDefinition : DefinitionNode {
	int stickiness;//The parsing precedence
	
	InfixDefinition(SymbolID name,int stickiness,Location& location);
	virtual Node* parse(Parser* parser,Node* node) = 0;
};

#include "type.h"

struct LiteralNode: Node {
	inline LiteralNode() { setFlag(CONSTANT | RESOLVED); }
};

//(0..9)+ : integer
struct IntegerLiteral : LiteralNode {
	IntegerLiteral(const BigInt& integer);
	Type* returnType() const;	
	
	BigInt integer;
	IntegerType* _type;//optional
	DECLARE_NODE(IntegerLiteral);
};

//Either a float or a double
struct FloatingPointLiteral : LiteralNode {
	enum {
		IS_FLOAT = 0x8,//is it a float(or a double)
	};

	FloatingPointLiteral(const double v);
	Type* returnType() const;

	double value;
	DECLARE_NODE(FloatingPointLiteral);
};

//true | false
struct BoolExpression: LiteralNode {
	BoolExpression(const bool v);
	Type* returnType() const;

	bool value;
	DECLARE_NODE(BoolExpression);
};

struct StringLiteral : LiteralNode {
	StringLiteral(memory::Block& block);
	StringLiteral(SymbolID symbol);//<-- reuses the string, no duplication
	Type* returnType() const;
	
	memory::Block block;
	DECLARE_NODE(StringLiteral);
};

struct NodeList : Node {
	std::vector<Node*> children;

	inline std::vector<Node*>::iterator begin(){ return children.begin(); }
	inline std::vector<Node*>::const_iterator begin() const { return children.begin(); }
	inline std::vector<Node*>::iterator end(){ return children.end(); }
	inline std::vector<Node*>::const_iterator end() const { return children.end(); }
	inline size_t size() const  { return children.size(); }
	inline Node** childrenPtr() { return &children[0];    }
	inline void addChild(Node* child) { children.push_back(child); }
	Node* duplicateChildren(NodeList* dest,DuplicationModifiers* mods) const ;
};

struct ArrayLiteral : NodeList {
	ArrayLiteral();
	
	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	DECLARE_NODE(ArrayLiteral);
};


//():void
struct UnitExpression : LiteralNode {
	UnitExpression(){}
	Type* returnType() const;

	DECLARE_NODE(UnitExpression);
private:
	UnitExpression(const UnitExpression& other){}
};


//: intrinsics::types::Scope
struct ImportedScopeReference : LiteralNode {
	ImportedScopeReference(ImportedScope* scope);

	Type* returnType() const;

	ImportedScope* scope;
	DECLARE_NODE(ImportedScopeReference);
};

struct NodeReference : LiteralNode {
	NodeReference(Node* node);
        
	Type* returnType() const;
    Node* node() const { return _node; }
      
	DECLARE_NODE(NodeReference);
private:
    Node* _node;
};

// Type checks the expression, returning an expression which fits the expectedType or null if the types don't match
Node* typecheck(Node* expression,Type* expectedType);

struct TypeReference : Node {
	TypeReference(Type* type);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Type* type;
	DECLARE_NODE(TypeReference);
};

//: variable->type
struct VariableReference : Node {
	VariableReference(Variable* variable);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Variable* variable;
	DECLARE_NODE(VariableReference);
};

//: record
struct TupleExpression : NodeList {
	TupleExpression();
	TupleExpression(Node* a,Node* b);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Type* type; // = nullptr

	DECLARE_NODE(TupleExpression);
};

struct FunctionReference : Node {
	FunctionReference(Function* func);

	Type* returnType() const;

	Function* function;
	DECLARE_NODE(FunctionReference);
};

struct CallExpression : Node {
	enum {
		DOT_SYNTAX = 0x4, //a call expression created from foo.bar
	};
	CallExpression(Node* object,Node* argument);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Node* object;
	Node* arg;
	DECLARE_NODE(CallExpression);
};

//this is an expression which has multiple variants
struct VariantNode : Node {
	VariantNode(uint32 kind);
	uint32 kind() const;
protected:
	void mutate(uint32 newKind);
private:
	uint32 _kind;
};

struct UnaryOperation : VariantNode {
	enum {
		BOOL_NOT = 0, // ! true
		MINUS, // - 
		MAX_VARIANT,
	};
	UnaryOperation(uint32 kind,Node* expression);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	bool  isValid() ;
	Node* interpret() const;

	Node* expression;
	DECLARE_NODE(UnaryOperation);
};

struct BinaryOperation : VariantNode {
	enum {
		BOOL_AND = 0,//true && true
		BOOL_OR,     //false || false
		EQUALS,
		LESS,
		GREATER,
		ADD,
		SUBTRACT,
		MULTIPLY,
		DIVIDE,
		MOD,
		MAX_VARIANT,
	};
	BinaryOperation(uint32 kind,Node* a,Node* b);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	bool  isValid() ;
	Node* interpret() const;

	Node* a,*b;
	DECLARE_NODE(BinaryOperation);
};

// Record.field
// Pointer(Record).field
struct FieldAccessExpression : Node {
	FieldAccessExpression(Node* object,int field);

	Type* returnType() const;

	// Returns record T when object is of type T or pointer T 
	Record* objectsRecord() const;

	Node* object;
	int field;
	DECLARE_NODE(FieldAccessExpression);
};

/// NB: assignment(make exception for the initializing assignment) is a statement(Hello nemerle)
struct AssignmentExpression : Node {
	AssignmentExpression(Node* object,Node* value);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Node* object;
	Node* value;
	bool isInitializingAssignment;// = false
	DECLARE_NODE(AssignmentExpression);
};

// : intrinsics::types::Void
struct ReturnExpression : Node {
	ReturnExpression(Node* expression);

	Node* resolve(Resolver* resolver);

	Node* expression;
	DECLARE_NODE(ReturnExpression);
};

struct ControlFlowExpression: Node {
	enum {
		CONTINUE,BREAK,FALLTHROUGH
	};
	ControlFlowExpression(int type);

	inline bool isContinue() const { return kind == CONTINUE; }
	inline bool isBreak() const { return kind == BREAK; }
	inline bool isFallthrough() const { return kind == FALLTHROUGH; }
	SymbolID labeledJump;//For future
	int kind;
	DECLARE_NODE(ControlFlowExpression);
};

//A unary expression, which is either an adress or a dereference
//: Pointer(expression.typeof)
struct PointerOperation : Node {
	PointerOperation(Node* expression,int type);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	enum {
		ADDRESS,     //&
		DEREFERENCE, //*
	};	
	inline bool isAddress()     const { return kind == ADDRESS; }
	inline bool isDereference() const { return kind == DEREFERENCE; }
	Node* expression;
	int kind; // = ADDRESS
	DECLARE_NODE(PointerOperation);
};

struct IfExpression : Node {
	enum {
		MATCH_SYNTAX = 0x4, //Produced by a match expression
	};
	IfExpression(Node* condition,Node* consequence,Node* alternative);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Node* condition;
	Node* consequence;
	Node* alternative;
	DECLARE_NODE(IfExpression);
};

// : intrinsics::types::Void
struct BlockExpression : NodeList {
	enum {
		RETURNS_LAST_EXPRESSION = 0x8,
		USES_PARENT_SCOPE = 0x10 ,//mixined blocks [> <] use parent scope to define definitons and they return the last expression
	};
    BlockExpression();

	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	void _duplicate(BlockExpression* dest,DuplicationModifiers* mods) const;

	BlockExpression* duplicateMixin(DuplicationModifiers* mods) const;

	Scope* scope;
	DECLARE_NODE(BlockExpression);
private:
	BlockExpression(Scope* scope);
};

// A while or a do while expression
// : intrinsics::types::Void
struct LoopExpression : Node {
	LoopExpression(Node* body);

	Node* resolve(Resolver* resolver);

	Node* body;
	DECLARE_NODE(LoopExpression);
};

struct CastExpression : Node {
	CastExpression(Node* object,Type* type);

	Node* resolve(Resolver* resolver);
	Type* returnType() const;

	Node* object;
	Type* type;
	DECLARE_NODE(CastExpression);
};

/*****
* These nodes are temporary utility nodes which are resolved into proper nodes.
*****/

struct AlwaysUnresolved : Node {
};

// Used for typetesting - verifies that a given expression is compatible to a certain type
struct ExpressionVerifier : AlwaysUnresolved {
	ExpressionVerifier(const Location& loc,Node* child,Type* typeExpected);

	Node* expression;
	Type* expectedType;
	DECLARE_TEMPNODE(ExpressionVerifier);
};

// An unresolved symbol
struct UnresolvedSymbol :AlwaysUnresolved {
	UnresolvedSymbol(const Location& loc,SymbolID sym,Scope* scope = nullptr);

	Node* resolve(Resolver* resolver);

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
	
	Node* resolve(Resolver* resolver);

	Node* object;
	SymbolID symbol;
	DECLARE_TEMPNODE(AccessExpression);
};

// An expression representing pattern matching of a certain object
// A temporary match placeholder must be used before if-else tree so that the pattern for certain object like Types or records will be produced correctly!
struct MatchResolver : NodeList {
	MatchResolver(Node* object);

	Node* resolve(Resolver* resolver);

	Node* object;
	DECLARE_TEMPNODE(MatchResolver);
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

//Macroes
struct PrefixMacro: PrefixDefinition {

	PrefixMacro(Function* f);

	Node* parse(Parser* parser);

	Node* resolve(Resolver* resolver);

	inline Function* func(){ return function; }

	DECLARE_NODE(PrefixMacro);
protected:
	Function* function;
};

struct InfixMacro: InfixDefinition {
	InfixMacro(Function* f,Node* stickiness);
	bool applyProperty(SymbolID name,Node* value);

	Node* parse(Parser* parser,Node* node);

	Node* resolve(Resolver* resolver);

	inline Function* func(){ return function; }

	DECLARE_NODE(InfixMacro);
protected:
	Function* function;
	Node* stickinessExpression;
};

#endif
