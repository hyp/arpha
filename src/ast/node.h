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
struct Optimizer;

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE(T) \
	Node* duplicate(DuplicationModifiers* mods) const; \
	Node* accept(NodeVisitor* visitor);    \
	private:             \
	void  dumpImplementation(Dumper& dumper) const; \
	T* as##T();  \

#define DECLARE_TEMPNODE(T) DECLARE_NODE(T)

//This is a list of node types. TODO refactor into NODETYPE_LIST
#define NODE_LIST(X)  \
	X(IntegerLiteral) \
	X(FloatingPointLiteral) \
	X(CharacterLiteral) \
	X(BoolExpression) \
	X(StringLiteral)  \
	X(ArrayExpression)   \
	X(UnitExpression) \
	X(ErrorExpression)        \
	X(TypeReference)          \
	X(TraitParameterReference) \
	X(ImportedScopeReference) \
	X(VariableReference)     \
	X(FunctionReference)     \
	X(TupleExpression)       \
	X(CallExpression)        \
	X(LogicalOperation)      \
	X(FieldAccessExpression) \
	X(AccessExpression)      \
	X(AssignmentExpression)  \
	X(ReturnExpression)      \
	X(ControlFlowExpression) \
	X(ThrowExpression) \
	X(PointerOperation)  \
	X(IfExpression)      \
	X(BlockExpression)   \
	X(LoopExpression)    \
	X(CastExpression)    \
	X(ScopedCommand) \
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

	bool redefine;
	//The bool indicates whether the redirector is expression(true) or a definition(false)
	std::map<void*,std::pair<void*,bool> > redirectors;//Used to redirect references for duplicated definitions
	
	Variable* returnValueRedirector;//The variable to which the return value is assigned in inlined and mixined functions

	DuplicationModifiers(Scope* target) : returnValueRedirector(nullptr),expandedMacroOptimization(nullptr),redefine(true) { this->target = target; }

	void expandArgument(Argument* original,Node* value);
	void duplicateDefinition(Argument* original,Argument* duplicate);
	void duplicateDefinition(Variable* original,Variable* duplicate);
	void duplicateDefinition(Function* original,Function* duplicate);
	void duplicateDefinition(TypeDeclaration* original,TypeDeclaration* duplicate);
	void duplicateDefinition(PrefixMacro* original,PrefixMacro* duplicate);
	void duplicateDefinition(InfixMacro* original,InfixMacro* duplicate);

	TypeDeclaration* getDuplicate(TypeDeclaration* original);
};

//An AST node
struct Node {
protected:	
	SymbolID _label; //optional label e.g. x:1 => 1 will have label x
public:
	Location _location;
	uint16   flags;
protected:	
	

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
	virtual void  walkDefiningLocals(Resolver* resolver){ }
	virtual Node* optimize(Optimizer* optimizer);
	virtual bool  isSame  (Node* other){ return false; }

	inline  SymbolID label()   const { return _label; }
	inline void label(SymbolID symbol) { _label = symbol; }
    Location location() const { return _location; }
	virtual bool applyProperty(SymbolID name,Node* value){ return false; }

	void dump(Dumper& dumper) const;

	//Dynamic casts
	virtual bool isDefinitionNode(){ return false; }
	virtual PrefixDefinition* asPrefixDefinition() { return nullptr; }
	virtual bool isUntypedLiteral() const { return false; }
#define CAST(T) virtual T* as##T() { return nullptr; }
	NODE_LIST(CAST)
#undef CAST
private:
	virtual void  dumpImplementation(Dumper& dumper) const { }
};
std::ostream& operator<< (std::ostream& stream,Node* node);

/**
* These nodes are definition nodes.
* A definition node provides a mapping from a symbol in the source file into an custom expression for both parser and resolver.
*/
struct DefinitionNode : Node { 
	DefinitionNode() : generatorData(nullptr) {}

	uint8 visibilityMode() const;
	bool  isPublic() const;
//protected:
	void visibilityMode(uint8 mode);

	void* generatorData; //extra data used by generator
	int   generatorDataRound;
	Node* parentNode;
private:
	bool isDefinitionNode(){ return true; }
	enum { IS_PRIVATE = 0x2 };//NB: it overrides the const flag, which is irrelevant for declarations
};

struct PrefixDefinition : DefinitionNode {
	enum {
		LOOKUP_HIDE_BEFORE_DECLARATION = 0x4, //Hides local variables before they are declared.
	};

	PrefixDefinition(SymbolID name,Location& location);
	virtual Node* parse(Parser* parser) = 0;
	PrefixDefinition* asPrefixDefinition() { return this; }

	//Used to hide local variables until their declaration.
	inline bool isHiddenBeforeDeclaration(int resolvingPass) { return isFlagSet(LOOKUP_HIDE_BEFORE_DECLARATION) && resolvingPass != this->generatorDataRound; }
	inline void makeDeclarationVisible   (int resolvingPass) { this->generatorDataRound = resolvingPass; }

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
	enum {
		EXPLICIT_TYPE = 0x4 //strongly typed literal
	};
	inline LiteralNode() { setFlag(CONSTANT | RESOLVED); }
	bool isUntypedLiteral() const { return !isFlagSet(EXPLICIT_TYPE); }

	Type* explicitType;

	inline void specifyType(Type* type){
		explicitType = type;
		setFlag(EXPLICIT_TYPE);
	}
};

//(0..9)+ : integer
struct IntegerLiteral : LiteralNode {
	IntegerLiteral(const BigInt& integer,Type* t = nullptr);
	Type* returnType() const;

	bool  isSame(Node* other);
	
	BigInt integer;
	DECLARE_NODE(IntegerLiteral);
};

struct FloatingPointLiteral : LiteralNode {
	FloatingPointLiteral(const double v,Type* t = nullptr);
	Type* returnType() const;

	bool  isSame(Node* other);

	double value;
	DECLARE_NODE(FloatingPointLiteral);
};

struct CharacterLiteral: LiteralNode {
	CharacterLiteral(UnicodeChar c,Type* t = nullptr);
	Type* returnType() const;

	bool  isSame(Node* other);

	UnicodeChar value ;
	DECLARE_NODE(CharacterLiteral);
};

//true | false
struct BoolExpression: Node {
	BoolExpression(const bool v);
	Type* returnType() const;

	bool  isSame(Node* other);

	bool value;
	DECLARE_NODE(BoolExpression);
};

struct StringLiteral : LiteralNode {
	StringLiteral(memory::Block& block,Type* t = nullptr);
	StringLiteral(SymbolID symbol);//<-- reuses the string, no duplication
	Type* returnType() const;

	bool  isSame(Node* other);
	
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

	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);
};

struct ArrayExpression : NodeList {
	ArrayExpression();
	
	Type* returnType() const;
	Node* resolve(Resolver* resolver);

	Type* explicitType;
	DECLARE_NODE(ArrayExpression);
};


//():void
struct UnitExpression : Node{
	UnitExpression(){ setFlag(CONSTANT | RESOLVED); }
	Type* returnType() const;

	bool  isSame(Node* other);

	DECLARE_NODE(UnitExpression);
private:
	UnitExpression(const UnitExpression& other){}
};


//: intrinsics::types::Scope
struct ImportedScopeReference : Node {
	ImportedScopeReference(ImportedScope* scope);

	Type* returnType() const;

	ImportedScope* scope;
	DECLARE_NODE(ImportedScopeReference);
};

struct NodeReference : Node {
	enum {
		DONT_DUPLICATE_OBJECT = 0x8,
	};
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

	bool  isSame(Node* other);

	Type* type;
	DECLARE_NODE(TypeReference);
};

struct TraitParameterReference : Node {
	TraitParameterReference(size_t i);

	size_t index;
	DECLARE_NODE(TraitParameterReference);
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
	enum {
		GEN_REWRITE_AS_VECTOR = 0x16,
	};
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

		CALL_TO_DESTRUCTOR = 0x8,
		CALL_TO_CONSTRUCTOR = 0x10,
	};
	CallExpression(Node* object,Node* argument);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

	Node* object;
	Node* arg;
	DECLARE_NODE(CallExpression);
};

//&& or ||
struct LogicalOperation: Node {
	LogicalOperation(Node* x,Node* y,bool isOr);
	Type* returnType() const;
	void  walkDefiningLocals(Resolver* resolver);
	Node* resolve(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

	inline bool isAnd() const { return !isFlagSet(IS_OR); }
	inline bool isOr()  const { return isFlagSet(IS_OR); }

	Node* parameters[2];
	DECLARE_NODE(LogicalOperation);
private:
	LogicalOperation(Node* x,Node* y);
	enum {
		IS_OR = 0x4
	};
};

// Record.field
// Pointer(Record).field
struct FieldAccessExpression : Node {
	FieldAccessExpression(Node* object,int field);
	
	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);
	Type* returnType() const;

	Type*    fieldsType() const;
	SymbolID fieldsName() const;

	Node* object;
	int field;
	DECLARE_NODE(FieldAccessExpression);
};

/// NB: assignment(make exception for the initializing assignment) is a statement(Hello nemerle)
struct AssignmentExpression : Node {
	enum {
		INITIALIZATION_ASSIGNMENT = 0x8
	};
	AssignmentExpression(Node* object,Node* value);

	Type* returnType() const;
	Node* optimize(Optimizer* optimizer);
	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);

	Node* object;
	Node* value;
	DECLARE_NODE(AssignmentExpression);
};

// : intrinsics::types::Void
struct ReturnExpression : Node {
	ReturnExpression(Node* expression);

	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

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

struct ThrowExpression: Node {
	ThrowExpression(Node* node);

	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);

	Node* expression;
	DECLARE_NODE(ThrowExpression);
};

//A unary expression, which is either an adress or a dereference
//: Pointer(expression.typeof)
struct PointerOperation : Node {
	PointerOperation(Node* expression,int type);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);


	enum {
		ADDRESS,     //&
		DEREFERENCE, //*

		DEREFERENCE_OR_TYPE,//*
	};
	enum {
		ADDRESS_RETURNS_REF = 0x10,
	};

	inline bool isAddress()     const { return kind == ADDRESS; }
	inline bool isDereference() const { return kind == DEREFERENCE; }
	inline bool isDereferenceOrType() const { return kind == DEREFERENCE_OR_TYPE; };
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
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

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
		OPTIMIZATION_CONSTRUCTS_VARIABLE = 0x20,// Foo() -> { var _ Foo; construct(&_); _ }
	};
    BlockExpression();
	BlockExpression(Scope* scope);

	Type* returnType() const;
	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

	void _duplicate(BlockExpression* dest,DuplicationModifiers* mods) const;

	BlockExpression* duplicateMixin(DuplicationModifiers* mods) const;

	/// lame...
	void addChildPotentiallyDisturbingIteration(Node* child);

	Scope* scope;
	Node* parentNode;
	DECLARE_NODE(BlockExpression);
private:
	enum {
		ITERATING = 0x100,
		ITERATION_MODIFIED_CHILDREN = 0x200,
	};
};

// A while or a do while expression
// : intrinsics::types::Void
struct LoopExpression : Node {
	LoopExpression(Node* body);

	Node* resolve(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);

	Node* body;
	DECLARE_NODE(LoopExpression);
};

struct CastExpression : Node {
	CastExpression(Node* object,Type* type);

	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	Type* returnType() const;
	Node* optimize(Optimizer* optimizer);

	Node* object;
	Type* type;
	DECLARE_NODE(CastExpression);
};

struct ScopedCommand : Node {


	//public: / private:
	ScopedCommand(data::ast::VisibilityMode mode,Node* expression = nullptr);
	ScopedCommand();

	Node* resolve(Resolver* resolver);
	Node* optimize(Optimizer* optimizer);
	bool  isSame(Node* other);

	std::vector<std::pair<SymbolID,Node*>> parameters;
	Node* child;
	DECLARE_NODE(ScopedCommand);

	enum {
		PUBLIC  = 0x10,
		PRIVATE = 0x20,
		WHERE   = 0x80,
	};
public:
	inline bool isVisibilityMode(data::ast::VisibilityMode mode) const { return isFlagSet(mode == data::ast::PRIVATE? PRIVATE: PUBLIC); }
	inline bool isWhere() const { return isFlagSet(WHERE); }
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
	void  walkDefiningLocals(Resolver* resolver);

	Node* object;
	SymbolID symbol;
	DECLARE_TEMPNODE(AccessExpression);
};

// An expression representing pattern matching of a certain object
// A temporary match placeholder must be used before if-else tree so that the pattern for certain object like Types or records will be produced correctly!
struct MatchResolver : NodeList {
	MatchResolver(Node* object);

	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);

	Node* object;
	DECLARE_TEMPNODE(MatchResolver);
};

// A dummy expression representing an error and never resolving
// N.B. use getInstance instead of new for creation!
struct ErrorExpression : AlwaysUnresolved {
	static ErrorExpression* getInstance(); //avoid multiple creations
	Node* resolve(Resolver* resolver);

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
