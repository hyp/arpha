/**
* This module contains the amin declarations such as function, type and variable.
*/
#ifndef ARPHA_AST_DECLARATIONS_H
#define ARPHA_AST_DECLARATIONS_H

struct Node;
struct Parser;
struct BlockExpression;
struct CallExpression;
struct Type;
struct Type;
struct Resolver;
struct CTFEintrinsicInvocation;

#include "../base/bigint.h"
#include "node.h"


//Some macroes must be implemented not in arpha but here :(
struct IntrinsicPrefixMacro : PrefixDefinition {
	IntrinsicPrefixMacro(SymbolID name);

private:
	//dummy implementation
	Node* duplicate(DuplicationModifiers* mods) const { return nullptr; }
	Node* accept(NodeVisitor* visitor){ return this; }
};

struct IntrinsicInfixMacro : InfixDefinition {
	IntrinsicInfixMacro(SymbolID name,int stickiness);

private:
	//dummy implementation
	Node* duplicate(DuplicationModifiers* mods) const { return nullptr; }
	Node* accept(NodeVisitor* visitor){ return this; }
};

struct Argument;

struct Variable : PrefixDefinition  {
	enum {
		//The type that the variable returns isn't equal to the type that the variable was defined with 
		// e.g. macro foo(x) = x  , x will be _ pattern on declaration, but really it is a *ast.Expression
		HIDDEN_TYPE = 0x10 ,
		IS_IMMUTABLE = 0x20,//def
		CONSTANT_SUBSTITUTE = 0x40,//def a = 1 => an occurence of a will be replaced by 1. Make sure value is set before setting this flag.
		IS_THREADLOCAL = 0x80,
		ANALYSIS_USED = 0x100,
	};
	Variable(SymbolID name,Location& location);
	Variable(SymbolID name,Location& location,Scope* owner,Node* value);//constant injection

	bool applyProperty(SymbolID name,Node* value);

	Node* parse(Parser* parser);
	Node* createReference();

	void setImmutableValue(Node* value);
	inline Node* asConstantSubstitute(){ return isFlagSet(CONSTANT_SUBSTITUTE) ? value : nullptr; }

	Node* resolve(Resolver* resolver);
	void  walkDefiningLocals(Resolver* resolver);
	//Use at variable's definition when a type is specified
	void specifyType(Type* givenType);
	//Matches a type to a patterned type and resolves the patterned type. Returns an error if match fails.
	bool deduceType(Type* givenType); 

	bool isLocal() const;
	Function* functionOwner() const;

	virtual Argument* asArgument();
	Type* referenceType() const;

	TypePatternUnresolvedExpression type;
	Node* value;    // = nullptr // if it's immutable, place the assigned value here
	Scope* _owner;  // set at resolving
	uint16 ctfeRegisterID;

	static Node* getIntrinsicValue(Variable* variable);

	DECLARE_NODE(Variable);
};

struct Argument : Variable {
	enum {
		IS_EXPENDABLE = 0x80, //Is this argument expendable?
		IS_VARARG = 0x40
	};

	Argument(SymbolID name,Location& location,Function* owner);

	Argument* asArgument();	
	void hideType(Type* givenType);
	Type* hiddenType() const;

	Argument* reallyDuplicate(Function* dest,DuplicationModifiers* mods) const;

	Argument* specializedDuplicate(Function* dest,DuplicationModifiers* mods,Type* specializedType,Node* expandedValue);

	Node* resolve(Resolver* resolver);
	void  defaultValue(Node* expression);
	Node* defaultValue() const;

	bool expandAtCompileTime() const;
	bool isDependent() const;

	inline bool isVararg() const { return isFlagSet(IS_VARARG); }

	
private:
	Type* _hiddenType;
	Node* _defaultValue;
};

//An overload set consists of function with the same name, whcih are defined in the same scope
struct Overloadset: public PrefixDefinition {
	enum {
		TYPE_GENERATOR_SET = 0x100,//Overload set for functions which generate types e.g. Pointer(T) can't be combined with other sets
	};
	Overloadset(Function* firstFunction);

	Node* parse(Parser* parser);

	Node* duplicate(DuplicationModifiers* mods) const { assert(false); return nullptr; }
	Node* accept(NodeVisitor* visitor){ return this; }
	void push_back(Function* function);

	Overloadset* asOverloadset();
	
	std::vector<Function*> functions;
};

// A function
// By default function is created with an empty body with null scope
// Return type is infered by default

struct Function: public PrefixDefinition {
	//Flags
	enum {
		//Indicates whether some function, which can be evaluated at compile time,
		//is allowed to be interpreted only when it's owner function is being interpreted
		INTERPRET_ONLY_INSIDE = 0x10,

		//This function is a body of constraint, it allows it to act as a type in type patterns
		CONSTRAINT_FUNCTION = 0x20,

		//This function is a body of a macro
		MACRO_FUNCTION = 0x40,

		//Is there a return expression present?
		CONTAINS_RETURN = 0x80,

		//This function can't be interpreted at compile time
		CANT_CTFE = 0x100,

		//This function is pure
		PURE = 0x200,

		//Does this function have at least one pattern argument? 
		// (If it does this function can be specialized)
		HAS_PATTERN_ARGUMENTS = 0x400,

		//Does this function have at least one expendable argument? 
		// (If it does this function can be simplified by bringing the arguments like types from the invocation into the function itself)
		// The function expansion based on expendable arguments is very similar to templates!
		HAS_EXPENDABLE_ARGUMENTS = 0x800,

		ARGUMENTS_RESOLVED = 0x1000,

	};

	Function(SymbolID name,Location& location);

	void dumpDeclaration(Dumper& dumper) const;

	Node* parse(Parser* parser);
	Node* createReference();

	Node* resolve(Resolver* evaluator);
	Node* optimize(Optimizer* optimizer);

	Type* returnType() const;

	//Properties:
	inline bool areArgumentsResolved() const { return isFlagSet(ARGUMENTS_RESOLVED); }

	bool applyProperty(SymbolID name,Node* value);
	void makeAllArgumentsExpendable(); //Makes all arguments to be expendable, essentialy making this function a template.

#define IMPL_PROP_GET(flag) const { return (miscFlags & flag) != 0; }

	void setNonthrow();
	inline bool isNonthrow()    IMPL_PROP_GET(data::ast::Function::NONTHROW)

	inline bool isTest()        IMPL_PROP_GET(data::ast::Function::Internal::UNITTEST)
	inline bool isExternal()    IMPL_PROP_GET(data::ast::Function::Internal::EXTERNAL)
	inline bool isDllimport()   IMPL_PROP_GET(data::ast::Function::Internal::EXTERNAL_DLLIMPORT)

	inline bool isIntrinsic()   IMPL_PROP_GET(data::ast::Function::Internal::INTRINSIC)
	inline bool isIntrinsicReturningPattern() IMPL_PROP_GET(data::ast::Function::Internal::INTRINSIC_RETURNS_PATTERN)
	inline bool isIntrinsicOperation() IMPL_PROP_GET(data::ast::Function::Internal::INTRINSIC_OPERATION)
	inline data::ast::Operations::Kind getOperation() const { return (data::ast::Operations::Kind)ctfeRegisterCount; }
	void makeIntrinsic();
	void makeIntrinsicReturningPattern();
	void makeIntrinsicOperation(data::ast::Operations::Kind op);
	static Type* getIntrinsicOperationReturnType(Type* operand1,data::ast::Operations::Kind op);

	inline bool isFieldAccessMacro() IMPL_PROP_GET(data::ast::Function::Internal::MACRO_FIELD_ACCESS)
	void makeFieldAccess(int fieldId);
	int  getField() const;

	inline bool isTypeTemplate() IMPL_PROP_GET(data::ast::Function::Internal::TYPE_TEMPLATE)
	void makeTypeTemplate(TypeDeclaration* node);
	TypeDeclaration* getTemplateTypeDeclaration();

#undef IMPL_PROP_GET

	data::ast::Function::CallConvention callingConvention() const;

	//..

	Type* argumentType() const;//Packs all arguments into a tuple.
	Type* returns() const;
	Scope* owner() const;
	Scope* parameterPatternMatchingScope() const;

	//Used to specialise a function with type pattern and/or expandable parameters
	Function* specializationExists(Type** specializedParameters,Node** passedExpressions,Scope* usageScope);
	Function* specializedDuplicate(DuplicationModifiers* mods,Type** specializedParameters,Node** passedExpressions);

	Function* reallyDuplicate(DuplicationModifiers* mods,bool redefine = true);

	//Returns -1 when an argument isn't found
	int findArgument(Variable* var) const;
	void addArgument(Argument* arg);
	void specifyReturnType(Type* givenType);

	std::vector<Argument*> arguments;
	TypePatternUnresolvedExpression _returnType;
	BlockExpression body;
	TypePatternUnresolvedExpression::PatternMatcher allArgMatcher; //a fused matcher is used so that a patterned argument will be able to acess introduced definitons from other patterns
	//expanded
	uint32 miscFlags;
	uint16 ctfeRegisterCount;
	uint16 inliningWeight;
	uint8  cc;
	typedef void (*CTFE_Binder)(CTFEintrinsicInvocation* invocation);
	union {
		const char* externalLib; //NB: Don't use extern on intrinsic functions!
		CTFE_Binder intrinsicCTFEbinder;
	};

	//Bindings to compile time evaluator
	static void getIntrinsicFunctionBinder(Function* function);
	static void getIntrinsicTypeTemplateBinder(Function* function);

	//generated functions
	Function* generatedFunctionParent;
	std::vector<Function*> generatedFunctions;
	std::vector<Node*>     expandedArguments ;
	
	DECLARE_NODE(Function);
private:

	Function* duplicateReturnBody(DuplicationModifiers* mods,Function* func) const;
	
	
};

// A single node in an import symbol tree
//a -> actual scope
//b -> bogus scope
//b.a -> actual scope
struct ImportedScope : PrefixDefinition {
	ImportedScope(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Scope* scope;
	std::map<SymbolID,ImportedScope*> importTree;
	//A single reference expression
	ImportedScopeReference _reference;
	inline ImportedScopeReference* reference(){ return &_reference; }

	Node* duplicate(DuplicationModifiers* mods) const { assert(false); return nullptr; }
	Node* accept(NodeVisitor* visitor){ return this; }
};

#endif
