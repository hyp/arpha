/**
* This module implements arpha type.
*/
#ifndef ARPHA_AST_TYPE_H
#define ARPHA_AST_TYPE_H

struct Trait;

// This is either a type expression or a type pattern. 
// It can also be an unresolved expression which is expected to resolve into a type or type pattern at a later stage.
struct TypePatternUnresolvedExpression {
	enum {
		PATTERN,//_,Pointer(_) etc
		UNRESOLVED,
		TYPE,
	};
	int kind;
	union {
		Type* _type;
		Node* unresolvedExpression;
		Node* pattern; //it can be null to indicate that the pattern is "_"
	};

	inline TypePatternUnresolvedExpression() : kind(PATTERN),pattern(nullptr) {}
	inline TypePatternUnresolvedExpression(Type* expr) : kind(TYPE),_type(expr) {}
	inline bool isResolved() const { return kind == TYPE;     }
	inline bool isPattern() const  { return kind == PATTERN;  }
	Type* type() const;

	TypePatternUnresolvedExpression duplicate(DuplicationModifiers* mods) const;

	void specify(Type* givenType);
	bool deduce (Type* givenType,Scope* container);

	struct PatternMatcher {
		struct IntroducedDefinition {
			SymbolID name;Location location;Node* value;

			inline IntroducedDefinition(SymbolID n,Location l,Node* v) : name(n),location(l),value(v) {}
		};
		std::vector<IntroducedDefinition> introducedDefinitions;
		Scope* container;
		Scope* expansionScope;//where the expansion is happening.. required to check if a type satisfies a trait from a perspective of the expansion scope
		Resolver* resolver;

		PatternMatcher(Scope* scope,Resolver* resolver) : container(scope),resolver(resolver) {}
		IntroducedDefinition* lookupDefinition(SymbolID name);
		void introduceDefinition(SymbolID name,Location location,Node* value = nullptr);
		bool check(Node* expression,Trait* currentTrait = nullptr); //Returns true if a certain expression is a type pattern e.g. _
		bool  match(Type* type,Node* pattern,Scope* expansionScope = nullptr);
		Type* matchWithSubtyping(Type* type,Node* pattern,Scope* expansionScope = nullptr);
		Node* inverseMatch(Node* pattern);
		void defineIntroducedDefinitions();
	private:
		bool match(Node* object,Node* pattern);
		Node* getPatternParameter(Node* pattern,Type* givenType);
	};

	bool resolve(Resolver* evaluator,PatternMatcher* patternMatcher = nullptr);
	void parse(Parser* parser,int stickiness);
};
std::ostream& operator<< (std::ostream& stream,TypePatternUnresolvedExpression& type);


#include "../data/data.h"


struct AnonymousAggregate;
struct FunctionPointer;
struct Variant;

//(type ...): intrinsics::types::Type
struct Type {

	enum {
		VOID,
		TYPE,//typeof(int32)
		BOOL,
		RECORD,
		VARIANT,
		INTEGER,//(u)intX
		FLOAT,//float32,float64
		CHAR,//char8,char16,char32
		NATURAL,// = size_t
		UINTPTRT,//uintptr_t
		
		POINTER,
		LINEAR_SEQUENCE,//a range of elements = { T*,natural }
		STATIC_ARRAY,

		FUNCTION_POINTER,

		NODE, //i.e. *ast.Expression, uses nodeSubtype to determine the type

		ANONYMOUS_RECORD,
		ANONYMOUS_VARIANT,

		VARIANT_OPTION,
		TRAIT,
		

		LITERAL_INTEGER,
		LITERAL_FLOAT,//1.2
		LITERAL_CHAR,//'A'
		LITERAL_STRING,
	};

	Type(int kind);
	Type(int kind,Type* next);//ptr | bounded pointer
	Type(int kind,Type* next,size_t N); //static array | bounded pointer constant length
	Type(int kind,int subtype);//node

	static Type* getIntegerType(int bits,bool isSigned);
	static Type* getIntegerLiteralType();
	bool   integerFits(uint64 value,bool isNegative);
	bool   doesLiteralFit(IntegerLiteral* node); // type must be integer or character

	static Type* getFloatType(int bits);
	static Type* getFloatLiteralType();

	static Type* getCharType (int bits);
	static Type* getCharLiteralType();
	bool   doesLiteralFit(CharacterLiteral* node); // type must be character

	static Type* getStringLiteralType();
	static Type* getLinearSequence(Type* next);

	static Type* getNaturalType();
	static Type* getUintptrType();

	static Type* getPointerType(Type* next);

	static Type* getVariantOption(int optionID);

	bool   canCastTo(Type* other);

	

	//
	inline bool isVoid()     const { return type == VOID;     }
	inline bool isType()     const { return type == TYPE;     }
	inline bool isBool()     const { return type == BOOL;     }
	inline bool isInteger()  const { return type == INTEGER;  }
	inline bool isPlatformInteger() const { return type == NATURAL; }
	inline bool isUintptr()  const { return type == UINTPTRT; }
	inline bool isFloat()    const { return type == FLOAT;    }
	inline bool isFloat32()  const { return type == FLOAT && bits == 32; }
	inline bool isFloat64()  const { return type == FLOAT && bits == 64; }
	inline bool isChar   ()  const { return type == CHAR;     }
	inline bool isChar8  ()  const { return type == CHAR && bits == 8;     }
	inline bool isPointer()  const { return type == POINTER;  }
	inline bool isRecord()   const { return type == RECORD;   }
	inline bool isVariant()  const { return type == VARIANT;  }
	inline bool isFunctionPointer() const { return type == FUNCTION_POINTER; }
	inline bool isNodePointer() const { return type == POINTER && argument->type == NODE; }
	inline bool isLiteral() const { return type>=LITERAL_INTEGER; }
	inline bool isLinearSequence() const { return type == LINEAR_SEQUENCE; }
	inline bool isVariantOption() const  { return type == VARIANT_OPTION; }
	inline bool isTrait() const { return type == TRAIT; }


	Record* asRecord();
	Variant* asVariant();
	AnonymousAggregate* asAnonymousRecord();
	FunctionPointer* asFunctionPointer();
	Trait* asTrait();

	inline Type* next(){ return argument; }
	inline const Type* next() const { return argument; }

	//self explanatory
	bool isValidTypeForVariable();
	bool isValidTypeForArgument();
	bool isValidTypeForReturn();
	bool isValidTypeForField();

	//..
	bool isResolved() const;
	bool isPartiallyResolved() const;

	bool requiresDestructorCall() const;

	bool isSame(Type* other);

	void setFlag(uint16 flag);
	bool isFlagSet(uint16 flag) const;

	//const int32
	inline bool hasConstSemantics() const { return isFlagSet(HAS_CONSTANT_SEMANTICS); }


	bool wasGenerated() const; //Is this a parametrized type?
	bool wasGeneratedBy(Function* function) const;  //Returns the function which generated this type
	Node* generatedArgument(size_t i) const; //Returns the parameter i which was the argument to the function which generated this type

	struct generators {
		static Function* linearSequence;
		static Function* functionPointer;
	};

	/**
	* This is the one of the key functions of the type system.
	* Given an expression and its type, this function will check if the 'this' type can be assigned from expression's type.
	* If such an assignment is possible, it will return the resulting expression with possible conversions.
	* If not, it will return null.
	*/
	int assignFrom(Node** expression,Type* type,bool doTransform);
	Node* assignableFrom(Node* expression,Type* type);
	int canAssignFrom(Node* expression,Type* type);


public:
	enum {
		IS_RESOLVED = 0x1,
		HAS_CONSTANT_SEMANTICS = 0x2,
		HAS_LOCAL_SEMANTICS = 0x4,
	};
	uint16 type;
	uint16 flags;
	union {
		Type* argument;
		Node* pattern;
		int   optionID;
		int   nodeSubtype;//USE -1 for untyped note i.e. [> 1 <] returns untyped node, but new ast.IntegerLiteral returns typed node!
		int   bits;       //number of bits in an integer / character type
	};
	union {
		size_t N;
	};
	friend std::ostream& operator<< (std::ostream& stream,Type* node);
};

std::ostream& operator<< (std::ostream& stream,Type* type);

/**
* A function pointer type.
*/
struct FunctionPointer: public Type {
private:
	Type* _returns;
	uint8 cc;

	FunctionPointer() : Type(FUNCTION_POINTER) {}
public:
	inline Type* parameter() const { return argument; }
	inline Type* returns()   const { return _returns; }
	inline data::ast::Function::CallConvention callingConvention() const { return (data::ast::Function::CallConvention)cc; }
	inline bool isNonthrow() const { return true;    }//TODO

	static FunctionPointer* get(Type* argument,Type* ret,data::ast::Function::CallConvention cc = data::ast::Function::ARPHA);
};

/*
* Anonymous aggregate
* It can be either a record type or a sum(variant) type
*/
struct AnonymousAggregate: public Type {
	struct Field {
		SymbolID name;
		Type*    type;
	};

	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName) const;

	//Unique anonymous record construction
	static AnonymousAggregate* create(Field* fields,size_t fieldsCount,bool isVariant = false);
private:
	AnonymousAggregate(Type** t,SymbolID* fs,size_t n,bool isVariant);

public:
	Type**     types;          //points to [int32,int32]
	SymbolID*  fields;         //points to ["x","y"]
	size_t     numberOfFields; // 2;
};

struct DeclaredType: public Type {
	DeclaredType(int kind) : Type(kind),generatorData(nullptr) {}

	virtual DeclaredType* duplicate(DuplicationModifiers* mods) const = 0;
	virtual DeclaredType* resolve(Resolver* resolver) = 0;
	virtual void  onTemplateSpecialization(Resolver* resolver){}
	inline  bool  isResolved() const { return isFlagSet(IS_RESOLVED); }

	

	TypeDeclaration* declaration;
	void* generatorData;
};

/**
* aka concept.
*/
struct Trait: public DeclaredType {
	Trait(Scope* templateDeclaration);

	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);

	inline bool isImplicit(){ return true; }
	size_t numberOfTemplateParameters();

	std::vector<Function*> methods;

	Scope* templateDeclaration;
private:
	bool verify();
};


struct Variant: public DeclaredType {
	enum {
		NO_INNER_STRUCTURES = 0x10,//plain variant
	};

	Variant();

	inline bool hasNoStructuredOptions(){ return isFlagSet(NO_INNER_STRUCTURES); }

	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);

	size_t numberOfOptions;
};

/**
* A variant option (without additional structure load).
*/
struct VariantOption: public DeclaredType {
private:
	Variant* owner;
	
public:
	VariantOption(Variant* variant,int id);

	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);

	inline Variant* variant(){ return owner;    }
	inline int id()          { return optionID; }
};

struct Record: public DeclaredType {
	struct Field {
		SymbolID name;
		TypePatternUnresolvedExpression type;
		Node* initializer;
		bool isExtending; //a field aliased as this
		bool isPrivate;
		bool isReadonly;

		Field(SymbolID id,Type* typ) : name(id),type(typ),isExtending(false),isPrivate(false),isReadonly(false) {}
		Field duplicate(DuplicationModifiers* mods) const;
	private:
		Field(){}
	};
	
	Record();

	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName) const;
	// Adds a field to the record. NB record must be unresolved.
	void add(const Field& var); 
	

	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);
	void onTemplateSpecialization(Resolver* resolver);
	std::pair<Function*,Function*> createFieldGetterSetter(Location location,int fieldID);

	std::vector<Field> fields;
};

// an actual declaration of a type which is added to the expression list
struct TypeDeclaration: PrefixDefinition {
	TypeDeclaration(DeclaredType* type,SymbolID name,bool isParametrized = false);

	//Type reference creation
	Node* parse(Parser* parser);
	Node* createReference();

	Node* resolve(Resolver* resolver);

	BlockExpression* optionalStaticBlock;//can be used for static members i.e. type as a namespace kinda thingy
private:
	enum {
		PARAMETRIZED = 0x8
	};

	DeclaredType*  _type;
	DECLARE_NODE(TypeDeclaration);
public:
	inline DeclaredType*  type()  const { return _type; }
	inline bool isParametrized()  const { return isFlagSet(PARAMETRIZED); }
	Function* parametrization()   const;
};

#endif
