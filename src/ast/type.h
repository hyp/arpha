/**
* This module implements arpha type.
*/
#ifndef ARPHA_AST_TYPE_H
#define ARPHA_AST_TYPE_H

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

		PatternMatcher(Scope* scope) : container(scope) {}
		IntroducedDefinition* lookupDefinition(SymbolID name);
		void introduceDefinition(SymbolID name,Location location,Node* value = nullptr);
		bool check(Node* expression); //Returns true if a certain expression is a type pattern e.g. _
		bool match(Type* type,Node* pattern);
		Node* inverseMatch(Node* pattern);
		void defineIntroducedDefinitions();
	private:
		bool match(Node* object,Node* pattern);
	};

	bool resolve(Resolver* evaluator,PatternMatcher* patternMatcher = nullptr);
	void parse(Parser* parser,int stickiness);
};
std::ostream& operator<< (std::ostream& stream,TypePatternUnresolvedExpression& type);


#include "../data/data.h"

struct TypeLayout {
	uint32 size;
	uint32 alignment;
};

//(type ...): intrinsics::types::Type
struct Type {
	static data::gen::AbstractTarget::TypeSystemState typeSystemState;

	enum {
		VOID,
		TYPE,//typeof(int32)
		BOOL,
		RECORD,
		VARIANT,
		INTEGER,
		FUNCTION,
		POINTER,
		STATIC_ARRAY,
		POINTER_BOUNDED_CONSTANT, //[2]int32
		POINTER_BOUNDED, //[]int32
		NODE, //i.e. *ast.Expression, uses nodeSubtype to determine the type
	};

	Type(int kind);
	Type(IntegerType* integer);
	Type(Record* record);
	Type(int kind,Type* next);//ptr | bounded pointer
	Type(Type* argument,Type* returns);//function
	Type(int kind,Type* next,size_t N); //static array | bounded pointer constant length
	Type(int kind,int subtype);//node

	//
	inline bool isVoid()     const { return type == VOID;     }
	inline bool isType()     const { return type == TYPE;     }
	inline bool isBool()     const { return type == BOOL;     }
	inline bool isInteger()  const { return type == INTEGER;  }
	inline bool isPointer()  const { return type == POINTER;  }
	inline bool isRecord()   const { return type == RECORD;   }
	inline bool isVariant()  const { return type == VARIANT;  }
	inline bool isFunction() const { return type == FUNCTION; }
	inline bool isNodePointer() const { return type == POINTER && argument->type == NODE; }
	inline bool isBoundedPointer() const { return type == POINTER_BOUNDED; }
	inline bool isBoundedPointerConstantLength() const { return type == POINTER_BOUNDED_CONSTANT; }


	Record* asRecord() { return record; }
	const Record* asRecord() const { return record; }

	IntegerType* asInteger(){ return integer; }
	const IntegerType* asInteger() const { return integer; }

	inline Type* next(){ return argument; }
	inline const Type* next() const { return argument; }

	//self explanatory
	bool isValidTypeForVariable();
	bool isValidTypeForArgument();

	//..
	bool isResolved() const;
	bool isPartiallyResolved() const;
	bool matchRecord(Record* record) const;
	size_t size() const;

	bool isSame(Type* other);

	//const int32
	bool hasConstSemantics() const { return false; }


	bool wasGenerated() const; //Is this a parametrized type?
	bool wasGeneratedBy(Function* function) const;  //Returns the function which generated this type
	Node* generatedArgument(size_t i) const; //Returns the parameter i which was the argument to the function which generated this type

	/**
	* This is the one of the key functions of the type system.
	* Given an expression and its type, this function will check if the 'this' type can be assigned from expression's type.
	* If such an assignment is possible, it will return the resulting expression with possible conversions.
	* If not, it will return null.
	*/
	Node* assignableFrom(Node* expression,Type* type);
	int canAssignFrom(Node* expression,Type* type);
public:
	int type;
	union {
		Record* record;
		IntegerType* integer;
		Type* argument;
		Node* pattern;
		int   nodeSubtype;//USE -1 for untyped note i.e. [> 1 <] returns untyped node, but new ast.IntegerLiteral returns typed node!
	};
	union {
		Type* returns;
		size_t N;
	};
	friend std::ostream& operator<< (std::ostream& stream,Type* node);
};

std::ostream& operator<< (std::ostream& stream,Type* type);

struct DeclaredType: public Type {
	DeclaredType(int kind) : Type(kind) {}

	virtual DeclaredType* duplicate(DuplicationModifiers* mods) const = 0;
	virtual DeclaredType* resolve(Resolver* resolver) = 0;
	inline  bool  isResolved(){ return _resolved; }

	TypeDeclaration* declaration;
protected:
	bool _resolved;
};

/**
* aka concept.
*/
struct Trait: public DeclaredType {
	Trait();

	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);

	std::vector<Function*> methods;
};

struct Variant: public DeclaredType {
	Variant();

	struct Field {
		SymbolID name;
		int      associatedType;//-1 for not type
	};
	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName) const;
	// Adds a field to an unresolved variant.
	void add(const Field& field);

	size_t size();
	DeclaredType* duplicate(DuplicationModifiers* mods) const;
	DeclaredType* resolve(Resolver* resolver);

	std::vector<Field> fields;
private:
	TypeLayout _layout;
	void calculateResolvedProperties();
};

// Variant | Record
struct AggregateType: public PrefixDefinition  {
protected:
	size_t _size;	
	Type _type; //type object

	AggregateType(int typeKind,SymbolID name,Location& location);
public:
	Type* asType()             { return &_type; }
	const Type* asType() const { return &_type; }

	//generated types
	enum {
		GENERATED = 0x4,// Was this type generated by a type generating function?
	};
	inline bool wasGenerated(){ return isFlagSet(GENERATED); }
	bool   wasGeneratedBy(Function* func) const;
	Node*  generatedArgument(size_t i) const;

	//Type reference creation
	Node* parse(Parser* parser);
	Node* createReference();

	//Returns the size of the type
	size_t size();

	Scope* _owner;
};


//A record type
struct Record: public AggregateType {
private:	
	Record* headRecord; ///if this is null, then the type isn't an unonymous record
public:
	enum {
		HAS_DERIVING_HIERARCHY = 0x8, // Is there any field that is derived?
	};
	struct Field {
		SymbolID name;
		TypePatternUnresolvedExpression type;
		bool isExtending; //a field aliased as this

		Field(SymbolID id,Type* typ) : name(id),type(typ),isExtending(false) {}
		Field duplicate(DuplicationModifiers* mods) const;
	private:
		Field(){}
	};
	std::vector<Field> fields;

	Record(SymbolID name,Location& location);

	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName) const;
	// Adds a field to the record. NB record must be unresolved.
	void add(const Field& var); 
	
	//Try to resolve the record.
	//NB Not used by anonymous records!
	Node* resolve(Resolver* resolver);

	//Unique anonymous record construction
	//NB all fields must have resolved types
	static Record* findAnonymousRecord(std::vector<Field>& record);
	//An anonymous record
	inline bool isAnonymous() const { return headRecord != nullptr; }
	//Determines whether two records have the same field types or not
	inline static bool anonymousRecordsSameTypes(Record* r1,Record* r2){ return r1->headRecord == r2->headRecord; }

	DECLARE_NODE(Record);
private:
	
	static Record* createRecordType(std::vector<Field>& record,Record* headRecord = nullptr);
	static Record* findSubRecord(Record* headRecord,std::vector<Record*>& subRecords,std::vector<Field>& record);
	
	//Calculates sizeof etc.
	void calculateResolvedProperties();
};

std::ostream& operator<< (std::ostream& stream,Record* type);

// an actual declaration of a type which is added to the expression list
struct TypeDeclaration: PrefixDefinition {
	TypeDeclaration(DeclaredType* type,SymbolID name);

	Type*  type()  const;

	//Type reference creation
	Node* parse(Parser* parser);
	Node* createReference();

	Node* resolve(Resolver* resolver);

	BlockExpression* optionalStaticBlock;//can be used for static members i.e. type as a namespace kinda thingy
private:
	DeclaredType*  _type;
	DECLARE_NODE(TypeDeclaration);
};

#endif
