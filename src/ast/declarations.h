/**
* This module contains the amin declarations such as function, type and variable.
*/
#ifndef DECLARATIONS_H
#define DECLARATIONS_H

struct Node;
struct Parser;
struct BlockExpression;
struct CallExpression;
struct Type;
struct TypeExpression;
struct Evaluator;

#include "../base/bigint.h"
#include "node.h"

struct Argument;

struct Variable : PrefixDefinition  {
	Variable(SymbolID name,Location& location,bool isLocal = false);

	Node* parse(Parser* parser);

	void setImmutableValue(Node* value);

	bool isResolved();
	bool resolve(Evaluator* evaluator);

	bool isLocal();

	PrefixDefinition* duplicate(DuplicationModifiers* mods);

	virtual Argument* asArgument();

	InferredUnresolvedTypeExpression type;
	Node* value;    // = nullptr // if it's immutable, place the assigned value here
	bool isMutable; // = true
	bool expandMe;  // = false // assume value != nullptr
	bool _local;
};

struct Argument : Variable {
	Argument(SymbolID name,Location& location);

	Argument* asArgument();	

	PrefixDefinition* duplicate(DuplicationModifiers* mods);

	Argument* reallyDuplicate(DuplicationModifiers* mods,TypeExpression* newType);

	void defaultValue(Node* expression,bool inferType,bool typecheck = true);
	Node* defaultValue() const;

	bool expandAtCompileTime();
	bool isDependent() const;
	
	bool _dependent;
	Function* _constraint;//For wildcard eyes only!
private:
	Node* _defaultValue;
};


struct TypeBase : PrefixDefinition {
public:
	TypeBase(SymbolID name,Location& location) : PrefixDefinition(name,location) {}

	virtual size_t size() const = 0;

};

//An intrinsic type
struct IntrinsicType : public TypeBase {
	IntrinsicType* _base;
	IntrinsicType(SymbolID name,Location& location,IntrinsicType* base = nullptr);

	size_t size() const;
	Node* parse(Parser* parser);

	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }
	
	//Parsing special types on creation
	Function* construct;
};

//An integral type
struct IntegerType: public TypeBase {
	IntegerType(SymbolID name,Location& location);

	size_t size() const;
	bool isValid(BigInt& value) const;
	bool isUnsigned() const;

	bool isSubset(IntegerType* other) const;

	Node* parse(Parser* parser);
	BigInt max,min;
	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }

private:
	
	size_t _size;
};

//A pointer type
struct PointerType: public TypeBase {
	PointerType(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	size_t size() const;
};

//A record type
struct Record: public TypeBase {
private:	
	
	Record* headRecord; ///if this is null, then the type isn't an unonymous record
	bool _resolved; // = false
	size_t _size;
public:

	struct Field {
		SymbolID name;
		InferredUnresolvedTypeExpression type;
		bool isExtending; //a field aliased as this

		Field(SymbolID id,TypeExpression* typ) : name(id),type(typ),isExtending(false) {}
		Field duplicate(DuplicationModifiers* mods);
	private:
		Field(){}
	};
	std::vector<Field> fields;

	Record(SymbolID name,Location& location);

	Node* parse(Parser* parser);
	Node* createReference();

	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName);
	// Adds a field to the record. NB record must be unresolved.
	void add(const Field& var); 

	//Record's properties
	bool isResolved();
	size_t size() const;
	
	//Try to resolve the record.
	//NB Not used by anonymous records!
	bool resolve(Evaluator* evaluator);

	PrefixDefinition* duplicate(DuplicationModifiers* mods);

	
	//Unique anonymous record construction
	//NB all fields must have resolved types
	static Record* findAnonymousRecord(std::vector<Field>& record);
	//An anonymous record
	inline bool isAnonymous() const { return headRecord != nullptr; }
	//Determines whether two records have the same field types or not
	inline static bool anonymousRecordsSameTypes(Record* r1,Record* r2){ return r1->headRecord == r2->headRecord; }
private:
	static Record* createRecordType(std::vector<Field>& record,Record* headRecord = nullptr);
	static Record* findSubRecord(Record* headRecord,std::vector<Record*>& subRecords,std::vector<Field>& record);
	
	//Calculates sizeof etc.
	void calculateResolvedProperties();
	bool resolveCircularReferences(Evaluator* evaluator);
};

std::ostream& operator<< (std::ostream& stream,Record* type);

//An overload set consists of function with the same name, whcih are defined in the same scope
struct Overloadset: public PrefixDefinition {
	Overloadset(Function* firstFunction);

	Node* parse(Parser* parser);


	bool isResolved();
	bool resolve(Evaluator* evaluator);
	PrefixDefinition* duplicate(DuplicationModifiers* mods);
	PrefixDefinition* mergedDuplicate(DuplicationModifiers* mods,Overloadset* dest);
	void push_back(Function* function);

	Overloadset* asOverloadset();
	
	std::vector<Function*> functions;
private: 
	bool _resolved;
};

// A function
// By defualt function is created with an empty body with null scope
// Return type is infered by default

struct Function: public PrefixDefinition {
	//Flags
	enum {
		//Indicates whether some function, which can be evaluated at compile time,
		//is allowed to be interpreted only when it's owner function is being interpreted
		INTERPRET_ONLY_INSIDE = 0x1,

		//Allows it to act as a type when declaring arguments for other functions
		CONSTRAINT_FUNCTION = 0x2,

		MACRO_FUNCTION = 0x4,

		CONTAINS_RETURN = 0x10
	};

	Function(SymbolID name,Location& location,Scope* bodyScope);
	
	Node* parse(Parser* parser);
	Node* createReference();

	bool isResolved();
	bool isPartiallyResolved();//It's when arguments and return type are resolved! The function's body doesn't have to be resolved yet.
	bool resolve(Evaluator* evaluator);
	bool canExpandAtCompileTime();//i.e. f(T Type)
	bool canAcceptLocalParameter(size_t argument); 

	TypeExpression* argumentType();
	TypeExpression* returnType();
	Scope* owner() const;

	Function* duplicate(DuplicationModifiers* mods);
	//Used to specialise a function with wildcard parameters
	Function* specializedDuplicate(DuplicationModifiers* mods,std::vector<TypeExpression* >& specializedArgTypes);
	bool expandedDuplicate(DuplicationModifiers* mods,std::vector<Node*>& parameters,Function** dest);

	//Returns -1 when an argument isn't found
	int findArgument(Variable* var) const;

	InferredUnresolvedTypeExpression _returnType;
	BlockExpression body;
	Node* (*constInterpreter)(Node* parameters); //Can be null. Used to interpret the function with const parameters.
	Node* (*intrinsicEvaluator)(CallExpression*,Evaluator* evaluator); //Can be null. Used for compile time arithmetics etc.
	std::vector<Argument*> arguments;
	bool _hasGenericArguments;
	bool _hasExpandableArguments;
	bool _argsResolved;
	bool _resolved;

private:
	Function* duplicateReturnBody(DuplicationModifiers* mods,Function* func);
	
	
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
};

#endif