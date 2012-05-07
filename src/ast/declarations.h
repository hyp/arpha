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

#include "../base/bigint.h"
#include "node.h"

struct Variable : PrefixDefinition  {
	Variable(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	void setImmutableValue(AssignmentExpression* node,Node* value);

	InferredUnresolvedTypeExpression type;
	Node* value;    // = nullptr // if it's immutable, place the assigned value here
	//A single reference expression
	VariableReference _reference;
	inline VariableReference* reference(){ return &_reference; }
	
	bool isMutable; // = true
	bool expandMe;  // = false // assume value != nullptr
	AssignmentExpression* nodeWhichAssignedMe; // = nullptr
};

struct TypeBase : PrefixDefinition {
public:
	TypeBase(SymbolID name,Location& location) : PrefixDefinition(name,location) {}

	virtual size_t size() const = 0;

};

//An intrinsic type
struct IntrinsicType : public TypeBase {
	IntrinsicType(SymbolID name,Location& location);

	size_t size() const;
	Node* parse(Parser* parser);

	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }
};

//An integral type
struct IntegerType: public TypeBase {
	IntegerType(SymbolID name,Location& location);

	size_t size() const;
	bool isValid(BigInt& value) const;
	bool isUnsigned() const;
	Node* assignableFrom(Node* expression,IntegerType* type);

	Node* parse(Parser* parser);
	BigInt max,min;
	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }
private:
	
	size_t _size;
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
	};
	std::vector<Field> fields;
	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }

	Record(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	// Returns -1 if field isn't found
	int lookupField(const SymbolID fieldName);
	// Adds a field to the record. NB record must be unresolved.
	void add(const Field& var); 

	//Record's properties
	bool resolved();
	size_t size() const;
	
	//Try to resolve the record.
	//NB Not used by anonymous records!
	bool resolve(Evaluator* evaluator);

	
	//unique anonymous record construction
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
};

std::ostream& operator<< (std::ostream& stream,Record* type);

//An overload set consists of function with the same name, whcih are defined in the same scope
struct Overloadset: public PrefixDefinition {
	Overloadset(Function* firstFunction);

	Node* parse(Parser* parser);
	
	std::vector<Function*> functions;
};

struct Function: public PrefixDefinition {

	struct Argument {
		Variable* variable;			 // so that code inside the functions has access to arguments
		//FunctionDef* typeConstraint; // Arithmetic
		//Definition* valueConstraint; // x <- int32 //can be type or function!
		
		Argument(Variable* var);
	};



	Function(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	//Function's properties
	bool resolved(){ return true; }
	TypeExpression* type();

	//Calculate's properties when the type is fully resolved.
	void updateOnSolving();

	TypeExpression* argument;
	TypeExpression* returnType;
	Scope* bodyScope;
	BlockExpression* body;
	Node* (*intrinsicEvaluator)(CallExpression*);
	std::vector<Argument> arguments;
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

struct PrefixOperator : public PrefixDefinition {

	PrefixOperator(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	SymbolID function;
};

struct InfixOperator : public InfixDefinition {

	InfixOperator(SymbolID name,int stickiness,Location& location);

	Node* parse(Parser* parser,Node* node);

	SymbolID function;
};
	


#endif