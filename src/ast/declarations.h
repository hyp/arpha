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
	
	
public:

	struct Field {
		SymbolID name;
		InferredUnresolvedTypeExpression type;
		bool isExtending; //a field aliased as this

		Field(SymbolID id,TypeExpression* typ) : name(id),type(typ),isExtending(false) {}
	};
	bool _resolved;
	size_t _size;

	
	std::vector<Field> fields;
	//A single reference expression
	TypeExpression _reference;
	inline TypeExpression* reference(){ return &_reference; }

	Record(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Field* lookupField(const SymbolID fieldName);
	void add(const Field& var); //adds a field to the type

	//Type's properties
	bool resolved();
	size_t size() const;
	
	//Calculate's properties when the type is fully resolved.
	void updateOnSolving();

	


	//unique anonymous record construction
	static Record* findAnonymousRecord(std::vector<Field>& record);

	//An anonymous record
	inline bool isAnonymous() const { return headRecord != nullptr; }
	//Determines whether two records have the same field types or not
	inline static bool anonymousRecordsSameTypes(Record* r1,Record* r2){ return r1->headRecord == r2->headRecord; }
private:
	static Record* createRecordType(std::vector<Field>& record,Record* headRecord = nullptr);
	static Record* findSubRecord(Record* headRecord,std::vector<Record*>& subRecords,std::vector<Field>& record);
	
};



std::ostream& operator<< (std::ostream& stream,Record* type);

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