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

struct Variable : PrefixDefinition  {
	Variable(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	//Type(use intrinsincs::types::Inferred for inferred)
	TypeExpression* _type;
	//Type(can be null for inferred type);
	Type* type;
};

struct Type: public PrefixDefinition {
private:	
	
	Type* headRecord; ///if this is null, then the type isn't a record
	bool _resolved;
	
public:
	size_t _size;
	
	std::vector<Variable> fields;
	std::vector<int> extenders;

	Type(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Variable* lookupField(const SymbolID fieldName);
	void add(const Variable& var); //adds a field to the type

	//Type's extenders
	//Returns the id of field which is extended or -1
	int extendsType(Type* type);

	//Type's properties
	bool resolved();
	size_t size();
	
	//Calculate's properties when the type is fully resolved.
	void updateOnSolving();

	


	//unique record construction
	static Type* tuple(std::vector<std::pair<SymbolID,Type*>>& fields);


	bool isRecord() const { return headRecord != nullptr; }

	//Determines whether two records have the same field types or not
	inline static bool recordsSameTypes(Type* r1,Type* r2){ return r1->headRecord == r2->headRecord; }
private:
	static Type* createRecordType(std::vector<std::pair<SymbolID,Type*>>& record,Type* headRecord = nullptr);
	static Type* findSubRecord(Type* headRecord,std::vector<Type*>& subRecords,std::vector<std::pair<SymbolID,Type*>>& record);
	static Type* findRecord(std::vector<std::pair<SymbolID,Type*>>& record);
};

std::ostream& operator<< (std::ostream& stream,Type* type);

struct TypeBase : PrefixDefinition {
public:
	TypeBase(SymbolID name,Location& location) : PrefixDefinition(name,location) {}

	virtual size_t size() const = 0;

};

//An integral type
struct IntegerType: TypeBase {
	IntegerType(SymbolID name,Location& location);

	size_t size() const;
	bool isValid(BigInt& value) const;
	bool isUnsigned() const;
	Node* assignableFrom(Node* expression,IntegerType* type);

	Node* parse(Parser* parser);
	BigInt max,min;
private:
	
	size_t _size;
	bool _unsigned;
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
	Type* type();

	//Calculate's properties when the type is fully resolved.
	void updateOnSolving();

	Type* argument;
	Type* returnType;
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