#ifndef DECLARATIONS_H
#define DECLARATIONS_H

struct Node;
struct Parser;
struct BlockExpression;

struct Substitute : PrefixDefinition {
	Substitute(SymbolID name,Location& location) : PrefixDefinition(name,location) {}
	
	Node* parse(Parser* parser);

	Node* expression;
};

struct Type;

struct Variable : PrefixDefinition  {
	Variable(SymbolID name,Location& location);
	void inferType(Type* t);

	Node* parse(Parser* parser);

	Type* type;
};

struct Type: public PrefixDefinition {
private:	
	
	Type* headRecord; ///if this is null, then the type isn't a record
public:
	std::vector<Variable> fields;
	

	Type(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Variable* lookupField(const SymbolID fieldName);
	void add(const Variable& var); //adds a field to the type


	//unique record construction
	static Type* tuple(std::vector<std::pair<SymbolID,Type*>>& fields);

	uint32 size;
	uint32 alignment;
	

	bool isRecord() const { return headRecord != nullptr; }

	//Determines whether two records have the same field types or not
	inline static bool recordsSameTypes(Type* r1,Type* r2){ return r1->headRecord == r2->headRecord; }
private:
	static Type* createRecordType(std::vector<std::pair<SymbolID,Type*>>& record,Type* headRecord = nullptr);
	static Type* findSubRecord(Type* headRecord,std::vector<Type*>& subRecords,std::vector<std::pair<SymbolID,Type*>>& record);
	static Type* findRecord(std::vector<std::pair<SymbolID,Type*>>& record);
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

	Type* argument;
	Type* returnType;
	Scope* bodyScope;
	BlockExpression* body;
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