#ifndef DECLARATIONS_H
#define DECLARATIONS_H

struct Node;
struct Parser;

struct Substitute : PrefixDefinition {
	Substitute(SymbolID name,Location& location) : PrefixDefinition(name,location) {}
	
	Node* parse(Parser* parser);

	Node* expression;
};

struct Type;

struct Variable : PrefixDefinition  {
	Variable(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Type* type;
};

struct Type: public PrefixDefinition {

	Type(SymbolID name,Location& location);

	Node* parse(Parser* parser);

	Variable* lookupField(const SymbolID fieldName);
	void add(Variable& var);
	
	//implicit type cast
	bool canAssignFrom(Type* other);

	//global tuple constructs
	static std::vector<Type*> tuples;
	static Type* tuple(std::vector<std::pair<SymbolID,Type*>>& fields);

	uint32 size;
	uint32 alignment;
	bool isTuple;
private:
	std::vector<Variable> fields;
};



#endif