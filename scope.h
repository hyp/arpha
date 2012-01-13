#ifndef SCOPE_H
#define SCOPE_H

struct Definition;
struct Parser;
struct Node;

struct PrefixDefinition {
	Node* (*parse)(PrefixDefinition*,Parser*);
	void* data;
};

struct InfixDefinition {
	int stickiness;
	Node* (*parse)(InfixDefinition*,Parser*,Node*);
	void* data;
};



struct Scope {

	Scope(Scope* parent);
	void define(Definition* definition);
	Definition* lookup(SymbolID name);
	Definition* contains(SymbolID name);

	//import all importable definitions to this scope
	void importAllDefinitions(Location& location,Scope* scope);
	//define scope alias in this scope
	void importAlias(Location& location,Scope* scope,SymbolID alias);

	PrefixDefinition* lookupPrefix(SymbolID name);
	PrefixDefinition* containsPrefix(SymbolID name);
	void definePrefix(Location& location,SymbolID name,Node* (*parselet)(PrefixDefinition*,Parser*),void* data);

	InfixDefinition* lookupInfix(SymbolID name);
	InfixDefinition* containsInfix(SymbolID name);
	void defineInfix(Location& location,SymbolID name,int stickiness,Node* (*parselet)(InfixDefinition*,Parser*,Node*),void* data);

	Scope* parent;
	Definition* owner;
private:

	std::map<SymbolID,PrefixDefinition> prefixDefinitions;
	std::map<SymbolID,InfixDefinition> infixDefinitions;

	std::map<SymbolID,PrefixDefinition> importedPrefixDefinitions;
	std::map<SymbolID,InfixDefinition> importedInfixDefinitions;

	std::map<SymbolID,Definition*>  definitions;
};

#endif