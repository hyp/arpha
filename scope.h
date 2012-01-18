#ifndef SCOPE_H
#define SCOPE_H

struct Definition;
struct Parser;
struct Node;

namespace Visibility {
	enum {
		Public = 0,
		Private
	};
}

struct PrefixDefinition {
	SymbolID id;
	Location location;
	uint8 visibilityMode;

	PrefixDefinition(SymbolID name,Location& location);
	virtual Node* parse(Parser* parser) = 0;
};

struct InfixDefinition {
	SymbolID id;
	int stickiness;
	Location location;
	uint8 visibilityMode;
	
	InfixDefinition(SymbolID name,int stickiness,Location& location);
	virtual Node* parse(Parser* parser,Node* node) = 0;
};

struct Variable;
struct FunctionDef;
struct ImportedScope;

//Scope resolves symbols to corresponding definitions, which tells parser how to parse the encountered symbol
struct Scope {

	Scope(Scope* parent);
	void define(Definition* definition);
	Definition* lookup(SymbolID name);
	Definition* contains(SymbolID name);

	struct ImportFlags {
		enum {
			PUBLIC = 0x1,        //public import - the import will be imported into scopes importing the current scope
			FORCE_ALIAS = 0x2    //force alias - the symbols from the scope are accessed only via the scope alias i.e. foo.bar
		};
	};
	void import(ImportedScope* alias,int flags = 0);

	//import all importable definitions to this scope
	void importAllDefinitions(Location& location,Scope* scope);

	PrefixDefinition* lookupPrefix(SymbolID name);
	PrefixDefinition* lookupImportedPrefix(SymbolID name);
	PrefixDefinition* containsPrefix(SymbolID name);
	void define(PrefixDefinition* definition);

	InfixDefinition* lookupInfix(SymbolID name);
	InfixDefinition* containsInfix(SymbolID name);
	void define(InfixDefinition* definition);

	void defineFunction(FunctionDef* definition);

	Scope* parent;
	Definition* owner;

private:

	std::map<SymbolID,PrefixDefinition*> prefixDefinitions;
	std::map<SymbolID,InfixDefinition*> infixDefinitions;

	std::map<SymbolID,PrefixDefinition*> importedPrefixDefinitions;
	std::map<SymbolID,InfixDefinition*> importedInfixDefinitions;

	std::map<SymbolID,Definition*>  definitions;
};

//
struct ImportedScope : PrefixDefinition {
	ImportedScope(SymbolID name,Location& location,Scope* scope);
	Node* parse(Parser* parser);

	Scope* scope;
};

#endif