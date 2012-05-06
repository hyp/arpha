/**
* This module provides a scope class which acts as a dictionary for prefix and infix definitions.
*/
#ifndef ARPHA_SCOPE_H
#define ARPHA_SCOPE_H

#include "../base/memory.h"

struct Parser;
struct Node;

namespace Visibility {
	enum {
		Public = 0,
		Private,	
	};
}

namespace DeclarationType {
	enum {
		OverloadSet = 1
	};
}

struct PrefixDefinition : memory::ManagedDefinition {
	SymbolID id;
	Location location;
	uint8 visibilityMode;
	uint8 declarationType;

	PrefixDefinition(SymbolID name,Location& location);
	virtual Node* parse(Parser* parser) = 0;
};

struct InfixDefinition : memory::ManagedDefinition {
	SymbolID id;
	int stickiness;
	Location location;
	uint8 visibilityMode;
	
	InfixDefinition(SymbolID name,int stickiness,Location& location);
	virtual Node* parse(Parser* parser,Node* node) = 0;
};

struct Node;
struct Type;
struct Variable;
struct Function;
struct ImportedScope;

//Scope resolves symbols to corresponding definitions, which tells parser how to parse the encountered symbol
struct Scope: memory::ManagedDefinition {

	Scope(Scope* parent);

	/**
	*Imports a scope aliased to string alias
	*Options:
	*	Exported import - the import will be imported into scopes importing the current scope
	*	Qualified import - the symbols from the scope are accessed only via the qualified scope alias i.e. foo.bar
	*/
	void import(Scope* scope,const char* alias,bool qualified = false,bool exported = false);


	PrefixDefinition* lookupPrefix(SymbolID name);
	PrefixDefinition* lookupImportedPrefix(SymbolID name);
	PrefixDefinition* containsPrefix(SymbolID name);
	void define(PrefixDefinition* definition);

	InfixDefinition* lookupInfix(SymbolID name);
	InfixDefinition* lookupImportedInfix(SymbolID name);
	InfixDefinition* containsInfix(SymbolID name);
	void define(InfixDefinition* definition);

	Function* resolve(const char* name,Type* argumentType);
	Function* resolveFunction(SymbolID name,const Node* argument);
	void defineFunction(Function* definition);

	Scope* parent;

private:


	std::vector<Scope*> imports;
	std::vector<std::pair<Scope*,std::pair<SymbolID,bool> > > exportedImports;
	std::vector<ImportedScope*> broadcastedImports;

	std::map<SymbolID,PrefixDefinition*> prefixDefinitions;
	std::map<SymbolID,InfixDefinition*> infixDefinitions;
	void reach();
};


#endif