/**
* This module provides a scope class which acts as a dictionary for prefix and infix definitions.
*/
#ifndef ARPHA_SCOPE_H
#define ARPHA_SCOPE_H

#include "../base/memory.h"

struct Parser;
struct Node;
struct Type;
struct Variable;
struct Overloadset;
struct Function;
struct ImportedScope;
struct Resolver;
struct DuplicationModifiers;
struct PrefixDefinition;
struct InfixDefinition;

//Scope resolves symbols to corresponding definitions, which tells parser how to parse the encountered symbol
struct Scope {

	Scope(Scope* parent);

	/**
	*Imports a scope aliased to string alias
	*Options:
	*	Exported import - the import will be imported into scopes importing the current scope
	*	Qualified import - the symbols from the scope are accessed only via the qualified scope alias i.e. foo.bar
	*/
	void import(Scope* scope,const char* alias,bool qualified = false,bool exported = false);
	void import(Scope* scope);

	PrefixDefinition* lookupPrefix(SymbolID name);
	PrefixDefinition* lookupImportedPrefix(SymbolID name);
	PrefixDefinition* containsPrefix(SymbolID name);
	void define(PrefixDefinition* definition);
	void remove(PrefixDefinition* definition);

	InfixDefinition* lookupInfix(SymbolID name);
	InfixDefinition* lookupImportedInfix(SymbolID name);
	InfixDefinition* containsInfix(SymbolID name);
	void define(InfixDefinition* definition);

	void defineFunction(Function* definition);

	void setParent(Scope* scope);

	Scope* moduleScope();

	Scope* parent;

	Function* functionOwner() const;

	Function* _functionOwner;

	size_t numberOfDefinitions() const;

	std::map<SymbolID,PrefixDefinition*> prefixDefinitions;
	std::map<SymbolID,InfixDefinition*> infixDefinitions;
	std::vector<Scope*> imports;

	//definition modifier commands(visibility mode, etc)
	Node* precedenceProperty;
	bool importsArphaIntrinsic;
private:
	
	std::vector<std::pair<Scope*,std::pair<SymbolID,bool> > > exportedImports;
	std::vector<ImportedScope*> broadcastedImports;

};


#endif