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
struct Evaluator;
struct DuplicationModifiers;

namespace Visibility {
	enum {
		Public = 0,
		Private,	
	};
}


struct PrefixDefinition : memory::ManagedDefinition {
	SymbolID id;
	Location location;
	uint8 visibilityMode;
	uint8 declarationType;

	PrefixDefinition(SymbolID name,Location& location);

	virtual Node* parse(Parser* parser) = 0;
	//Can it return a reference when it's parsed and resolved at later passes
	virtual Node* createReference(){ return nullptr; }

	virtual PrefixDefinition* duplicate(DuplicationModifiers* mods){ return nullptr; }

	virtual bool isResolved(){ return true; }
	virtual bool resolve(Evaluator* evaluator){ return true; }

	virtual Overloadset* asOverloadset(){ return nullptr; }

};

struct InfixDefinition : memory::ManagedDefinition {
	SymbolID id;
	int stickiness;
	Location location;
	uint8 visibilityMode;
	
	InfixDefinition(SymbolID name,int stickiness,Location& location);
	virtual Node* parse(Parser* parser,Node* node) = 0;
};



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
	Function* resolveFunction(SymbolID name,Node* argument);
	void defineFunction(Function* definition);

	void duplicate(DuplicationModifiers* mods);

	Scope* parent;

	bool isResolved();
	bool resolve(Evaluator* evaluator);

	Function* functionOwner() const;

	Function* _functionOwner;

	size_t numberOfDefinitions() const;

private:

	bool _resolved;
	std::vector<Scope*> imports;
	std::vector<std::pair<Scope*,std::pair<SymbolID,bool> > > exportedImports;
	std::vector<ImportedScope*> broadcastedImports;

	std::map<SymbolID,PrefixDefinition*> prefixDefinitions;
	std::map<SymbolID,InfixDefinition*> infixDefinitions;
	void reach();
};


#endif