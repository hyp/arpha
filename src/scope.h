#ifndef SCOPE_H
#define SCOPE_H

struct Parser;
struct Node;

namespace Visibility {
	enum {
		Public = 0,
		Private
	};
}

namespace DeclarationType {
	enum {
		OverloadSet = 1
	};
}

struct PrefixDefinition {
	SymbolID id;
	Location location;
	uint8 visibilityMode;
	uint8 declarationType;

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

struct Node;
struct Type;
struct Variable;
struct Function;
struct ImportedScope;

//Scope resolves symbols to corresponding definitions, which tells parser how to parse the encountered symbol
struct Scope {

	Scope(Scope* parent);

	struct ImportFlags {
		enum {
			BROADCAST = 0x1,  //Broadcasted import - the import will be imported into scopes importing the current scope
			QUALIFIED = 0x2   //Qualified import - the symbols from the scope are accessed only via the qualified scope alias i.e. foo.bar
		};
	};
	void import(ImportedScope* alias,int flags = 0);

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
	std::vector<ImportedScope*> broadcastedImports;

	std::map<SymbolID,PrefixDefinition*> prefixDefinitions;
	std::map<SymbolID,InfixDefinition*> infixDefinitions;

};

//
struct ImportedScope : PrefixDefinition {
	ImportedScope(SymbolID name,Location& location,Scope* scope);
	Node* parse(Parser* parser);

	Scope* scope;
	int importFlags; //To apply the correct import of broadcasted imports
};

//
struct Overloadset: public PrefixDefinition {
	Overloadset(Function* firstFunction);

	Node* parse(Parser* parser);
	
	std::vector<Function*> functions;
};

#endif