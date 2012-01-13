#include "common.h"
#include "scope.h"
#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

void Scope::importAllDefinitions(Location& location,Scope* scope){
	for(auto i = scope->prefixDefinitions.begin(); i != scope->prefixDefinitions.end() ; ++i){
		auto c = importedPrefixDefinitions.find((*i).first);
		if(c != importedPrefixDefinitions.end()){
			error(location,"definition '%s' is already (p)imported!",(*i).first);
		}
		else{
			debug("Scope p-importing definition '%s'",(*i).first);
			importedPrefixDefinitions[(*i).first]=(*i).second;
		}
	}

	for(auto i = scope->infixDefinitions.begin(); i != scope->infixDefinitions.end() ; ++i){
		auto c = importedInfixDefinitions.find((*i).first);
		if(c != importedInfixDefinitions.end()){
			error(location,"definition '%s' is alreday (i)imported!",(*i).first);
		}
		else{
			debug("Scope i-importing definition '%s'",(*i).first);
			importedInfixDefinitions[(*i).first]=(*i).second;
		}
	}
}
void Scope::importAlias(Location& location,Scope* scope,SymbolID alias){
	auto c = containsPrefix(alias);
	if(c) error(location,"Symbol '%s' is already defined! Failed to alias an imported scope as '%s'.",alias,alias);
	else definePrefix(location,alias,nullptr,scope);
}

#define LOOKUP(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end()) return &var->second; \
	var = imported##c##Definitions.find(name); \
	if(var != imported##c##Definitions.end()) return &var->second; \
	if(parent) return parent->lookup##c(name); \
	return nullptr

PrefixDefinition* Scope::lookupPrefix(SymbolID name){
	LOOKUP(prefix,Prefix);
}
InfixDefinition* Scope::lookupInfix(SymbolID name){
	LOOKUP(infix,Infix);
}

#define CONTAINS(t) \
	auto var = t.find(name);			   \
	if (var != t.end()) return &var->second; \
	return nullptr

PrefixDefinition* Scope::containsPrefix(SymbolID name){ CONTAINS(prefixDefinitions); }
InfixDefinition* Scope::containsInfix(SymbolID name)  { CONTAINS(infixDefinitions);  }

void Scope::definePrefix(Location& location,SymbolID name,Node* (*parselet)(PrefixDefinition*,Parser*),void* data){
	auto alreadyDefined = containsPrefix(name);
	if(alreadyDefined) error(location,"%s is already (prefix)defined in the current scope",name);
	PrefixDefinition def = { parselet,data };
	prefixDefinitions[name] = def;
}
void Scope::defineInfix(Location& location,SymbolID name,int stickiness,Node* (*parselet)(InfixDefinition*,Parser*,Node*),void* data){
	auto alreadyDefined = containsInfix(name);
	if(alreadyDefined) error(location,"%s is already (infix)defined in the current scope",name);
	InfixDefinition def = { stickiness,parselet,data };
	infixDefinitions[name] = def;
}
