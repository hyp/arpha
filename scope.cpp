#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

PrefixDefinition::PrefixDefinition(SymbolID name,Location& location){
	this->id = name;this->location = location;
	visibilityMode = Visibility::Public;
}
InfixDefinition::InfixDefinition(SymbolID name,int stickiness,Location& location){
	this->id = name;this->stickiness = stickiness;this->location = location;
	visibilityMode = Visibility::Public;
}

void Scope::importAllDefinitions(Location& location,Scope* scope){
	for(auto i = scope->prefixDefinitions.begin(); i != scope->prefixDefinitions.end() ; ++i){
		if((*i).second->visibilityMode == Visibility::Public){
			auto c = importedPrefixDefinitions.find((*i).first);
			if(c != importedPrefixDefinitions.end()){
				error(location,"definition '%s' is already (p)imported!",(*i).first);
			}
			else{
				debug("Scope p-importing definition '%s'",(*i).first);
				importedPrefixDefinitions[(*i).first]=(*i).second;
			}
		}
	}

	for(auto i = scope->infixDefinitions.begin(); i != scope->infixDefinitions.end() ; ++i){
		if((*i).second->visibilityMode == Visibility::Public){
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
}
void Scope::importAlias(Location& location,Scope* scope,SymbolID alias){
	auto c = containsPrefix(alias);
	if(c) error(location,"Symbol '%s' is already defined! Failed to alias an imported scope as '%s'.",alias,alias);
	else void; /*definePrefix(location,alias,nullptr,scope);//TODO parser func*/
}

#define LOOKUP(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end()) return var->second; \
	var = imported##c##Definitions.find(name); \
	if(var != imported##c##Definitions.end()) return var->second; \
	if(parent) return parent->lookup##c(name); \
	return nullptr

PrefixDefinition* Scope::lookupImportedPrefix(SymbolID name){
	auto var = prefixDefinitions.find(name); 
	if (var != prefixDefinitions.end() && var->second->visibilityMode == Visibility::Public) return var->second;
	return nullptr;
}
PrefixDefinition* Scope::lookupPrefix(SymbolID name){
	LOOKUP(prefix,Prefix);
}
InfixDefinition* Scope::lookupInfix(SymbolID name){
	LOOKUP(infix,Infix);
}

#define CONTAINS(t) \
	auto var = t.find(name);			   \
	if (var != t.end()) return var->second; \
	return nullptr

PrefixDefinition* Scope::containsPrefix(SymbolID name){ CONTAINS(prefixDefinitions); }
InfixDefinition* Scope::containsInfix(SymbolID name)  { CONTAINS(infixDefinitions);  }

void Scope::define(PrefixDefinition* definition){
	auto alreadyDefined = containsPrefix(definition->id);
	if(alreadyDefined) error(definition->location,"'%s' is already (prefix)defined in the current scope",definition->id);
	else prefixDefinitions[definition->id] = definition;
}
void Scope::define(InfixDefinition* definition){
	auto alreadyDefined = containsInfix(definition->id);
	if(alreadyDefined) error(definition->location,"'%s' is already (infix)defined in the current scope",definition->id);
	else infixDefinitions[definition->id] = definition;
}
