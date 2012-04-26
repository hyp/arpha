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
	declarationType = 0;
}
InfixDefinition::InfixDefinition(SymbolID name,int stickiness,Location& location){
	this->id = name;this->stickiness = stickiness;this->location = location;
	visibilityMode = Visibility::Public;
}

Scope::Scope(Scope* parent){
	this->parent = parent;
}

void Scope::import(ImportedScope* alias,int flags){
	auto c = containsPrefix(alias->id);
	if(c){
		error(alias->location,"Symbol '%s' is already defined! Failed to import scope '%s'.",alias->id,alias->id);
		return;
	}
	debug("Importing scope %s with flags %d",alias->id,flags);
	if(!(flags & ImportFlags::QUALIFIED)){
		imports.push_back(alias->scope);
		//import broadcasted scopes
		for(auto i=alias->scope->broadcastedImports.begin();i!=alias->scope->broadcastedImports.end();++i){
			debug("Broadcasting a scope aliased as %s",(*i)->id);
			import(*i,(*i)->importFlags);
		}
	}
	if(flags & ImportFlags::BROADCAST){
		debug("defining public import");
		broadcastedImports.push_back(alias);
		alias->importFlags = flags & (~ImportFlags::BROADCAST); //retrieve other flags
	}
	define(alias);
}

#define LOOKUP_IMPORTED(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end() && var->second->visibilityMode == Visibility::Public) return var->second; \
	if(parent) return parent->lookupImported##c(name); \
	return nullptr

PrefixDefinition* Scope::lookupImportedPrefix(SymbolID name){
	LOOKUP_IMPORTED(prefix,Prefix);
}

InfixDefinition* Scope::lookupImportedInfix(SymbolID name){
	LOOKUP_IMPORTED(infix,Infix);
}

#define LOOKUP(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end()) return var->second; \
	c##Definition* def = nullptr; \
	for(auto i = imports.begin();i!=imports.end();++i){ \
		auto d = (*i)->lookupImported##c(name); \
		if(d){ \
			if(def) error(d->location,"'%s' Symbol import conflict",d->id); /*TODO os conflict resolvement*/\
			else def = d; \
		} \
	} \
	if(def) return def;\
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

void findMatches(std::vector<Function*>& overloads,std::vector<Function*>& results,const Node* argument,bool enforcePublic = false){
	Type* argumentType = returnType(argument);
	Function *implicitMatch = nullptr,*inferMatch = nullptr,*exprMatch = nullptr;//lastResort

	for(auto i=overloads.begin();i!=overloads.end();++i){
		if(enforcePublic && (*i)->visibilityMode != Visibility::Public) continue;
		/*else*/ if((*i)->argument == compiler::expression){ debug("c"); exprMatch = *i; }
	}
	if(exprMatch && results.size()==0) results.push_back( exprMatch );//TODO careful with imports
}

Function* errorOnMultipleMatches(std::vector<Function*>& results){
	//TODO
	error(Location(),"multiple matches possible!");
	return nullptr;
}

Function* Scope::resolve(const char* name,Type* argumentType){
	auto argument = new ConstantExpression;
	argument->type = argumentType;
	argument->_isLiteral = false;
	//HACK create a fake constant node of type argumentType
	return resolveFunction(name,argument);
}
Function* Scope::resolveFunction(SymbolID name,const Node* argument){
	std::vector<Function*> results;
	//step 1 - check current scope for matching function
	if(auto os = containsPrefix(name)){
		if(os->declarationType == DeclarationType::OverloadSet){
			findMatches(((Overloadset*)os)->functions,results,argument);
			if(results.size() == 1) return results[0];
			else if(results.size()>1) return errorOnMultipleMatches(results);
		}
	}
	//step 2 - check imported scopes for matching function
	if(imports.size()){
		std::vector<Function*> overloads;
		for(auto i = imports.begin();i!=imports.end();++i){ 
			if(auto os = (*i)->containsPrefix(name)){
				if(os->declarationType == DeclarationType::OverloadSet) overloads.insert(overloads.end(),((Overloadset*)os)->functions.begin(),((Overloadset*)os)->functions.end()); 
			}
		}
		findMatches(overloads,results,argument,true);
		if(results.size() == 1) return results[0];
		else if(results.size()>1) return errorOnMultipleMatches(results);
	}
	//step 3 - check parent scope
	if(parent) return parent->resolveFunction(name,argument);
	return nullptr;
}

void Scope::defineFunction(Function* definition){

	Overloadset* set;

	auto alreadyDefined = containsPrefix(definition->id);
	if(alreadyDefined){
		if(alreadyDefined->declarationType != DeclarationType::OverloadSet){ 
			error(definition->location,"'%s' is already (prefix)defined in the current scope",definition->id);
			return;
		}
		set = (Overloadset*)alreadyDefined;
		//todo safe add function
		set->functions.push_back(definition);
		
	}
	else {
		set = new Overloadset(definition);
		prefixDefinitions[definition->id] = set;
	}
}

Overloadset::Overloadset(Function* firstFunction) : PrefixDefinition(firstFunction->id,firstFunction->location) {
	declarationType = DeclarationType::OverloadSet;
	visibilityMode = Visibility::Public;//!important // TODO module a type foo, module b private def foo() //<-- conflict
	functions.push_back(firstFunction);
}

ImportedScope::ImportedScope(SymbolID name,Location& location,Scope* scope) : PrefixDefinition(name,location) {
	this->scope = scope;
	visibilityMode = Visibility::Private;
}

	
	