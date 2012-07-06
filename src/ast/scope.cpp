#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/system.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "resolve.h"
#include "../intrinsics/types.h"


Scope::Scope(Scope* parent) : _functionOwner(parent ? parent->_functionOwner : nullptr) {
	this->parent = parent;
	externalFunction = 0;
	precedenceProperty = nullptr;
}
void Scope::setParent(Scope* scope){
	if(!_functionOwner) _functionOwner= scope ? scope->_functionOwner : nullptr;
	this->parent = scope;
}
Scope*   Scope::moduleScope(){
	if(!parent) return this;
	else return parent->moduleScope();
}
Function* Scope::functionOwner() const {
	return _functionOwner;
}
size_t Scope::numberOfDefinitions() const {
	return prefixDefinitions.size() + infixDefinitions.size();
}
void Scope::import(Scope* scope,const char* alias,bool qualified,bool exported){
	//alias in a form of single file
	const char* path = alias;
	ImportedScope* importTree = nullptr;
	SymbolID componentName;
	while(true){
		
		auto pair = System::path::firstComponent(&path);
		componentName = SymbolID(pair.first,pair.second);
		//Import tree is null at first round
		if(!importTree){
			auto c = containsPrefix(componentName);
			if(c){
				if(!(importTree = dynamic_cast<ImportedScope*>(c))){
					error(Location(),"Symbol '%s' is already defined in the current scope! Failed to import module '%s'.",componentName,alias);
					return;
				}
			}
			if(!importTree){
				importTree = new ImportedScope(componentName,Location());
				define(importTree);
			}
		}
		// A subsequent iteration
		else{
			auto var = importTree->importTree.find(componentName);
			if (var != importTree->importTree.end()) importTree = var->second;
			else{
				auto newImportTree = new ImportedScope(componentName,Location());
				importTree->importTree[componentName] = newImportTree;
				importTree = newImportTree;
			}
		}

		if(*path == '\0') break;
		else path++;	
	}

	//Found last component!
	importTree->scope = scope;
	debug("Importing scope %s with flags %s,%s",componentName,qualified?"qualified":"_",exported?"exported":"_");
	//Qualified imports can only access the imported scope via alias
	if(!qualified){
		imports.push_back(scope);
		//import exported scopes
		for(auto i=scope->exportedImports.begin();i!=scope->exportedImports.end();++i){
			std::string fullName = std::string(alias) + '/' + std::string((*i).second.first.ptr());
			import((*i).first,fullName.c_str(),(*i).second.second,false);
		}
	}
	if(exported){
		debug("defining public import %s",alias);
		exportedImports.push_back(std::make_pair(scope,std::make_pair(alias,qualified)));
	}
}
void Scope::import(Scope* scope){
	imports.push_back(scope);
}

#define LOOKUP_IMPORTED(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end() && (int)var->second->isPublic()) return var->second; \
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
			if(def) error(d,"'%s' Symbol import conflict",d->label()); \
			else def = d; \
		} \
	} \
	if(def) return def;\
	if(parent) return parent->lookup##c(name); \
	return nullptr


PrefixDefinition* Scope::lookupPrefix(SymbolID name){
	auto var = prefixDefinitions.find(name);
	if (var != prefixDefinitions.end()) return var->second;
	PrefixDefinition* def = nullptr; 
	for(auto i = imports.begin();i!=imports.end();++i){ 
		auto d = (*i)->lookupImportedPrefix(name); 
		if(d){ 
			if(def && !(def->asOverloadset() && d->asOverloadset()) ) 
				error(d->location(),"'%s' Symbol import conflict",d->label()); 
			else def = d; 
		} 
	} 
	if(def) return def;
	if(parent) return parent->lookupPrefix(name); 
	return nullptr;
}
InfixDefinition* Scope::lookupInfix(SymbolID name){
	LOOKUP(infix,Infix);
}

#define CONTAINS(t) \
	auto var = t.find(name);			    \
	if (var != t.end()) return var->second; \
	return nullptr

PrefixDefinition* Scope::containsPrefix(SymbolID name){ CONTAINS(prefixDefinitions); }
InfixDefinition* Scope::containsInfix(SymbolID name)  { CONTAINS(infixDefinitions);  }

void Scope::define(PrefixDefinition* definition){
	auto id = definition->label();
	auto alreadyDefined = containsPrefix(id);
	if(alreadyDefined) error(definition,"'%s' is already (prefix)defined in the current scope",id);
	else prefixDefinitions[id] = definition;
}
void Scope::define(InfixDefinition* definition){
	auto id = definition->label();
	auto alreadyDefined = containsInfix(id);
	if(alreadyDefined) error(definition,"'%s' is already (infix)defined in the current scope",id);
	else infixDefinitions[id] = definition;
}

void Scope::defineFunction(Function* definition){
	if(auto alreadyDefined = containsPrefix(definition->label())){
		if(auto os = alreadyDefined->asOverloadset()) os->push_back(definition);
		else error(definition,"'%s' is already (prefix)defined in the current scope",definition->label());//TODO better message
	}
	else prefixDefinitions[definition->label()] = new Overloadset(definition);
}

void Scope::remove(PrefixDefinition* definition){
	auto id = definition->label();
	assert(containsPrefix(id));
	prefixDefinitions.erase(id);
}

