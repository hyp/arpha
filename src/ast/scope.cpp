#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/system.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "evaluate.h"
#include "../intrinsics/types.h"

PrefixDefinition::PrefixDefinition(SymbolID name,Location& location){
	this->id = name;this->location = location;
	visibilityMode = Visibility::Public;
}
PrefixDefinition* PrefixDefinition::copyProperties(PrefixDefinition* dest){
	//Other stuff copied on creation
	dest->visibilityMode = visibilityMode;
	return dest;
}
InfixDefinition::InfixDefinition(SymbolID name,int stickiness,Location& location){
	this->id = name;this->stickiness = stickiness;this->location = location;
	visibilityMode = Visibility::Public;
}
InfixDefinition* InfixDefinition::copyProperties(InfixDefinition* dest){
	//Other stuff copied on creation
	dest->visibilityMode = visibilityMode;
	return dest;
}

Scope::Scope(Scope* parent) : _functionOwner(parent ? parent->_functionOwner : nullptr) {
	this->parent = parent;
	_resolved = false;//TODO The 2nd round of resolving is not always necessary !
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

#define LOOKUP_IMPORTED(t,c) \
	auto var = t##Definitions.find(name); \
	if (var != t##Definitions.end() && (int)var->second->visibilityMode == Visibility::Public) return var->second; \
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
			if(def) error(d->location,"'%s' Symbol import conflict",d->id); \
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
				error(d->location,"'%s' Symbol import conflict",d->id); 
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

Function* errorOnMultipleMatches(std::vector<Function*>& results){
	//TODO
	error(Location(),"multiple matches possible!");
	return nullptr;
}

Function* Scope::resolve(const char* name,Type* argumentType){
	//HACK create a fake constant node of type argumentType
	//ConstantExpression argument;
	//argument.type = argumentType;
	//argument._isLiteral = false;
	//return resolveFunction(name,&argument);
	return nullptr;
}
Function* Scope::resolveFunction(Evaluator* evaluator,SymbolID name,Node* argument){
	std::vector<Function*> results;
	//step 1 - check current scope for matching function
	if(auto hasDef = containsPrefix(name)){
		if(auto os = hasDef->asOverloadset()){
			evaluator->findMatchingFunctions(os->functions,results,argument);
			if(results.size() == 1) return results[0];
			else if(results.size()>1) return errorOnMultipleMatches(results);
		}
	}
	//step 2 - check imported scopes for matching function
	if(imports.size()){
		std::vector<Function*> overloads;
		for(auto i = imports.begin();i!=imports.end();++i){ 
			if(auto hasDef = (*i)->containsPrefix(name)){
				if(auto os = hasDef->asOverloadset()) overloads.insert(overloads.end(),os->functions.begin(),os->functions.end()); 
			}
		}
		evaluator->findMatchingFunctions(overloads,results,argument,true);
		if(results.size() == 1) return results[0];
		else if(results.size()>1) return errorOnMultipleMatches(results);
	}
	//step 3 - check parent scope
	if(parent) return parent->resolveFunction(evaluator,name,argument);
	return nullptr;
}

void Scope::defineFunction(Function* definition){
	if(auto alreadyDefined = containsPrefix(definition->id)){
		if(auto os = alreadyDefined->asOverloadset()){
			os->push_back(definition);
		}
		else {
			error(definition->location,"'%s' is already (prefix)defined in the current scope",definition->id);//TODO better message
			return;
		}	
	}
	else {
		auto os = new Overloadset(definition);
		prefixDefinitions[definition->id] = os;
	}
}

bool Scope::isResolved(){
	return _resolved;
}

bool Scope::resolve(Evaluator* evaluator){
	debug("RSLV scope");
	_resolved = true;
	for(auto i = prefixDefinitions.begin();i!=prefixDefinitions.end();i++){
		if((!dynamic_cast<Argument*>((*i).second)) && !(*i).second->isResolved()){
			if(!(*i).second->resolve(evaluator)) _resolved = false;
		}
	}
	return _resolved;
}

void Scope::duplicate(DuplicationModifiers* mods){
	for(auto i = prefixDefinitions.begin();i!=prefixDefinitions.end();i++){
		
		if(auto os = (*i).second->asOverloadset()){
			if(auto def  = mods->target->containsPrefix((*i).first)){
				if(auto destOs = def->asOverloadset()){
					os->mergedDuplicate(mods,destOs);
					continue;
				}
			}
		}
		if(auto dup = (*i).second->duplicate(mods)){
			mods->target->define(dup);
		}else{
			if(!dynamic_cast<Argument*>((*i).second))
				error(mods->location,"Can't duplicate some definition %s!",(*i).first);
		}
	}
}

void Scope::reach(){
	memory::reach(parent);
	for(auto i=imports.begin();i!=imports.end();++i) memory::reach(*i);
	for(auto i=broadcastedImports.begin();i!=broadcastedImports.end();++i) memory::reach(*i);
	for(auto i=prefixDefinitions.begin();i!=prefixDefinitions.end();++i) memory::reach(i->second);
	for(auto i=infixDefinitions.begin();i!=infixDefinitions.end();++i) memory::reach(i->second);
}

	
	