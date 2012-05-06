#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"

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
	/*Type* argumentType = argument->returnType();
	Function *implicitMatch = nullptr,*inferMatch = nullptr,*exprMatch = nullptr;//lastResort
	int extender;
	for(auto i=overloads.begin();i!=overloads.end();++i){
		if(enforcePublic && (*i)->visibilityMode != Visibility::Public) continue;
		auto argumentType = argument->returnType();
		if((*i)->argument == argumentType){
			//debug("-d");
			results.push_back(*i);
		}
		else if(argumentType->isRecord() && Type::recordsSameTypes((*i)->argument, argumentType) ){
			//debug("-dr");
			results.push_back(*i);
		}
		else if((extender = argumentType->extendsType((*i)->argument)) != -1){
			debug("e-m");
			results.push_back(*i);
		}
		else if((*i)->argument == compiler::expression){ /*debug("-c"); exprMatch = *i; }
	}
	if(exprMatch && results.size()==0) results.push_back( exprMatch );//TODO careful with imports*/
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

void Scope::reach(){
	memory::reach(parent);
	for(auto i=imports.begin();i!=imports.end();++i) memory::reach(*i);
	for(auto i=broadcastedImports.begin();i!=broadcastedImports.end();++i) memory::reach(*i);
	for(auto i=prefixDefinitions.begin();i!=prefixDefinitions.end();++i) memory::reach(i->second);
	for(auto i=infixDefinitions.begin();i!=infixDefinitions.end();++i) memory::reach(i->second);
}

	
	