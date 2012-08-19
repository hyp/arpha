#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/system.h"
#include "../compiler.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "resolve.h"
#include "../intrinsics/types.h"


Scope::Scope(Scope* parent) : _functionOwner(parent ? parent->_functionOwner : nullptr) {
	this->parent = parent;
	parent2 = nullptr;
	precedenceProperty = nullptr;
	importsArphaIntrinsic = false;
	importsArphaExternal = false;
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
	if(parent && (def = parent->lookup##c(name))){ \
		return def; \
	} \
	if(parent2) return parent2->lookup##c(name);  \
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
	if(parent && (def = parent->lookupPrefix(name))){
		return def;
	}
	if(parent2) return parent2->lookupPrefix(name); 
	return nullptr;
}

PrefixDefinition* Scope::lookup(Resolver* resolver,UnresolvedSymbol* node){
	Scope* lookup = this;
	Scope* lookupP2 = nullptr;
	PrefixDefinition* def = nullptr; 
	auto name = node->symbol;

	while(true){
		auto r = lookup->prefixDefinitions.find(name);
		if (r != lookup->prefixDefinitions.end() && !r->second->isHiddenBeforeDeclaration(resolver->_pass)){
			return r->second;
		}

		//Check the imports
		if(lookup->imports.size()){
			PrefixDefinition* def = nullptr; 
			for(auto i = lookup->imports.begin();i!=lookup->imports.end();++i){ 
				auto d = (*i)->lookupImportedPrefix(name); 
				if(d && !d->isHiddenBeforeDeclaration(resolver->_pass)){
					if(def && !(def->asOverloadset() && d->asOverloadset()) ){
						error(node,"Ambiguos import: symbol '%s' is defined in more than one scope.",name);//TODO
					}
					else def = d; 
				}
			}
			if(def) return def;
		}

		if(lookup->parent2) lookupP2 = lookup->parent2;
		lookup = lookup->parent;
		if(!lookup){
			if(lookupP2) lookup = lookupP2;
			else break;
		}
	}
	return nullptr;
}

InfixDefinition* Scope::lookupInfix(SymbolID name){
	LOOKUP(infix,Infix);
}


#define ABS(x) (((x) < 0)   ? -(x) : (x))

/** Returns the absolute similarity score for 's1' and 's2' */
static int similarity(const char* s1, const char* s2)
{
	int score = 0;
	size_t len1 = strlen(s1), len2 = strlen(s2), len = len1 > len2 ?  len1 : len2;
	size_t i = 0;

	while ((len--) > 0) {
		int abs;

		if (len1 >= i && len2 < i) {
			int x = 0 - s1[i];
			score += ABS(x);
		} else if (len2 >= i && len1 < i) {
			int x = 0 - s2[i];
			score += ABS(x);
		} else {
			abs = ABS(strcmp(s1 + i, s2 + i));
			score += abs;
			while (s1[i] == s2[i] && len > 0) {
				++i;
				--len;
			}
			++i;
		}
	}
	return score;
}

#undef ABS

PrefixDefinition* Scope::lookupBestSimilar(SymbolID name,int threshold){
	int minWeight = threshold;
	PrefixDefinition* def = nullptr;

	for(auto i = prefixDefinitions.begin();i!=prefixDefinitions.end();i++){
		auto weight = similarity((*i).first.ptr(),name.ptr());
		if(weight < minWeight){
			minWeight = weight;
			def= (*i).second;
		}
	}

	if(parent){
		if(auto p = parent->lookupBestSimilar(name,minWeight)) return p;
	}
	if(parent2){
		if(auto p = parent2->lookupBestSimilar(name,minWeight)) return p;
	}
	return def;
}

#define CONTAINS(t) \
	auto var = t.find(name);			    \
	if (var != t.end()) return var->second; \
	return nullptr

PrefixDefinition* Scope::containsPrefix(SymbolID name){ CONTAINS(prefixDefinitions); }
InfixDefinition* Scope::containsInfix(SymbolID name)  { CONTAINS(infixDefinitions);  }
Overloadset* Scope::containsOverloadset(SymbolID name){
	auto var = prefixDefinitions.find(name);
	if (var != prefixDefinitions.end()){
		if(auto os = var->second->asOverloadset()) return os;
	}
	return nullptr;
}

void Scope::define(PrefixDefinition* definition){
	auto id = definition->label();
	auto alreadyDefined = containsPrefix(id);
	if(alreadyDefined){
		if(alreadyDefined->asOverloadset()) error(definition,"The name '%s' is already used in the current scope for function '%s'.",id,id);
		else error(definition,"The name '%s' is already used in the current scope by '%s'.",id,alreadyDefined);
	}
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

