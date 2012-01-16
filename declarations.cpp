#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

//variable
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	type = compiler::inferred;
}

//type

Type::Type(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	size = 0;
	isTuple = false;
}

bool Type::canAssignFrom(Type* other){
	/*if(other->isTuple){
		//check if tuple has the same underlying structure
		if(fields.size() != other->fields.size()) return false;
		for(size_t i =0;i<fields.size();i++){
			if(!fields[i].type->canAssignFrom(other->fields[i].type)) return false;
		}
		return true;
	}else{
		if( ( other == arpha::constant ) && arpha::isAssignableFromConstant(this)){
			debug("%s can assign from constant!",this->id);
			return true;
		}
	}*/
	return false;
}

std::vector<Type*> Type::tuples;

Variable* Type::lookupField(const SymbolID fieldName){
	for(auto i = fields.begin();i!=fields.end();++i){
		if( (*i).id == fieldName ) return i._Ptr;
	}
	return nullptr;
}

void Type::add(Variable& var){
	fields.push_back(var);
	size += var.type->size;
}

Type* Type::tuple(std::vector<std::pair<SymbolID,Type*>>& fields){
	assert(fields.size() > 1);
	//check if such tuple already exists
	for(std::vector<Type*>::iterator i=tuples.begin();i!=tuples.end();++i){
		if((*i)->fields.size() == fields.size()){
			bool exists = true;
			for(size_t j=0;j < fields.size();++j) if( ( (*i)->fields[j].type != fields[j].second ) || ( (*i)->fields[j].id != fields[j].first ) ) exists=false;
			if(exists) return *i;
		}
	}

	char buffer1[1024] = "tuple";
	char buffer2[1024];
	for(size_t i=0;i<fields.size();i++){
		sprintf(buffer2,"%c%s",i == 0 ? '(' : ',',fields[i].second->id.ptr());
		strcat(buffer1,buffer2);
	}
	strcat(buffer1,")");
	debug("Tuple created: %s",buffer1);

	Type* tuple=new Type(buffer1,Location());
	tuple->isTuple = true;
	auto var=Variable(SymbolID(),Location());
	for(size_t i=0;i<fields.size();i++){
		var.id = fields[i].first;
		var.type = fields[i].second;
		tuple->add(var);
	}
	tuples.push_back(tuple);
	return tuple;
}

//function

FunctionDef::Argument::Argument(const Variable& var) : variable(var) {
}

FunctionDef::FunctionDef(SymbolID name,Location& location) : PrefixDefinition(name,location){
	argument = arpha::Nothing;
	returnType   = arpha::Nothing;
	bodyScope = nullptr;
	body = nullptr;
}

//opreator

PrefixOperator::PrefixOperator(SymbolID name,Location& location) : PrefixDefinition(name,location) {
}

InfixOperator::InfixOperator(SymbolID name,int stickiness,Location& location) : InfixDefinition(name,stickiness,location){
}
