#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "syntax/parser.h"
#include "ast/node.h"
#include "compiler.h"
#include "arpha.h"

//variable
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	type = compiler::inferred;
}
void Variable::inferType(Type* t){
	debug("Inferred type %s for variable %s",t->id,id);
	type = t;
}

//type

Type::Type(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	size = 0;
	headRecord = nullptr;
}

Variable* Type::lookupField(const SymbolID fieldName){
	for(auto i = fields.begin();i!=fields.end();++i){
		if( (*i).id == fieldName ) return i._Ptr;
	}
	return nullptr;
}

void Type::add(const Variable& var){
	fields.push_back(var);
	size += var.type->size;
}

/**
*Organized as
*  record(int32,int32) <-- headRecord
*     record(x: int32,y:int32) <-- subRecord
*     record(width: int32, height: int32)
*  record(int32,bool)
*/
std::vector<std::pair<Type*,std::vector<Type*>>> records;

Type* Type::createRecordType(std::vector<std::pair<SymbolID,Type*>>& record,Type* headRecord){
	std::string typeName = "record";
	for(size_t i=0;i<record.size();i++)
		typeName+=format("%c%s:%s",i == 0 ? '(' : ',',headRecord ? record[i].first.ptr() : "_",record[i].second->id.ptr());
	typeName+=')';
	debug("Tuple created: %s",typeName);

	Type* type=new Type(typeName.c_str(),Location());
	type->headRecord = headRecord ? headRecord : type;
	auto var=Variable(SymbolID(),Location()); //NOTE: Non-new definition construction
	for(size_t i=0;i<record.size();i++){
		if(headRecord) var.id = record[i].first;
		var.type = record[i].second;
		type->add(var);
	}
	return type;
}

Type* Type::findSubRecord(Type* headRecord,std::vector<Type*>& subRecords,std::vector<std::pair<SymbolID,Type*>>& record){
	//Match the names to the corresponding record
	for(auto i=subRecords.begin();i!=subRecords.end();i++){
		auto subRecord = *i;
		auto areFieldsSameName = true;
		for(size_t j=0;j < record.size();j++){
			if(subRecord->fields[j].id != record[j].first) areFieldsSameName = false;
		}
		if(areFieldsSameName) return subRecord;
	}
	//If the record doesn't exist we have to add it to subrecords
	auto t = createRecordType(record,headRecord);
	subRecords.push_back(t);
	return t;
}

Type* Type::findRecord(std::vector<std::pair<SymbolID,Type*>>& record){
	assert(record.size() > 1);

	//Check to see if the record has named fields
	auto areFieldsUnnamed = true;
	for(size_t j=0;j < record.size();j++){
		if(!record[j].first.isNull()) areFieldsUnnamed = false;
	}

	//Check to see if the following record already exists.
	//Match the types to the corresponding tuple
	for(auto i=records.begin();i!=records.end();i++){
		auto headRecord = (*i).first;
		if(headRecord->fields.size() == record.size()){
			auto areFieldsSameType = true;			
			for(size_t j=0;j < record.size();j++){
				if(!(headRecord->fields[j].type == record[j].second)) areFieldsSameType = false;
			}
			//Match the names to the corresponding record
			if(areFieldsSameType) return areFieldsUnnamed ? headRecord : findSubRecord(headRecord,(*i).second,record);
		}
	}

	//If the record doesn't exist we have to add it to head and sub records
	auto t = createRecordType(record);
	records.push_back(std::make_pair(t,std::vector<Type*>()));
	if(areFieldsUnnamed) return t;
	t = createRecordType(record,t);
	records.back().second.push_back(t);
	return t;
}

Type* Type::tuple(std::vector<std::pair<SymbolID,Type*>>& fields){
	return findRecord(fields);
}

//function

Function::Argument::Argument(const Variable& var) : variable(var) {
}

Function::Function(SymbolID name,Location& location) : PrefixDefinition(name,location){
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
