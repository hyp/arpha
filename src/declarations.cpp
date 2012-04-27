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
	headRecord = nullptr;
}

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
	/*assert(fields.size() > 1);
	//check if such tuple already exists
	for(std::vector<Type*>::iterator i=tuples.begin();i!=tuples.end();++i){
		
		if((*i)->fields.size() == fields.size()){
			bool exists = true;
			for(size_t j=0;j < fields.size();++j){
				if( ( (*i)->fields[j].type != fields[j].second ) || ( (*i)->fields[j].id != fields[j].first ) ) exists=false;
			}
			if(exists) return *i;
		}
	}

	char buffer1[1024] = "tuple";
	char buffer2[1024];
	for(size_t i=0;i<fields.size();i++){
		sprintf(buffer2,"%c%s:%s",i == 0 ? '(' : ',',fields[i].first.isNull() ? "_" : fields[i].first.ptr(),fields[i].second->id.ptr());
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
	return tuple;*/
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
