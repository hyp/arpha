#include "../common.h"
#include "../scope.h"
#include "declarations.h"
#include "../syntax/parser.h"
#include "node.h"
#include "../compiler.h"
#include "../arpha.h"

//variable
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	type  = nullptr;
}


//integer type

IntegerType::IntegerType(SymbolID name,Location& location) : TypeBase(name,location){
	//temporary TODO move to arpha package source files
	_unsigned = false;
	if(name == "bool"){
		_unsigned = true;
		min = 0;
		max = 1;
		_size = 1;
	}
	else if(name == "int32"){
		min = std::numeric_limits<int>::min();
		max = std::numeric_limits<int>::max();
		_size = 4;
	}
	else if(name == "int64"){
		min = std::numeric_limits<int64>::min();
		max = std::numeric_limits<int64>::max();
		_size = 8;
	}
	else if(name == "int8"){
		min = std::numeric_limits<signed char>::min();
		max = std::numeric_limits<signed char>::max();
		_size = 1;
	}
	else if(name == "int16"){
		min = std::numeric_limits<signed short>::min();
		max = std::numeric_limits<signed short>::max();
		_size = 2;
	}
	else{
		min = 0;
		_unsigned = true;
		if(name == "uint32"){
			max = (uint64)std::numeric_limits<uint32>::max();
			_size = 4;
		}
		else if(name == "uint64"){
			max = std::numeric_limits<uint64>::max();
			_size = 8;
		}
		else if(name == "uint8"){
			max = std::numeric_limits<uint8>::max();
			_size = 1;
		}
		else if(name == "uint16"){
			max = std::numeric_limits<unsigned short>::max();
			_size = 2;
		}
	}
}

size_t IntegerType::size() const {
	return _size;
}
bool IntegerType::isValid(BigInt& value) const {
	return min<=value && value<=max;
}
bool IntegerType::isUnsigned() const {
	return _unsigned;
}
Node* IntegerType::assignableFrom(Node* expression,IntegerType* type){
	return expression;//TODO
}

//type

Type::Type(SymbolID name,Location& location) : PrefixDefinition(name,location) {
	_size = 0;
	headRecord = nullptr;
	_resolved = false;
}

Variable* Type::lookupField(const SymbolID fieldName){
	for(auto i = fields.begin();i!=fields.end();++i){
		if( (*i).id == fieldName ) return i._Ptr;
	}
	return nullptr;
}

void Type::add(const Variable& var){
	assert(!_resolved);
	//if(var.type == compiler::Unresolved) _resolved = false;
	fields.push_back(var);
}
void Type::updateOnSolving(){
	_resolved = true;
	debug("Updating type's %s state",id);
	_size = 0;
	for(auto i = fields.begin();i!=fields.end();++i){
		assert((*i).type != compiler::Unresolved);
		_size += (*i).type->_size;
	}
}
bool Type::resolved(){
	return _resolved;
}
size_t Type::size(){
	assert(_resolved);
	return _size;
}
int Type::extendsType(Type* type){
	for(auto i=extenders.begin();i!=extenders.end();i++){
		if(fields[*i].type == type) return *i;
	}
	return -1;
}

std::ostream& operator<< (std::ostream& stream,Type* type){
	if(type->isRecord()){
		stream<<"RecordType";
		for(size_t i = 0;i < type->fields.size();i++){
			stream<<(i == 0 ? '(' : ',')<<(type->fields[i].id.isNull()?"_":type->fields[i].id)<<":"<<type->fields[i].type;
		}
		stream<<')';
	}
	else stream<<"Type "<<type->id<<"(?"<<(type->resolved()?'t':'f')<<")";
	return stream;
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
	for(size_t i=0;i<record.size();i++)//TODO change this ludicrous display
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
	type->updateOnSolving();
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

Function::Argument::Argument(Variable* var) : variable(var) {
}

Function::Function(SymbolID name,Location& location) : PrefixDefinition(name,location){
	argument = compiler::Nothing;
	returnType   = compiler::Nothing;
	bodyScope = nullptr;
	body = nullptr;
	intrinsicEvaluator = nullptr;
}
Type* Function::type(){
	return nullptr;//compiler::function;//TODO
}

void Function::updateOnSolving(){
	//TODO
	//calculate argument type
	if(arguments.size() == 0) argument = compiler::Nothing;
	else if(arguments.size()>1){
		std::vector<std::pair<SymbolID,Type*>> fields;
		for(auto i=arguments.begin();i!=arguments.end();++i) fields.push_back(std::make_pair((*i).variable->id,(*i).variable->type));
		argument = Type::tuple(fields);
	}
	else argument = arguments.front().variable->type;
	debug("Updating function's %s state (arg: %s) ret %s",id,argument,returnType);
}

//operators

PrefixOperator::PrefixOperator(SymbolID name,Location& location) : PrefixDefinition(name,location) {
}

InfixOperator::InfixOperator(SymbolID name,int stickiness,Location& location) : InfixDefinition(name,stickiness,location){
}
