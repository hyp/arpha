#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "../intrinsics/types.h"

//variable
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location),value(nullptr),_reference(this),isMutable(true),expandMe(false) {
}
bool Variable::resolved(){
	return type.resolved();
}
bool Variable::resolve(Evaluator* evaluator){
	if(type.isInferred()) return false;
	else return type.resolve(evaluator);
}
void Variable::setImmutableValue(Node* value){
	assert(isMutable == false);
	assert(value->_returnType() != intrinsics::types::Unresolved);
	this->value = value;
	debug("Setting value %s to variable %s",value,id);
	if(value->isConst()){
		if(value->asIntegerLiteral()) this->value->asIntegerLiteral()->_type = type.type(); //def a bool = 1 -> make the 1 explicitly boolean
		expandMe = true;
	}
}
//intrinsic type
IntrinsicType::IntrinsicType(SymbolID name,Location& location) : TypeBase(name,location),_reference(this) {
}
size_t IntrinsicType::size() const { return 0; }

//integer type

IntegerType::IntegerType(SymbolID name,Location& location) : TypeBase(name,location),_reference(this){
	//temporary TODO move to arpha package source files
	if(name == "bool"){
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
	return !(min<BigInt((uint64)0));
}

// Pointer type
PointerType::PointerType(SymbolID name,Location& location) : TypeBase(name,location) {
}
size_t PointerType::size() const { return 0; } //implemented in typeExpression

//type

Record::Record(SymbolID name,Location& location) : TypeBase(name,location),_reference(this) {
	_size = 0;
	headRecord = nullptr;
	_resolved = false;
}

int Record::lookupField(const SymbolID fieldName){
	for(size_t i = 0;i<fields.size();i++){
		if( fields[i].name == fieldName ) return int(i);
	}
	return -1;
}
void Record::add(const Field& var){
	assert(!_resolved);
	fields.push_back(var);
}

//Collects all the extenders field from the record extender hierarchy
static bool insertUniqueExtender(std::vector<TypeExpression*>& collection,TypeExpression* extender){
	for(auto i = collection.begin();i!=collection.end();i++){
		if((*i)->isSame(extender)){
			collection.push_back(extender);
			return false;
		}
	}
	collection.push_back(extender);
	return true;
}
static bool traverseExtenderHierarchy(Record* record,std::vector<TypeExpression*>& collection){
	for(auto i = record->fields.begin();i!=record->fields.end();i++){
		if((*i).isExtending){
			if(!insertUniqueExtender(collection,(*i).type.type())) return false;
		}
		if((*i).type.type()->type == TypeExpression::RECORD)
			if(!traverseExtenderHierarchy((*i).type.type()->record,collection)) return false;
	}
	return true;
}

bool Record::resolve(Evaluator* evaluator){
	assert(!_resolved);
	_resolved = true;
	for(auto i = fields.begin();i!=fields.end();++i){
		if(!(*i).type.resolved()){
			if(!(*i).type.resolve(evaluator)) _resolved = false;
		}
		//Don't you dare use itself in itself!
		if((*i).type.resolved() && (*i).type.type() == reference()){
			error(location,"Recursive type declaration - The type %s has a field %s of its own type!",id,(*i).name);
			_resolved = false;
			break;
		}
	}
	if(_resolved){
		std::vector<TypeExpression* > collection;
		if(!traverseExtenderHierarchy(this,collection)){
				error(location,"Faulty type extension hierarchy - The type %s features multiple path to type %s",id,collection.back());
				_resolved = false;
		}
	}
	if(_resolved){
		calculateResolvedProperties();
		debug("Successfully resolved type %s - sizeof %s",id,_size);
	}
	return _resolved;
}
void Record::calculateResolvedProperties(){
	//sizeof
	//TODO perhaps make it lazy calculation??
	_size = 0;
	for(auto i = fields.begin();i!=fields.end();++i){
		assert((*i).type.resolved());
		_size += (*i).type.type()->size();
	}
	
}

bool Record::resolved(){
	return _resolved;
}
size_t Record::size() const{
	assert(_resolved);
	return _size;
}

std::ostream& operator<< (std::ostream& stream,Record* type){
	if(type->isAnonymous()){
		stream<<"anon-record"; 
		for(size_t i = 0;i < type->fields.size();i++){
			stream<<(i == 0 ? '(' : ',')<<(type->fields[i].name.isNull()?"_":type->fields[i].name)<<":"<<type->fields[i].type.type();
		}
		stream<<')';
	}
	else stream<<type->id;
	return stream;
}

/**
*Organized as
*  record(int32,int32) <-- headRecord
*     record(x: int32,y:int32) <-- subRecord
*     record(width: int32, height: int32)
*  record(int32,bool)
*/
std::vector<std::pair<Record*,std::vector<Record*> > > records;

Record* Record::createRecordType(std::vector<Field>& record,Record* headRecord){
	std::string typeName = "record";
	for(size_t i=0;i<record.size();i++)//TODO change this ludicrous display
		typeName+=format("%c%s:%s",i == 0 ? '(' : ',',headRecord ? record[i].name.ptr() : "_",record[i].type.type());
	typeName+=')';
	debug("Tuple created: %s",typeName);

	auto type=new Record(typeName.c_str(),Location());
	type->headRecord = headRecord ? headRecord : type;
	for(size_t i=0;i<record.size();i++){
		type->add(Field(headRecord!=nullptr?(record[i].name):(SymbolID()),record[i].type.type() ));
	}
	type->_resolved = true;
	type->calculateResolvedProperties();
	return type;
}

Record* Record::findSubRecord(Record* headRecord,std::vector<Record*>& subRecords,std::vector<Field>& record){
	//Match the names to the corresponding record
	for(auto i=subRecords.begin();i!=subRecords.end();i++){
		auto subRecord = *i;
		auto areFieldsSameName = true;
		for(size_t j=0;j < record.size();j++){
			if(subRecord->fields[j].name != record[j].name) areFieldsSameName = false;
		}
		if(areFieldsSameName) return subRecord;
	}
	//If the record doesn't exist we have to add it to subrecords
	auto t = createRecordType(record,headRecord);
	subRecords.push_back(t);
	return t;
}

Record* Record::findAnonymousRecord(std::vector<Field>& record){
	assert(record.size() > 1);

	//Check to see if the record has named fields
	auto areFieldsUnnamed = true;
	for(size_t j=0;j < record.size();j++){
		if(!record[j].name.isNull()) areFieldsUnnamed = false;
		assert(record[j].type.resolved());
	}

	//Check to see if the following record already exists.
	//Match the types to the corresponding tuple
	for(auto i=records.begin();i!=records.end();i++){
		auto headRecord = (*i).first;
		if(headRecord->fields.size() == record.size()){
			auto areFieldsSameType = true;			
			for(size_t j=0;j < record.size();j++){
				if(!(headRecord->fields[j].type.type()->isSame(record[j].type.type()))) areFieldsSameType = false;
			}
			//Match the names to the corresponding record
			if(areFieldsSameType) return areFieldsUnnamed ? headRecord : findSubRecord(headRecord,(*i).second,record);
		}
	}

	//If the record doesn't exist we have to add it to head and sub records
	auto t = createRecordType(record);
	records.push_back(std::make_pair(t,std::vector<Record*>()));
	if(areFieldsUnnamed) return t;
	t = createRecordType(record,t);
	records.back().second.push_back(t);
	return t;
}

//Overload set

Overloadset::Overloadset(Function* firstFunction) : PrefixDefinition(firstFunction->id,firstFunction->location) {
	declarationType = DeclarationType::OverloadSet;
	visibilityMode = Visibility::Public;//!important // TODO fix import module a type foo, module b private def foo() //<-- conflict
	functions.push_back(firstFunction);
}

//Function

Function::Argument::Argument(Variable* var) : variable(var) {
}

Function::Function(SymbolID name,Location& location) : PrefixDefinition(name,location){
	argument = intrinsics::types::Void;
	returnType   = intrinsics::types::Void;
	bodyScope = nullptr;
	body = nullptr;
	intrinsicEvaluator = nullptr;
}
TypeExpression* Function::type(){
	return nullptr;//compiler::function;//TODO
}

void Function::updateOnSolving(){
	//TODO
	//calculate argument type
	/*if(arguments.size() == 0) argument = compiler::Nothing;
	else if(arguments.size()>1){
		std::vector<std::pair<SymbolID,Type*>> fields;
		for(auto i=arguments.begin();i!=arguments.end();++i) fields.push_back(std::make_pair((*i).variable->id,(*i).variable->type));
		argument = Type::tuple(fields);
	}
	else argument = arguments.front().variable->type;
	debug("Updating function's %s state (arg: %s) ret %s",id,argument,returnType);*/
}

//Imported scope
ImportedScope::ImportedScope(SymbolID name,Location& location) : PrefixDefinition(name,location),scope(nullptr),_reference(this) {
	visibilityMode = Visibility::Private;
}

//operators

PrefixOperator::PrefixOperator(SymbolID name,Location& location) : PrefixDefinition(name,location) {
}

InfixOperator::InfixOperator(SymbolID name,int stickiness,Location& location) : InfixDefinition(name,stickiness,location){
}
