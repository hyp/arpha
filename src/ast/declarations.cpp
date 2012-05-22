#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "evaluate.h"
#include "../intrinsics/types.h"

//variable
Variable::Variable(SymbolID name,Location& location,bool isLocal) : PrefixDefinition(name,location),value(nullptr),isMutable(true),expandMe(false) {
	_local = isLocal;
}

bool Variable::isLocal(){
	return _local;
}
bool Variable::isResolved(){
	return type.isResolved();
}
bool Variable::resolve(Evaluator* evaluator){
	auto _resolved = true;
	if(type.isInferred()) _resolved = false;
	else _resolved = type.resolve(evaluator);
	if(!_resolved) evaluator->markUnresolved(this);
	return _resolved;
}
void Variable::setImmutableValue(Node* value){
	assert(isMutable == false);
	assert(value->isResolved());
	this->value = value;
	debug("Setting value %s to variable %s",value,id);
	if(value->isConst()){
		if(value->asIntegerLiteral()) this->value->asIntegerLiteral()->_type = type.type(); //def a bool = 1 -> make the 1 explicitly boolean
		expandMe = true;
	}
}

PrefixDefinition* Variable::duplicate(DuplicationModifiers* mods){
	auto duplicatedReplacement = new Variable(id,location,mods->target->functionOwner() ? true : false);
	duplicatedReplacement->type = type.duplicate(mods);
	duplicatedReplacement->isMutable = isMutable;
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(duplicatedReplacement),false);
	return duplicatedReplacement;
}


Argument::Argument(SymbolID name,Location& location) : Variable(name,location,true),	_defaultValue(nullptr) {
}
PrefixDefinition* Argument::duplicate(DuplicationModifiers* mods){
	return nullptr; //Arguments are duplicates inside function resolve
}
Argument* Argument::reallyDuplicate(DuplicationModifiers* mods,TypeExpression* newType){
	Argument* dup = new Argument(id,location);
	if(newType && type.isWildcard()){
		dup->type._type = newType;//TODO dup?
		dup->type.kind = InferredUnresolvedTypeExpression::Type;
	}
	else dup->type = type.duplicate(mods);
	dup->isMutable = isMutable;
	dup->_defaultValue = _defaultValue ? _defaultValue->duplicate() : nullptr;
	mods->redirectors[reinterpret_cast<void*>(static_cast<Variable*>(this))] = std::make_pair(reinterpret_cast<void*>(static_cast<Variable*>(dup)),false);
	return dup;
}
bool Argument::resolve(Evaluator* evaluator){
	return false;//Arguments are resolved inside function resolve
}
void Argument::defaultValue(Node* expression,bool inferType){
	assert(expression->isResolved());
	if(inferType){
		type.kind = InferredUnresolvedTypeExpression::Type;
		type._type = expression->_returnType();
	}
	else {
		expression = typecheck(expression->location,expression,type.type());
	}
	_defaultValue = expression;
}
Node* Argument::defaultValue() const {
	return _defaultValue;
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

Node* Record::createReference(){
	return reference();
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
		if((*i).type.isResolved() && (*i).isExtending){
			if(!insertUniqueExtender(collection,(*i).type.type())) return false;
		}
		if((*i).type.isResolved() && (*i).type.type()->type == TypeExpression::RECORD)
			if(!traverseExtenderHierarchy((*i).type.type()->record,collection)) return false;
	}
	return true;
}

// This function recursively checks if a Record r or the record from one of r's fields contains a field with an exact type of illegalRecord
static Record* containsItself(Record* record,Record* illegalRecord){
	assert(record->isResolved());
	for(auto i = record->fields.begin();i!=record->fields.end();++i){
		if((*i).type.isResolved()){
			auto typeExpr = (*i).type.type();
			if(typeExpr->type == TypeExpression::RECORD){
				if(typeExpr->record == illegalRecord) return record;
				else if(auto recursiveCheck = containsItself(typeExpr->record,illegalRecord)) return recursiveCheck;
			}
		}
	}
	return nullptr;
}

//TODO what about multiple unresolved records in one expression?? e.g. MyType(Foo::unresolved,Bar::unresolved)
// TODO insertUnique, increment unresolvedCount for ^
static void collectUnresolvedRecord(Node* unresolvedExpression,Record* initialRecord,std::vector<Record*>& records,size_t& foundNotRecords ){
	if(auto typeExpr = unresolvedExpression->asTypeExpression()){
		if(typeExpr->type == TypeExpression::RECORD){
			if(typeExpr->record!=initialRecord) //NB dont mark itself //unresolved record is implied here
				records.push_back(typeExpr->record);
		}
		else foundNotRecords++;
	}
	//TODO hackz
	else if(auto ptr = unresolvedExpression->asPointerOperation()){
		if(ptr->kind == PointerOperation::DEREFERENCE) collectUnresolvedRecord(ptr->expression,initialRecord,records,foundNotRecords);
	}
	else if(auto tuple = unresolvedExpression->asTupleExpression()){
		for(auto i = tuple->children.begin();i!=tuple->children.end();i++){
			if(!(*i)->isResolved()) collectUnresolvedRecord((*i),initialRecord,records,foundNotRecords);
		}
	}
	else if(!unresolvedExpression->isResolved()) foundNotRecords++;
}

// This functions tries to resolve type definitions containing unresolved records which contain the records of the original type
bool Record::resolveCircularReferences(Evaluator* evaluator){
	debug("Trying to resolve possible circular references for type %s",this);
	std::vector<Record*> records;
	size_t unresolvedCount = 0;
	size_t notRecords = 0;
	//Check if otherRecord contains a field which is a this record or pointer to this record
	for(auto i = fields.begin();i!=fields.end();++i){
		if(!(*i).type.isResolved()){
			unresolvedCount++;
			auto expr = (*i).type.unresolvedExpression;
			collectUnresolvedRecord((*i).type.unresolvedExpression,this,records,notRecords);
		}
	}

	debug(" 1) Collected %s,%s records. %s",records.size(),unresolvedCount, notRecords );
	if((!records.size()) || records.size() != unresolvedCount){
		//special case for 
		// type Foo12 { var x *Bar12,*Bar12 }
		// type Bar12 { var x *Foo12,*Foo12 }
		if(!(records.size() > unresolvedCount && notRecords == 0))
			return false;
	}
	

	//Step 2 - pretend that I am resolved and try to resolve all collected records
	_resolved = true;
	for(auto i = records.begin();i!=records.end();i++){
		if(!(*i)->resolve(evaluator)){
			_resolved = false;
			debug(" .) Couldn't resolve circular references for single element %s",(*i));
			break;
		}
	}

	if(!_resolved){
		//Reset the records
		for(auto i = records.begin();i!=records.end();i++){
			(*i)->resolve(evaluator);
		}
		return false;
	}else{	
		//resolve myself
		for(auto i = fields.begin();i!=fields.end();++i){
			if(!(*i).type.isResolved()){
				if(!(*i).type.resolve(evaluator)) _resolved = false;
			}
		}
	}

	//Here all records were sucessfully resolved, so we can safely assume that we've been resolved.
	if(_resolved) debug(" .) suceeded!");
	return _resolved;
}

bool Record::resolve(Evaluator* evaluator){
	/*assert(!_resolved);*/
	_resolved = true;
	for(auto i = fields.begin();i!=fields.end();++i){
		if(!(*i).type.isResolved()){
			if(!(*i).type.resolve(evaluator)) _resolved = false;
		}
		//Don't you dare use itself in itself!
		//TODO rm? This isn't necessary as we are checking hasItself below
		//TODO don't allow extended Pointer to self
		if((*i).type.isResolved() && (*i).type.type() == reference()){
			error(location,"Recursive type declaration - The type %s has a field %s of its own type!",id,(*i).name);
			_resolved = false;
			break;
		}
	}
	if(!_resolved) resolveCircularReferences(evaluator);
	if(_resolved){
		if(auto hasItself = containsItself(this,this)){
			error(location,"Recursive type declaration - The type %s contains a field of type %s which has a field of type %s!",this,hasItself,this);
			_resolved = false;
		}
		if(_resolved){
			std::vector<TypeExpression* > collection;
			if(!traverseExtenderHierarchy(this,collection)){
					error(location,"Faulty type extension hierarchy - The type %s features multiple path to type %s",id,collection.back());
					_resolved = false;
			}
		}
	}
	if(_resolved){
		calculateResolvedProperties();
		debug("Successfully resolved type %s - sizeof %s",id,_size);
	}else evaluator->markUnresolved(this);
	return _resolved;
}
void Record::calculateResolvedProperties(){
	//sizeof
	//TODO perhaps make it lazy calculation??
	_size = 0;
	for(auto i = fields.begin();i!=fields.end();++i){
		assert((*i).type.isResolved());
		_size += (*i).type.type()->size();
	}
	
}

bool Record::isResolved(){
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
		assert(record[j].type.isResolved());
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
	visibilityMode = Visibility::Public;//!important // TODO fix import module a type foo, module b private def foo() //<-- conflict
	functions.push_back(firstFunction);
	_resolved = false;
}
Overloadset* Overloadset::asOverloadset(){
	 return this;
}
void Overloadset::push_back(Function* function){
	functions.push_back(function);//TODO check against same functions
}
bool Overloadset::isResolved(){ 
	return _resolved;
}
bool Overloadset::resolve(Evaluator* evaluator){
	_resolved = true;
	for(auto i = functions.begin();i!= functions.end();i++){
		if(!(*i)->isResolved()){
			if(!(*i)->resolve(evaluator)) _resolved = false;
		}
	}
	return _resolved;
}
PrefixDefinition* Overloadset::duplicate(DuplicationModifiers* mods){
	auto os = new Overloadset(functions[0]->duplicate(mods));
	auto i = functions.begin();
	for(i++;i!=functions.end();i++){
		os->push_back((*i)->duplicate(mods));
	}
	return os;
}

//Function

Function::Function(SymbolID name,Location& location,Scope* bodyScope) : PrefixDefinition(name,location), body(bodyScope) {
	intrinsicEvaluator = nullptr;
	_resolved = false;
	_hasReturnInside = false;
}

bool Function::isResolved(){
	return  _hasGenericArguments ? _argsResolved : _resolved;
}
bool Function::isPartiallyResolved(){
	return true;//TODO
}
bool Function::resolve(Evaluator* evaluator){
	assert(!_resolved);
	_resolved = true;
	_argsResolved = true;
	_hasGenericArguments = false;
	for(auto i = arguments.begin();i!=arguments.end();++i){
		if((*i)->type.isWildcard()) _hasGenericArguments = true; //TODO constraints
		else if(!(*i)->isResolved()){
			if(!(*i)->resolve(evaluator)) _resolved = false;
		}
	}
	_argsResolved = _resolved;

	//resolve body
	if(!body.isResolved()){
		evaluator->eval(&body);
		if(!body.isResolved()) _resolved = false;
	}

	//resolve return type
	//Body has no return expression => return void
	if(!_hasReturnInside && _returnType.isInferred()) _returnType.infer(intrinsics::types::Void);
	if(!_returnType.isResolved()){
		if(_returnType.isInferred() || !_returnType.resolve(evaluator)) _resolved = false;
	}

	if(_resolved){
		debug("Function %s is fully resolved!\n G : %s Ret : %s Body: %s",id,_hasGenericArguments,_returnType.type(),&body);
	}
	else {
		debug("Function %s isn't resolved!\n Body: %s,%s,%s,%s",id,&body,_argsResolved,body.isResolved(),_returnType.kind);
		evaluator->markUnresolved(this);
	}
	return _resolved;
}

TypeExpression* Function::argumentType()  {
	assert(_resolved);
	assert(!_hasGenericArguments);
	auto args = arguments.size();
	if(args == 0) return intrinsics::types::Void;
	else if(args == 1) return arguments[0]->type.type();
	std::vector<Record::Field> fields;
	for(auto i = arguments.begin();i!=arguments.end();++i){
		fields.push_back(Record::Field(SymbolID(),(*i)->type.type()));
	}
	return new TypeExpression(Record::findAnonymousRecord(fields));
}
TypeExpression* Function::returnType() {
	assert(_resolved);
	return _returnType.type();
}
Function* Function::duplicate(DuplicationModifiers* mods){
	auto func = new Function(id,location,new Scope(mods->target));
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(func),false);//NB before body duplication to account for recursion
	//args
	auto argFix = mods->functionArgumentTypeFix;
	if(argFix) {
		for(size_t i = 0;i!=arguments.size();++i){
			func->arguments.push_back(arguments[i]->reallyDuplicate(mods,(*argFix)[i]));
		}
	}
	else {
		for(auto i = arguments.begin();i!=arguments.end();++i){
			func->arguments.push_back((*i)->reallyDuplicate(mods,nullptr));
		}
	}
	//return type
	func->_returnType = _returnType.duplicate(mods);
	//body
	func->body.scope->_functionOwner = func;
	auto oldTarget = mods->target;
	mods->target = func->body.scope;
	auto oldRed = mods->returnValueRedirector;
	mods->returnValueRedirector = nullptr;
	body.scope->duplicate(mods);
	body._duplicate(&func->body,mods);
	mods->target = oldTarget;
	mods->returnValueRedirector = oldRed;
	//TODO resolve
	return func;
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
