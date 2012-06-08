#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "evaluate.h"
#include "interpret.h"
#include "analyze.h"
#include "../intrinsics/types.h"
#include "../intrinsics/ast.h"

PrefixDefinition::PrefixDefinition(SymbolID name,Location& location){
	this->id = name;this->location = location;
	visibilityMode = Visibility::Public;
	flags = 0;
}
PrefixDefinition* PrefixDefinition::copyProperties(PrefixDefinition* dest){
	//Other stuff copied on creation
	dest->visibilityMode = visibilityMode;
	dest->flags = flags;
	return dest;
}
InfixDefinition::InfixDefinition(SymbolID name,int stickiness,Location& location){
	this->id = name;this->stickiness = stickiness;this->location = location;
	visibilityMode = Visibility::Public;
	flags = 0;
}
InfixDefinition* InfixDefinition::copyProperties(InfixDefinition* dest){
	//Other stuff copied on creation
	dest->visibilityMode = visibilityMode;
	dest->flags = flags;
	return dest;
}
bool PrefixDefinition::isFlagSet(uint16 id){
	return (flags & id) != 0;
}
bool PrefixDefinition::setFlag(uint16 id){
	auto v = (flags & id) != 0;
	flags|=id;
	return v;
}
bool InfixDefinition::isFlagSet(uint16 id){
	return (flags & id) != 0;
}
bool InfixDefinition::setFlag(uint16 id){
	auto v = (flags & id) != 0;
	flags|=id;
	return v;
}


//variable
Variable::Variable(SymbolID name,Location& location,Scope* owner) : PrefixDefinition(name,location),value(nullptr),isMutable(true),expandMe(false) {
	_owner = owner;
}
Function* Variable::functionOwner() const{
	return _owner->functionOwner();
}
bool Variable::isLocal() const {
	return _owner->functionOwner() != nullptr;
}
bool Variable::isResolved(){
	return isFlagSet(IS_RESOLVED);
}
bool Variable::resolve(Evaluator* evaluator){
	auto _resolved = true;
	if(type.isResolved()) _resolved = true;
	else if(type.isPattern()) _resolved = false;
	else _resolved = type.resolve(evaluator);

	if(!_resolved){
		evaluator->markUnresolved(this);
	}
	else {
		setFlag(IS_RESOLVED);
		if(asArgument()){
			if(!type.type()->isValidTypeForArgument()) error(location,"An argument %s can't have a type %s",id,type.type());
		}
		else if(!type.type()->isValidTypeForVariable()) error(location,"A variable %s can't have a type %s",id,type.type());
	}
	return _resolved;
}
void Variable::specifyType(TypeExpression* givenType){
	type.specify(givenType);
	setFlag(IS_RESOLVED);
}
//Matches a type to a patterned type and resolves the patterned type.
bool Variable::deduceType(TypeExpression* givenType){
	if(type.deduce(givenType,_owner)){ 
		setFlag(IS_RESOLVED);//TODO check if valid type..
		return true;
	}
	return false;
}
void Variable::setImmutableValue(Node* value){
	assert(isMutable == false);
	assert(value->isResolved());
	this->value = value;
	
	if(value->isConst()){
		if(value->asIntegerLiteral()){
			assert(type.type()->type == TypeExpression::INTEGER);
			this->value->asIntegerLiteral()->_type = type.type()->integer; //def a int8 = 1 -> make the 1 explicitly int8
		}
		expandMe = true;
	}
	debug("Setting value %s to variable %s - %s",value,id,expandMe);
}

PrefixDefinition* Variable::duplicate(DuplicationModifiers* mods){
	auto duplicatedReplacement = new Variable(id,location,mods->target);
	duplicatedReplacement->type = type.duplicate(mods);
	duplicatedReplacement->isMutable = isMutable;
	duplicatedReplacement->registerID = registerID;
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(duplicatedReplacement),false);
	return copyProperties(duplicatedReplacement);
}
Node* Variable::createReference(){
	return new VariableReference(this);
}

Argument* Variable::asArgument(){ return nullptr; }

Argument* Argument::asArgument(){ return this; }

Argument::Argument(SymbolID name,Location& location,Scope* owner) : Variable(name,location,owner),	_defaultValue(nullptr),_dependent(false),_constraint(nullptr) {
}
bool Argument::expandAtCompileTime() {
	assert(isResolved());
	return type.type()->isSame(intrinsics::types::Type);
}
PrefixDefinition* Argument::duplicate(DuplicationModifiers* mods){
	return nullptr; //Arguments are duplicates inside function duplicate
}
Argument* Argument::reallyDuplicate(DuplicationModifiers* mods,TypeExpression* newType){
	Argument* dup = new Argument(id,location,mods->target);
	if(newType && type.isPattern()){
		dup->type._type = newType;//TODO dup?
		dup->type._type->_localSemantics = false;//Use non-local type
		dup->type.kind = TypePatternUnresolvedExpression::Type;
	}
	else dup->type = type.duplicate(mods);
	dup->isMutable = isMutable;
	dup->registerID = registerID;
	dup->_defaultValue = _defaultValue ? _defaultValue->duplicate() : nullptr;
	mods->redirectors[reinterpret_cast<void*>(static_cast<Variable*>(this))] = std::make_pair(reinterpret_cast<void*>(static_cast<Variable*>(dup)),false);
	copyProperties(dup);
	return dup;
}
void Argument::defaultValue(Node* expression,bool inferType,bool typecheck){
	assert(expression->isResolved());
	if(inferType){
		type.kind = TypePatternUnresolvedExpression::Type;
		type._type = expression->_returnType();
	}
	else if(typecheck) {
		expression = ::typecheck(expression->location,expression,type.type());
	}
	_defaultValue = expression;
}
Node* Argument::defaultValue() const {
	return _defaultValue;
}
bool Argument::isDependent() const {
	return _dependent;
}

//intrinsic type
IntrinsicType::IntrinsicType(SymbolID name,Location& location,IntrinsicType* base) : TypeBase(name,location),_reference(this),_base(base),construct(nullptr) {
	setFlag(IS_RESOLVED);
}
size_t IntrinsicType::size() const { return 0; }

//integer type

IntegerType::IntegerType(SymbolID name,Location& location) : TypeBase(name,location){
	setFlag(IS_RESOLVED);
	//temporary TODO move to arpha package source files
	if(name == "int32"){
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
bool IntegerType::isSubset(IntegerType* other) const {
	return (!(min<other->min)) && max<=other->max;
}

//type

Record::Record(SymbolID name,Location& location) : TypeBase(name,location) {
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
	assert(!isResolved());
	fields.push_back(var);
}

Node* Record::createReference(){
	return new TypeExpression(this);
}
//TODO record duplication

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
		else if(typeExpr->type == TypeExpression::POINTER){
			collectUnresolvedRecord(typeExpr->argument,initialRecord,records,foundNotRecords);
		}
		else foundNotRecords++;
	}
	else if(auto call = unresolvedExpression->asCallExpression()){
		collectUnresolvedRecord(call->arg,initialRecord,records,foundNotRecords);
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
		if((*i).type.isResolved() && (*i).type.type()->matchRecord(this)){
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
Record::Field Record::Field::duplicate(DuplicationModifiers* mods){
	Field result;
	result.name = name;
	result.type = type.duplicate(mods);
	result.isExtending = isExtending;
	return result;
}
PrefixDefinition* Record::duplicate(DuplicationModifiers* mods){
	if(headRecord){ //anonymous record
		return this;
	}

	auto rec = new Record(id,location);
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(rec),false);
	for(auto i = fields.begin();i!=fields.end();i++){
		rec->fields.push_back((*i).duplicate(mods));
	}
	rec->_resolved = _resolved;
	rec->_size = _size;
	
	return copyProperties(rec);
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
	visibilityMode = firstFunction->visibilityMode;
	functions.push_back(firstFunction);
	if(firstFunction->isFlagSet(Function::TYPE_GENERATOR_FUNCTION)) setFlag(TYPE_GENERATOR_SET);
}
Overloadset* Overloadset::asOverloadset(){
	 return this;
}
void Overloadset::push_back(Function* function){
	if(isFlagSet(TYPE_GENERATOR_SET) && !function->isFlagSet(Function::TYPE_GENERATOR_FUNCTION)){
		error(function->location,"Can't add the function %s to type overload set",function->id);
		return;
	}
	if(visibilityMode == Visibility::Private && function->visibilityMode == Visibility::Public) visibilityMode = Visibility::Public;
	functions.push_back(function);//TODO check against same functions
}
bool Overloadset::isResolved(){ 
	return isFlagSet(IS_RESOLVED);
}
bool Overloadset::resolve(Evaluator* evaluator){
	auto _resolved = true;
	for(auto i = functions.begin();i!= functions.end();i++){
		if(!(*i)->isResolved() || evaluator->forcedToEvaluate){
			if(!(*i)->resolve(evaluator)) _resolved = false;
		}
	}
	if(_resolved) setFlag(IS_RESOLVED);
	return _resolved;
}
PrefixDefinition* Overloadset::duplicate(DuplicationModifiers* mods){
	auto os = new Overloadset(functions[0]->duplicate(mods));
	auto i = functions.begin();
	for(i++;i!=functions.end();i++){
		os->push_back((*i)->duplicate(mods));
	}
	return copyProperties(os);
}
PrefixDefinition* Overloadset::mergedDuplicate(DuplicationModifiers* mods,Overloadset* dest){
	assert(!dest->isFlagSet(TYPE_GENERATOR_SET));//TODO lates
	for(auto i = functions.begin();i!=functions.end();i++){
		dest->push_back((*i)->duplicate(mods));
	}
	if(!isResolved()) dest->flags &= (~IS_RESOLVED);
	return dest;
}

//Function

static bool isUnresolvedArgumentDependent(Variable* argument,const Function* func,Node* unresolvedExpression){
	if(auto varRef = unresolvedExpression->asVariableReference()){
		auto arg = func->findArgument(varRef -> variable);
		if(arg != -1 && (func->arguments[arg]->type.isPattern() || 
			(func->arguments[arg]->type.isResolved() && func->arguments[arg]->type.type()->isSame(intrinsics::types::Type)) )
		){
				if(argument == func->arguments[arg]){
					//NB not really needed
					error(argument->location,"A parameter %s can't use itself to describe it's type!",argument->id);
				}
				return true;
		}
		return false;
	}
	else if(auto tuple = unresolvedExpression->asTupleExpression()){
		for(auto i = tuple->children.begin();i!=tuple->children.end();i++){
			if(isUnresolvedArgumentDependent(argument,func,*i)) return true;
		}
	}
	else if(auto call = unresolvedExpression->asCallExpression()){
		if(isUnresolvedArgumentDependent(argument,func,call->arg)) return true;
		if(isUnresolvedArgumentDependent(argument,func,call->object)) return true;
	}
	return false;
}

//TODO arguments depending on other arguments i.e. def f(t,v typeof(t)) = ..
//1. Find if unresolved arguments have a reference to arg
//2. When resolving the overload set do...
Function::Function(SymbolID name,Location& location,Scope* bodyScope) : PrefixDefinition(name,location), body(bodyScope), allArgMatcher(bodyScope) {
	_resolved = false;
	constInterpreter = nullptr;

	_argsResolved = false;
	_hasGenericArguments = false;
	_hasExpandableArguments = false;
}

Scope* Function::owner() const {
	return body.scope->parent;
}

bool Function::isResolved(){
	return  (_hasGenericArguments || _hasExpandableArguments) ? _argsResolved : _resolved;
}
bool Function::isPartiallyResolved(){
	return true;//TODO
}
int Function::findArgument(Variable* var) const{
	for(size_t i =0;i <arguments.size();i++){
		if(static_cast<Variable*>(arguments[i]) == var) return (int)i;
	}
	return -1;
}
Node* Function::createReference(){
	return new FunctionReference(this);
}

bool Function::resolve(Evaluator* evaluator){
	assert(!_resolved);
	_resolved = true;
	_argsResolved = true;
	_hasGenericArguments = false;
	_hasExpandableArguments = false;
	for(auto i = arguments.begin();i!=arguments.end();++i){
		if((*i)->type.isPattern()){
			if(isFlagSet(MACRO_FUNCTION)){
				(*i)->type.kind = TypePatternUnresolvedExpression::Type;
				(*i)->type._type = intrinsics::ast::ExprPtr;
				(*i)->resolve(evaluator);
			}
			else _hasGenericArguments = true;
		}
		else if(!(*i)->isResolved() || evaluator->forcedToEvaluate){
			if( !(*i)->type.resolve(evaluator,&allArgMatcher) ){
				
				//depenent argument
				//NB pretend that the arg is resolved with
				//TODO unresolved dependent argument!
				if((*i)->type.kind == TypePatternUnresolvedExpression::Unresolved && isUnresolvedArgumentDependent(*i,this,(*i)->type.unresolvedExpression)){
					debug("Argument %s is dependent!",(*i)->id);
					(*i)->_dependent = true;
				}
				else _resolved = false;

			} else (*i)->resolve(evaluator);
		}
		if((*i)->isResolved() && (*i)->expandAtCompileTime()) _hasExpandableArguments = true;
	}
	_argsResolved = _resolved;

	//resolve body
	if(!body.isResolved() || evaluator->forcedToEvaluate ){
		evaluator->eval(&body);
		if(!body.isResolved()) _resolved = false;
	}

	//resolve return type
	//Body has no return expression => return void
	if(!isFlagSet(CONTAINS_RETURN)){
		if(_returnType.isPattern() && _returnType.pattern == nullptr) _returnType.specify(intrinsics::types::Void);
		else if(body.isResolved() && body.children.size() != 0 && ( 
			(_returnType.isResolved() && !_returnType.type()->isSame(intrinsics::types::Void)) || _returnType.isPattern() ) ){ //TODO
			error(location,"The function %s is expected to return a value of type %s, but it contains no return statement!",id,_returnType.isPattern()?_returnType.pattern:_returnType.type());
		}
	}
	if(!_returnType.isResolved()){
		if(_returnType.isPattern() || !_returnType.resolve(evaluator)) _resolved = false;
	}

	if(_resolved){
		debug("Function %s is fully resolved!\n E : %s G : %s Ret : %s Body: %s",id,_hasExpandableArguments,_hasGenericArguments,_returnType.type(),&body);
		analyze(&body,this);
	}
	else {
		debug("Function %s isn't resolved!\n Body: %s,%s,%s,%s",id,&body,_argsResolved,body.isResolved(),_returnType.kind);
		/*for(auto i=arguments.begin();i!=arguments.end();++i){
			debug("%s:%s",(*i)->id,(*i)->type.kind);
		}*/
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
	//assert(_resolved); TODO retun resolved
	return _returnType.type();
}
bool Function::canExpandAtCompileTime(){
	assert(_argsResolved);
	return _hasExpandableArguments;
}
//TODO redefine arguments
//Return true if a new function was generated or false if an already generated function was used.
bool Function::expandedDuplicate(DuplicationModifiers* mods,std::vector<Node*>& parameters,Function** dest){
	
	//get the new name and see if such function was already generated!
	std::ostringstream name;
	name<<id.ptr()<<"_";
	auto oldD = compiler::currentUnit()->printingDecorationLevel;
	compiler::currentUnit()->printingDecorationLevel = 0;
	for(size_t i = 0;i!=arguments.size();++i){
		if(!arguments[i]->isDependent() && arguments[i]->expandAtCompileTime()){
			if(auto t = parameters[i]->asTypeExpression()) t->_localSemantics = false;
			name<<"_"<<arguments[i]->id<<"_"<<parameters[i];
		}
	}
	compiler::currentUnit()->printingDecorationLevel = oldD;
	auto sym = (SymbolID(name.str().c_str()));
	if(auto def = mods->target->containsPrefix(sym)){
		*dest = def->asOverloadset()->functions[0];//TODO safety checks??
		debug("Reusing duplicated expanded function %s => %s!",id,sym);
		return false;
	}

	//duplicate the function
	debug("Need to duplicate expand function %s => %s!",id,sym);
	auto func = new Function(sym,location,new Scope(mods->target));
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(func),false);//NB before body duplication to account for recursion

	func->body.scope->_functionOwner = func;
	auto oldTarget = mods->target;
	mods->target = func->body.scope;

	//args
	size_t erased = 0;
	for(size_t i = 0;i!=arguments.size();++i){
		if(!arguments[i]->isDependent() && arguments[i]->expandAtCompileTime()){
			debug("Exp");
			//Pass non-local instead of local types
			if(auto t = parameters[i - erased]->asTypeExpression()) t->_localSemantics = false;
			mods->redirectors[reinterpret_cast<void*>(static_cast<Variable*>(arguments[i]))] = 
				std::make_pair(reinterpret_cast<void*>(parameters[i - erased]),true);
			parameters.erase(parameters.begin() + (i - erased));
			erased++;
			
		}
		else func->arguments.push_back(arguments[i]->reallyDuplicate(mods,nullptr));
	}

	duplicateReturnBody(mods,func);
	mods->target = oldTarget;
	func->_resolved = false;
	*dest = func;
	return true;
}
Function* Function::specializedDuplicate(DuplicationModifiers* mods,std::vector<TypeExpression* >& specializedArgTypes){
	debug("Need to duplicate determined function %s!",id);
	auto func = new Function(id,location,new Scope(mods->target));
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(func),false);//NB before body duplication to account for recursion
	
	func->body.scope->_functionOwner = func;
	auto oldTarget = mods->target;
	mods->target = func->body.scope;
	//args
	for(size_t i = 0;i!=arguments.size();++i){
		func->arguments.push_back(arguments[i]->reallyDuplicate(mods,specializedArgTypes[i]));
	}

	duplicateReturnBody(mods,func);
	mods->target = oldTarget;
	func->_resolved = false;
	return func;
}
Function* Function::duplicate(DuplicationModifiers* mods){
	debug("Duplicating function %s",id);
	auto func = new Function(id,location,new Scope(mods->target));
	mods->redirectors[reinterpret_cast<void*>(this)] = std::make_pair(reinterpret_cast<void*>(func),false);//NB before body duplication to account for recursion
	
	func->body.scope->_functionOwner = func;
	auto oldTarget = mods->target;
	mods->target = func->body.scope;
	//args
	for(auto i = arguments.begin();i!=arguments.end();++i){
		func->arguments.push_back((*i)->reallyDuplicate(mods,nullptr));
		debug("Duplicating arg %s,%s,%s",(*i)->id,(*i)->type.kind,func->arguments.back()->type.kind);
	}
	duplicateReturnBody(mods,func);
	mods->target = oldTarget;
	return func;
}
Function* Function::duplicateReturnBody(DuplicationModifiers* mods,Function* func){
	func->_returnType = _returnType.duplicate(mods);
	
	auto oldRed = mods->returnValueRedirector;
	mods->returnValueRedirector = nullptr;
	body.scope->duplicate(mods);
	body._duplicate(&func->body,mods);
	mods->returnValueRedirector = oldRed;

	func->_resolved = _resolved;
	func->_argsResolved = _argsResolved;
	func->_hasGenericArguments = _hasGenericArguments;
	func->_hasExpandableArguments = _hasExpandableArguments;
	func->ctfeRegisterCount = ctfeRegisterCount;
	func->inliningWeight = inliningWeight;
	copyProperties(func);
	return func;
}


//Imported scope
ImportedScope::ImportedScope(SymbolID name,Location& location) : PrefixDefinition(name,location),scope(nullptr),_reference(this) {
	visibilityMode = Visibility::Private;
}
