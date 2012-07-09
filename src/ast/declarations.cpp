#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "resolve.h"
#include "interpret.h"
#include "analyze.h"
#include "../intrinsics/types.h"


IntrinsicPrefixMacro::IntrinsicPrefixMacro(SymbolID name) : PrefixDefinition(name,Location(0,0)) {
	setFlag(Node::RESOLVED);
}
IntrinsicInfixMacro::IntrinsicInfixMacro(SymbolID name,int stickiness) : InfixDefinition(name,stickiness,Location(0,0)) {
	setFlag(Node::RESOLVED);
}

//variable
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location),value(nullptr),isMutable(true) {
	_owner = nullptr;
}
Variable::Variable(SymbolID name,Location& location,Scope* owner,Node* value) : PrefixDefinition(name,location),isMutable(false) {
	_owner = owner;
	this->value = value;
	specifyType(value->returnType());
	setImmutableValue(value);
}
bool Variable::applyProperty(SymbolID name,Node* value){
	if(name == "intrinsic"){
		setImmutableValue(getIntrinsicValue(this));
		return true;
	}
	return false;
}
Function* Variable::functionOwner() const{
	return _owner->functionOwner();
}
bool Variable::isLocal() const {
	return _owner->functionOwner() != nullptr;
}
void Variable::specifyType(Type* givenType){
	type.specify(givenType);
	setFlag(RESOLVED);
}
//Matches a type to a patterned type and resolves the patterned type.
bool Variable::deduceType(Type* givenType){
	if(type.deduce(givenType,_owner)){ 
		setFlag(Node::RESOLVED);//TODO check if valid type..
		return true;
	}
	return false;
}
void Variable::setImmutableValue(Node* value){
	assert(isMutable == false);
	assert(value->isResolved() && value->isConst());
	this->value = value;
	if(value->asIntegerLiteral()){
		assert(type.type()->isInteger());
		this->value->asIntegerLiteral()->_type = type.type()->asInteger(); //def a int8 = 1 -> make the 1 explicitly int8
	}
	setFlag(CONSTANT_SUBSTITUTE);

	debug("Setting value %s to variable %s - %s",value,label(),isFlagSet(CONSTANT_SUBSTITUTE));
}

Node* Variable::duplicate(DuplicationModifiers* mods) const {
	auto duplicatedReplacement = new Variable(label(),location());
	duplicatedReplacement->type = type.duplicate(mods);
	duplicatedReplacement->_owner = mods->target;
	duplicatedReplacement->isMutable = isMutable;
	duplicatedReplacement->ctfeRegisterID = ctfeRegisterID;
	mods->duplicateDefinition(const_cast<Variable*>(this),duplicatedReplacement);
	return copyProperties(duplicatedReplacement);
}
Node* Variable::createReference(){
	return new VariableReference(this);
}

Type* Variable::referenceType() const {
	if(isFlagSet(HIDDEN_TYPE)) return const_cast<Variable*>(this)->asArgument()->hiddenType();
	return type.type();
}

Argument* Variable::asArgument(){ return nullptr; }

Argument* Argument::asArgument(){ return this; }

Argument::Argument(SymbolID name,Location& location,Function* owner) : Variable(name,location),	_defaultValue(nullptr),_hiddenType(nullptr) {
	_owner = owner->body.scope;
}
void Argument::hideType(Type* givenType){
	setFlag(HIDDEN_TYPE);
	_hiddenType = givenType;
}
Type* Argument::hiddenType() const {
	return _hiddenType;
}
bool Argument::expandAtCompileTime() const {
	return isFlagSet(IS_EXPENDABLE);
}
Argument* Argument::reallyDuplicate(Function* dest,DuplicationModifiers* mods) const {
	Argument* dup = new Argument(label(),location(),dest);
	dup->type = type.duplicate(mods);
	dup->isMutable = isMutable;
	dup->ctfeRegisterID = ctfeRegisterID;
	dup->_defaultValue = _defaultValue ? _defaultValue->duplicate(mods) : nullptr;
	dup->_hiddenType = _hiddenType;
	mods->duplicateDefinition(const_cast<Argument*>(this),dup);
	copyProperties(dup);
	return dup;
}
void Argument::defaultValue(Node* expression){
	_defaultValue = expression;
}
Node* Argument::defaultValue() const {
	return _defaultValue;
}
bool Argument::isDependent() const {
	return false;
}

//integer type

IntegerType::IntegerType(SymbolID name) : id(name),_type(this){
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

IntegerType* IntegerType::make(BigInt& min,BigInt& max){
	for(auto i = expansions.begin();i!=expansions.end();i++){
		if((*i)->min == min && (*i)->max == max) return *i;
	}
	std::ostringstream name;
	name<<"int("<<min<<","<<max<<")";
	auto t = new IntegerType(name.str().c_str());
	t->min = min;
	t->max = max;
	expansions.push_back(t);
	return t;
}
std::vector<IntegerType* > IntegerType::expansions;

//type
//Overload set

Overloadset::Overloadset(Function* firstFunction) : PrefixDefinition(firstFunction->label(),firstFunction->location()) {
	visibilityMode(firstFunction->visibilityMode());
	functions.push_back(firstFunction);
	if(firstFunction->isFlagSet(Function::TYPE_GENERATOR_FUNCTION)) setFlag(TYPE_GENERATOR_SET);
	setFlag(Node::RESOLVED);
}
Overloadset* Overloadset::asOverloadset(){
	 return this;
}
void Overloadset::push_back(Function* function){
	if(isFlagSet(TYPE_GENERATOR_SET) && !function->isFlagSet(Function::TYPE_GENERATOR_FUNCTION)){
		error(function,"Can't add the function %s to type overload set",function->label());
		return;
	}
	if(visibilityMode() == data::ast::PRIVATE && function->isPublic()) visibilityMode(data::ast::PUBLIC);
	functions.push_back(function);//TODO check against same functions
}

//Function

Function::Function(SymbolID name,Location& location) : PrefixDefinition(name,location), body(), allArgMatcher(body.scope) {
	intrinsicCTFEbinder = nullptr;
	generatedFunctionParent = nullptr;
	body.scope->_functionOwner = this;
}
bool   Function::applyProperty(SymbolID name,Node* value){
	if(name == "intrinsic"){
		setFlag(IS_INTRINSIC);
		return true;
	} else if(name == "external"){
		setFlag(IS_EXTERNAL);
		return true;
	}
	return false;
}
bool   Function::isIntrinsic() const {
	return isFlagSet(IS_INTRINSIC);
}
Function::CallingConvention Function::callingConvention() const {
	return CC_ARPHA;
}
Scope* Function::owner() const {
	return body.scope->parent;
}
void   Function::addArgument(Argument* arg){
	assert(!isResolved());
	arguments.push_back(arg);
	body.scope->define(arg);
}
void   Function::specifyReturnType(Type* givenType){
	assert(!isResolved());
	_returnType.specify(givenType);
}
int    Function::findArgument(Variable* var) const{
	for(size_t i =0;i <arguments.size();i++){
		if(static_cast<Variable*>(arguments[i]) == var) return (int)i;
	}
	return -1;
}
Node*  Function::createReference(){
	return new FunctionReference(this);
}
Type*  Function::argumentType()  {
	assert(isFlagSet(Node::RESOLVED));
	assert(!isFlagSet(HAS_PATTERN_ARGUMENTS));
	auto args = arguments.size();
	if(args == 0) return intrinsics::types::Void;
	else if(args == 1) return arguments[0]->type.type();
	std::vector<AnonymousAggregate::Field> fields;
	for(auto i = arguments.begin();i!=arguments.end();++i){
		AnonymousAggregate::Field field = { SymbolID(),(*i)->type.type() };
		fields.push_back(field);
	}
	return AnonymousAggregate::create(&fields[0],fields.size());
}
Type*  Function::returns() const {
	assert(isFlagSet(Node::RESOLVED));
	return _returnType.type();
}
void   Function::makeAllArgumentsExpendable(){
	assert(!isResolved());
	for(auto i = arguments.begin();i!=arguments.end();i++){
		(*i)->setFlag(Argument::IS_EXPENDABLE);
	}
	setFlag(HAS_EXPENDABLE_ARGUMENTS);
}

//field access macro function
void Function::makeFieldAccess(int fieldId) {
	setFlag(FIELD_ACCESS_FUNCTION);
	ctfeRegisterCount = fieldId;
}
int  Function::getField() const {
	return ctfeRegisterCount;
}

Node*  Function::duplicate(DuplicationModifiers* mods) const{
	return const_cast<Function*>(this)->reallyDuplicate(mods);
}
Function* Function::reallyDuplicate(DuplicationModifiers* mods,bool redefine){
	debug("Duplicating function %s",label());
	auto func = new Function(label(),location());
	if(redefine) mods->duplicateDefinition(const_cast<Function*>(this),func);
	func->body.scope->parent = mods->target;
	mods->target = func->body.scope;
	
	//args
	for(auto i = arguments.begin();i!=arguments.end();++i){
		func->addArgument((*i)->reallyDuplicate(func,mods));
	}
	duplicateReturnBody(mods,func);
	return func;
}
Function* Function::duplicateReturnBody(DuplicationModifiers* mods,Function* func) const {
	func->_returnType = _returnType.duplicate(mods);
	
	auto oldRed = mods->returnValueRedirector;
	mods->returnValueRedirector = nullptr;
	body._duplicate(&func->body,mods);
	mods->returnValueRedirector = oldRed;
	func->ctfeRegisterCount = ctfeRegisterCount;
	func->inliningWeight = inliningWeight;
	if(generatedFunctionParent) {
		assert(false);
	}
	copyProperties(func);
	mods->target = mods->target->parent;//leaveBlock
	return func;
}

//Imported scope
ImportedScope::ImportedScope(SymbolID name,Location& location) : PrefixDefinition(name,location),scope(nullptr),_reference(this) {
	setFlag(Node::RESOLVED);
	visibilityMode(data::ast::PRIVATE);
}

//Macroes
PrefixMacro::PrefixMacro(Function* f) : PrefixDefinition(f->label(),f->location()),function(f) {}
Node* PrefixMacro::duplicate(DuplicationModifiers* mods) const {
	auto macro = new PrefixMacro(function->reallyDuplicate(mods,false));
	mods->duplicateDefinition(const_cast<PrefixMacro*>(this),macro);
	return copyProperties(macro);
}

InfixMacro ::InfixMacro (Function* f,Node* stickiness) : InfixDefinition(f->label(),0,f->location()),function(f),stickinessExpression(stickiness) {
}
bool InfixMacro::applyProperty(SymbolID name,Node* value){
	if(name == "precedence" && value){
		stickinessExpression = value;
		return true;
	}
	return false;
}
Node* InfixMacro::duplicate(DuplicationModifiers* mods) const {
	auto macro = new InfixMacro(function->reallyDuplicate(mods,false),stickinessExpression->duplicate(mods));
	mods->duplicateDefinition(const_cast<InfixMacro*>(this),macro);
	return copyProperties(macro);
}
