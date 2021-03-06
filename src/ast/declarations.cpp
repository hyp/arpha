
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../compiler.h"
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
Variable::Variable(SymbolID name,Location& location) : PrefixDefinition(name,location),value(nullptr) {
	setFlag(LOOKUP_HIDE_BEFORE_DECLARATION);
	generatorDataRound = 0;

	_owner = nullptr;
}
Variable::Variable(SymbolID name,Location& location,Scope* owner,Node* value) : PrefixDefinition(name,location){
	setFlag(LOOKUP_HIDE_BEFORE_DECLARATION);
	generatorDataRound = 0;

	_owner = owner;
	specifyType(value->returnType());
	setFlag(IS_IMMUTABLE);
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
	assert(isFlagSet(IS_IMMUTABLE));
	assert(value->isResolved() && value->isConst());
	//assert(!this->value);
	this->value = value;
	setFlag(CONSTANT_SUBSTITUTE);

	debug("Setting value %s to variable %s",value,label());
}

Node* Variable::duplicate(DuplicationModifiers* mods) const {
	auto duplicatedReplacement = new Variable(label(),location());
	duplicatedReplacement->type = type.duplicate(mods);
	duplicatedReplacement->_owner = mods->target;
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
	flags &= (~LOOKUP_HIDE_BEFORE_DECLARATION);//Not required
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

//Overload set

Overloadset::Overloadset(Function* firstFunction) : PrefixDefinition(firstFunction->label(),firstFunction->location()) {
	visibilityMode(firstFunction->visibilityMode());
	functions.push_back(firstFunction);
	if(firstFunction->isTypeTemplate()) setFlag(TYPE_GENERATOR_SET);
	setFlag(Node::RESOLVED);
}
Overloadset* Overloadset::asOverloadset(){
	 return this;
}
void Overloadset::push_back(Function* function){
	if(isFlagSet(TYPE_GENERATOR_SET) && !function->isTypeTemplate()){
		error(function,"Can't add the function %s to type overload set",function->label());
		return;
	}
	if(visibilityMode() == data::ast::PRIVATE && function->isPublic()) visibilityMode(data::ast::PUBLIC);
	functions.push_back(function);//TODO check against same functions
}

//Function

Function::Function(SymbolID name,Location& location) : PrefixDefinition(name,location), body(), allArgMatcher(body.scope,nullptr) {
	intrinsicCTFEbinder = nullptr;
	generatedFunctionParent = nullptr;
	body.scope->_functionOwner = this;
	cc = data::ast::Function::ARPHA;
	miscFlags = 0;
}

#define IMPL_PROP_SET(flag) { miscFlags |= flag; }

bool   Function::applyProperty(SymbolID name,Node* value){
	if(name == "intrinsic"){
		IMPL_PROP_SET(data::ast::Function::Internal::INTRINSIC);
		IMPL_PROP_SET(data::ast::Function::Internal::ALLOW_NO_BODY);
	} else if(name == "external"){
		if(isIntrinsic()){
			return false;
		}
		IMPL_PROP_SET(data::ast::Function::Internal::EXTERNAL);
		IMPL_PROP_SET(data::ast::Function::Internal::ALLOW_NO_BODY);
		if(value){
			auto str = value->asStringLiteral();
			if(!str) return false;
			externalLib = str->block.ptr();
			auto ext = System::path::extension(externalLib);
			if(ext){
				if(!strcmp(ext,"dll")){
					IMPL_PROP_SET(data::ast::Function::Internal::EXTERNAL_DLLIMPORT);
				} else if(!strcmp(ext,"so") && !strcmp(ext,"lib") && !strcmp(ext,"a")){
					return false;
				}
			}
		}
	} 
	else if(name == "callingConvention"){
		if(!value) return false;
		if(auto access= value->asAccessExpression()){
			if(access->symbol == "fast") cc = data::ast::Function::ARPHA;
			else if(access->symbol == "cold") cc = data::ast::Function::COLD;
			else if(access->symbol == "c") cc = data::ast::Function::CCALL;
			else if(access->symbol == "stdcall") cc = data::ast::Function::STDCALL;
			else return false;
		}
		else return false;
	}
	else if(name == "winapi"){
		cc = data::ast::Function::STDCALL;
	}
	else if(name == "nonthrow"){
		setNonthrow();
	} else if(name == "unittest"){
		miscFlags |= data::ast::Function::Internal::UNITTEST;
	}
	else return false;
	return true;
}
data::ast::Function::CallConvention Function::callingConvention() const {
	return (data::ast::Function::CallConvention)cc;
}
void Function::setNonthrow(){
	miscFlags |= data::ast::Function::NONTHROW;
}
Scope* Function::owner() const {
	return body.scope->parent;
}
Scope* Function::parameterPatternMatchingScope() const {
	return body.scope;
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
Type*  Function::argumentType() const  {
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
Type* Function::returnType() const {
	if(owner()->functionOwner() && !isFlagSet(HAS_PATTERN_ARGUMENTS) && !isFlagSet(HAS_EXPENDABLE_ARGUMENTS)){
		return FunctionPointer::get(argumentType(),returns(),callingConvention());
	}
	return Node::returnType();
}

//field access macro function

void Function::makeNoBody()                    IMPL_PROP_SET(data::ast::Function::Internal::NO_BODY)
void Function::makeNoBodyAllowed()             IMPL_PROP_SET(data::ast::Function::Internal::ALLOW_NO_BODY)
void Function::makeIntrinsic()                 IMPL_PROP_SET(data::ast::Function::Internal::INTRINSIC)
void Function::makeIntrinsicReturningPattern() IMPL_PROP_SET(data::ast::Function::Internal::INTRINSIC_RETURNS_PATTERN)
void Function::makeIntrinsicOperation(data::ast::Operations::Kind op){
	IMPL_PROP_SET(data::ast::Function::Internal::INTRINSIC_OPERATION);
	ctfeRegisterCount = op;
}
void Function::makeFieldAccess(int fieldId) {
	IMPL_PROP_SET(data::ast::Function::Internal::MACRO_FIELD_ACCESS);
	assert(fieldId <= std::numeric_limits<uint16>::max());
	ctfeRegisterCount = fieldId;
}
int  Function::getField() const {
	return ctfeRegisterCount;
}
void Function::makeTypeTemplate(TypeDeclaration* node){
	makeAllArgumentsExpendable();
	IMPL_PROP_SET(data::ast::Function::Internal::TYPE_TEMPLATE)
	body.addChild(node);
}
TypeDeclaration* Function::getTemplateTypeDeclaration(){
	return (*body.begin())->asTypeDeclaration();
}

#undef IMPL_PROP_SET

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
	func->cc = cc;
	func->miscFlags = miscFlags;
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
PrefixMacro::PrefixMacro(Function* f) : PrefixDefinition(f->label(),f->location()),function(f) {
	//NB: makes this invisible at resolving stage.
	setFlag(LOOKUP_HIDE_BEFORE_DECLARATION);
	generatorDataRound =0;
}
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
