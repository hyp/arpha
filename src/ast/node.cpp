#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../compiler.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "interpret.h"
#include "../intrinsics/types.h"

void DuplicationModifiers::expandArgument(Argument* original,Node* value){
	redirectors[reinterpret_cast<void*>(static_cast<Variable*>(original))] = std::make_pair(reinterpret_cast<void*>(value),true);
}

void DuplicationModifiers::duplicateDefinition(Argument* original,Argument* duplicate){
	//target->define(duplicate); argument defined in duplicate function addChild 
	redirectors[reinterpret_cast<void*>(static_cast<Variable*>(original))] = std::make_pair(reinterpret_cast<void*>(static_cast<Variable*>(duplicate)),false);
}
void DuplicationModifiers::duplicateDefinition(Variable* original,Variable* duplicate){
	target->define(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(Function* original,Function* duplicate){
	target->defineFunction(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(Record* original,Record* duplicate){
	target->define(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(TypeDeclaration* original,TypeDeclaration* duplicate){
	target->define(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(PrefixMacro* original,PrefixMacro* duplicate){
	target->define(duplicate);
}
void DuplicationModifiers::duplicateDefinition(InfixMacro* original,InfixMacro* duplicate){
	target->define(duplicate);
}

void Node::setFlag(uint16 id){
	flags |= id;
}
bool Node::isFlagSet(uint16 id) const {
	return (flags & id) == id;
}

Type* Node::returnType() const {
	return intrinsics::types::Void;
}
Node* Node::copyProperties(Node* dest) const {
	dest->_location = _location;
	dest->_label = _label;
	dest->flags = flags;
	return dest;
}

Node* Node::copyLocationSymbol(Node* dest) const {
	dest->_location = _location;
	dest->_label = _label;
	return dest;
}

uint8 DefinitionNode::visibilityMode() const {
	return Visibility::Public; //TODO
}
bool  DefinitionNode::isPublic() const {
	return true;
}
void DefinitionNode::visibilityMode(uint8 mode){
}

PrefixDefinition::PrefixDefinition(SymbolID name,Location& location){
	_label = name;
	_location = location;
}

InfixDefinition::InfixDefinition(SymbolID name,int stickiness,Location& location){
	_label = name;
	_location = location;
	this->stickiness = stickiness;
	//visibilityMode = Visibility::Public;
}

Node* NodeList::duplicateChildren(NodeList* dest,DuplicationModifiers* mods) const {
	for(auto i = begin();i!=end();i++)
		dest->addChild((*i)->duplicate(mods));
	return dest;
}

// Integer literals
IntegerLiteral::IntegerLiteral(const BigInt& integer){
	this->integer = integer;
	_type = nullptr;
}
Type* IntegerLiteral::returnType() const{
	if(_type) return _type->asType();
	//TODO integers overflowing int64 max/min
	if(integer.isNegative()){
		if(/* >= */!(integer < intrinsics::types::int32->integer->min)) return intrinsics::types::int32;
		else return intrinsics::types::int64;
	}
	else{
		if(integer <= intrinsics::types::int32->integer->max) return intrinsics::types::int32;
		else if(integer <= intrinsics::types::uint32->integer->max) return intrinsics::types::uint32;
		else if(integer <= intrinsics::types::int64->integer->max) return intrinsics::types::int64;
		else return intrinsics::types::uint64;
	}
}
Node* IntegerLiteral::duplicate(DuplicationModifiers* mods) const {
	auto dup = new IntegerLiteral(integer);
	dup->_type = _type;
	return copyProperties(dup);
}

FloatingPointLiteral::FloatingPointLiteral(const double v): value(v) {}
Type* FloatingPointLiteral::returnType() const {
	return intrinsics::types::int32;//TODO
}
Node* FloatingPointLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FloatingPointLiteral(value));
}


BoolExpression::BoolExpression(const bool v) : value(v) { }
Type* BoolExpression::returnType() const {
	return intrinsics::types::boolean;
}
Node* BoolExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new BoolExpression(value));
}

StringLiteral::StringLiteral(memory::Block& block){
	this->block.aquire(block);
}

StringLiteral::StringLiteral(SymbolID symbol){
	if(!symbol.isNull())
		block.construct(symbol.ptr(),symbol.length());
	else block.construct("",0);
}
Type* StringLiteral::returnType() const{
	return new Type(Type::POINTER_BOUNDED,intrinsics::types::uint8);
}
Node* StringLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new StringLiteral(block.duplicate()));
}

ArrayLiteral::ArrayLiteral(){}
Type* ArrayLiteral::returnType() const {
	return new Type(Type::POINTER_BOUNDED,(*begin())->returnType(),size());
}
Node* ArrayLiteral::duplicate(DuplicationModifiers* mods) const {
	auto dup = new ArrayLiteral;
	return copyProperties(duplicateChildren(dup,mods));
}


// Unit expression
Type* UnitExpression::returnType() const {
	return intrinsics::types::Void;
}
Node* UnitExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnitExpression);
};

//Scope reference
ImportedScopeReference::ImportedScopeReference(ImportedScope* scope){
	this->scope = scope;
}
Type* ImportedScopeReference::returnType() const {
	return intrinsics::types::Void;//TODO
}
Node* ImportedScopeReference::duplicate(DuplicationModifiers* mods) const {
	return scope->reference();
}

// Variable reference
VariableReference::VariableReference(Variable* variable){
	this->variable = variable;
}
Type* VariableReference::returnType() const {
	return variable->referenceType();
}
Node* VariableReference::duplicate(DuplicationModifiers* mods) const {
	if(mods->expandedMacroOptimization){
		if(auto value = mods->expandedMacroOptimization->getValue(variable)){
			if(auto v = value->asNodeReference()){
				//Simplify blocks
				auto node = v->node();
				if(auto block = node->asBlockExpression()){
					if(block->scope->numberOfDefinitions() == 0 && block->children.size() == 1) node = block->children[0];
				}
				return node->duplicate(mods);
			}
			return value->duplicate(mods);
		}
	}
	auto red = mods->redirectors.find(variable);
	if(red != mods->redirectors.end()){
		Node* result;
		if((*red).second.second) result = reinterpret_cast<Node*>((*red).second.first)->duplicate(mods);
		else result = new VariableReference(reinterpret_cast<Variable*>((*red).second.first));
		return copyProperties(result);
	}
	return copyProperties(new VariableReference(variable));
}

// Tuple expression
TupleExpression::TupleExpression() : type(nullptr) {}
TupleExpression::TupleExpression(Node* a,Node* b) : type(nullptr) {
	if( auto aIsTuple = a->asTupleExpression() ){
		children = aIsTuple->children;	
		delete a;
	}
	else children.push_back(a);
	children.push_back(b);
}
Type* TupleExpression::returnType() const {
	assert(type);
	return type;
}
Node* TupleExpression::duplicate(DuplicationModifiers* mods) const {
	auto dup = new TupleExpression;
	dup->type = type;
	return copyProperties(duplicateChildren(dup,mods));
};

// Assignment expression
AssignmentExpression::AssignmentExpression(Node* object,Node* value){
	this->object = object;
	this->value = value;
	isInitializingAssignment = false;
}
Type* AssignmentExpression::returnType() const {
	return intrinsics::types::Void;//object->returnType();
}
Node* AssignmentExpression::duplicate(DuplicationModifiers* mods) const {
	auto e = new AssignmentExpression(object->duplicate(mods),value->duplicate(mods));
	e->isInitializingAssignment = isInitializingAssignment;
	return copyProperties(e);
}

// Return expression
ReturnExpression::ReturnExpression(Node* expression)  {
	this->expression = expression;
}
Node* ReturnExpression::duplicate(DuplicationModifiers* mods) const {
	if(mods->returnValueRedirector){
		auto assign = new AssignmentExpression(new VariableReference(mods->returnValueRedirector),expression->duplicate(mods));
		assign->isInitializingAssignment = true;//NB when mixing in we assign the return to a immutable value
		return copyProperties(assign);
	}
	return copyProperties(new ReturnExpression(expression->duplicate(mods)));
}

ControlFlowExpression::ControlFlowExpression(int type) : kind(type) {
}
Node* ControlFlowExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new ControlFlowExpression(kind));
}

//Pointer operation
PointerOperation::PointerOperation(Node* expression,int type){
	this->expression = expression;
	kind = type;
}
Type* PointerOperation::returnType() const {
	assert(isResolved());
	auto next = expression->returnType();
	if(isAddress()) return new Type(Type::POINTER,next);
	assert(next->isPointer());
	return next->argument;
}
Node* PointerOperation::duplicate(DuplicationModifiers* mods) const {
	auto result = new PointerOperation(expression->duplicate(mods),kind);
	return result;
}

IfExpression::IfExpression(Node* condition,Node* consequence,Node* alternative){
	this->condition = condition;
	this->consequence = consequence;
	this->alternative = alternative;
}
Type* IfExpression::returnType() const {
	auto cr = consequence->returnType();
	if(cr->isSame(alternative->returnType())) return cr;
	return intrinsics::types::Void;
}
Node* IfExpression::duplicate(DuplicationModifiers* mods) const{
	auto x = new IfExpression(condition->duplicate(mods),consequence->duplicate(mods),alternative->duplicate(mods));
	return copyProperties(x);
}

// Function reference
FunctionReference::FunctionReference(Function* func) : function(func) {
	if(!func->isIntrinsic()) assert(!func->isFlagSet(Function::HAS_PATTERN_ARGUMENTS) );
}
Type* FunctionReference::returnType() const {
	assert(isResolved());
	return new Type(function->argumentType(),function->returns());
}
Node* FunctionReference::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FunctionReference(function));
}

// Field access expression
FieldAccessExpression::FieldAccessExpression(Node* object,int field){
	this->object = object;
	this->field = field;
	assert(objectsRecord());
}
Record* FieldAccessExpression::objectsRecord() const {
	auto type = object->returnType();
	if(type->type == Type::RECORD) return type->record;
	else if(type->type == Type::POINTER && type->argument->type == Type::RECORD) return type->argument->record;
	return nullptr;
}
Type* FieldAccessExpression::returnType() const {
	return objectsRecord()->fields[field].type.type();
}
Node* FieldAccessExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FieldAccessExpression(object->duplicate(mods),field));
}

// Call expression

CallExpression::CallExpression(Node* object,Node* argument){
	this->object = object;
	this->arg = argument;
}

Type* CallExpression::returnType() const {
	assert(isResolved());
	if( auto refFunc = object->asFunctionReference()){
		return refFunc->function->_returnType.isResolved() ? refFunc->function->_returnType.type() : intrinsics::types::Void;//TODO fix when function has unresolved return type!
	}
	return intrinsics::types::Void;//TODO only allow functions?
}
Node* CallExpression::duplicate(DuplicationModifiers* mods) const {
	auto e = new CallExpression(object->duplicate(mods),arg->duplicate(mods));
	return copyProperties(e);
}

VariantNode::VariantNode(uint32 kind) : _kind(kind) {}
uint32 VariantNode::kind() const {
	return _kind;
}
void VariantNode::mutate(uint32 newKind){
	_kind = newKind;
}

UnaryOperation::UnaryOperation(uint32 kind,Node* expression) : VariantNode(kind) {
	this->expression = expression;
}
Type* UnaryOperation::returnType() const {
	switch(kind()){
	case BOOL_NOT: intrinsics::types::boolean;
	case MINUS: return expression->returnType();
	}
}
Node* UnaryOperation::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnaryOperation(kind(),expression->duplicate(mods)));
}

BinaryOperation::BinaryOperation(uint32 kind,Node* a,Node* b): VariantNode(kind) {
	this->a = a;
	this->b = b;
}
Type* BinaryOperation::returnType() const {
	switch(kind()){
	case BOOL_AND: case BOOL_OR: 
	case EQUALS: case LESS: case GREATER:
		return intrinsics::types::boolean;
	case ADD: case SUBTRACT: case MULTIPLY: case DIVIDE: case MOD: 
		return a->returnType();
	}
	return nullptr;
}
Node* BinaryOperation::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new BinaryOperation(kind(),a->duplicate(mods),b->duplicate(mods)));
}

//Loop expression
LoopExpression::LoopExpression(Node* body){
	this->body = body;
}
Node* LoopExpression::duplicate(DuplicationModifiers* mods) const{
	return copyProperties(new LoopExpression(body->duplicate(mods)));
}

// Block expression
BlockExpression::BlockExpression(){
	this->scope = new Scope(nullptr);
}
BlockExpression::BlockExpression(Scope* scope){
	this->scope = scope;
}
Type *BlockExpression::returnType() const {
	if(isFlagSet(RETURNS_LAST_EXPRESSION) && size()>0) return (*(end()-1))->returnType();
	return intrinsics::types::Void;
}

void BlockExpression::_duplicate(BlockExpression* dest,DuplicationModifiers* mods) const {
	copyProperties(duplicateChildren(dest,mods));
}
BlockExpression* BlockExpression::duplicateMixin(DuplicationModifiers* mods) const {
	auto dup = new BlockExpression(mods->target);
	copyProperties(duplicateChildren(dup,mods));
	dup->setFlag(USES_PARENT_SCOPE | RETURNS_LAST_EXPRESSION);
	return dup;
}
Node* BlockExpression::duplicate(DuplicationModifiers* mods) const {
	if(isFlagSet(USES_PARENT_SCOPE)){
		auto dup = new BlockExpression(mods->target);
		_duplicate(dup,mods);
		return dup;
	}
	else{
		auto dup = new BlockExpression();
		dup->scope->parent = mods->target;
		mods->target = dup->scope;
		_duplicate(dup,mods);
		mods->target = mods->target->parent;
		return dup;
	}
}

CastExpression::CastExpression(Node* object,Type* type) {
	this->object = object;
	this->type = type;
}
Node* CastExpression::duplicate(DuplicationModifiers* mods) const {;
	return copyProperties(new CastExpression(object->duplicate(mods),type));
}
Type* CastExpression::returnType() const {
	return type;
}

NodeReference::NodeReference(Node* node) : _node(node) {
}
Type* NodeReference::returnType() const { 
	return intrinsics::types::NodePointer; 
}
Node* NodeReference::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new NodeReference(_node->duplicate(mods)));
}

//

TypeReference::TypeReference(Type* type){
	this->type = type;
	if(type->isResolved()) setFlag(RESOLVED|CONSTANT);
}
Type* TypeReference::returnType() const {
	return intrinsics::types::Type;
}
Node* TypeReference::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new TypeReference(type));
}

//Other,temporary nodes

ExpressionVerifier::ExpressionVerifier(const Location& loc,Node* child,Type* typeExpected) : expression(child),expectedType(typeExpected) {
	assert(child);assert(typeExpected);
	_location = loc;
}
Node* ExpressionVerifier::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new ExpressionVerifier(_location,expression->duplicate(mods),expectedType/* NB no duplicate */));
}

UnresolvedSymbol::UnresolvedSymbol(const Location& loc,SymbolID sym,Scope* scope) : symbol(sym),explicitLookupScope(scope) {
	assert(!sym.isNull());
	_location = loc;
}
Node* UnresolvedSymbol::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnresolvedSymbol(_location,symbol,explicitLookupScope));
}
AccessExpression::AccessExpression(Node* object,SymbolID symbol){
	this->object = object;
	this->symbol = symbol;
}
Node* AccessExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new AccessExpression(object->duplicate(mods),symbol));
}
ErrorExpression* errorInstance = nullptr;
Node* ErrorExpression::duplicate(DuplicationModifiers* mods) const {
	return errorInstance;//NB an instance is already created, so we dont have to use getInstance
};
ErrorExpression* ErrorExpression::getInstance() {
	if(errorInstance) return errorInstance;
	else return errorInstance = new ErrorExpression;
}

MatchResolver::MatchResolver(Node* object){ this->object = object; }
Node* MatchResolver::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(duplicateChildren(new MatchResolver(object->duplicate(mods)),mods));
}

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE_IMPLEMENTATION(T) \
	Node* T::accept(NodeVisitor* visitor) { \
		return visitor->visit(this);        \
	}										\
	T* T::as##T() { return this; }                        

NODE_LIST(DECLARE_NODE_IMPLEMENTATION)

#undef DECLARE_NODE_IMPLEMENTATION

