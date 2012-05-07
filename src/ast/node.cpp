#include "../base/base.h"
#include "../base/format.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../syntax/location.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "../intrinsics/ast.h"
#include "../intrinsics/types.h"

struct NodeToString: NodeVisitor {
	std::ostream& stream;
	NodeToString(std::ostream& ostream) : stream(ostream) {}

	Node* visit(IntegerLiteral* node){
		stream<<node->integer;
		return node;
	}
	Node* visit(ErrorExpression* node){
		stream<<"error";
		return node;
	}
	Node* visit(UnitExpression* node){
		stream<<"()";
		return node;
	}
	Node* visit(WildcardExpression* node){
		stream<<"_";
		return node;
	}
	Node* visit(ExpressionReference* node){
		stream<<"eref "<<node->expression;
		return node;
	}
	Node* visit(ImportedScopeReference* node){
		stream<<"scope-ref "<<node->scope->id;
		return node;
	}
	Node* visit(VariableReference* node){
		stream<<"variable "<<node->variable->id;
		return node;
	}
	Node* visit(FunctionReference* node){
		stream<<"ref func "<<node->function()->id;
		return node;
	}

	Node* visit(OverloadSetExpression* node){
		stream<<"overloadset "<<node->symbol;
		return node;
	}
	Node* visit(CallExpression* node){
		stream<<"call "<<node->object<<" with "<<node->arg;
		return node;
	}
	Node* visit(FieldAccessExpression* node){
		stream<<node->object<<" f. "<<node->object->_returnType()->record->fields[node->field].name;
		return node;
	}
	Node* visit(AccessExpression* node){
		stream<<node->object<<" . "<<node->symbol;
		return node;
	}
	Node* visit(AssignmentExpression* node){
		stream<<node->object<<" = "<<node->value;
		return node;
	}
	Node* visit(ReturnExpression* node){
		stream<<"return";
		if(node->value) stream<<' '<<node->value;
		return node;
	}
	Node* visit(PointerOperation* node){
		stream<<(node->kind==PointerOperation::ADDRESS?'&':'*')<<node->expression;
		return node;
	}
	Node* visit(MatchExpression* node){
		stream<<"match "<<node->object;
		for(auto i=node->cases.begin();i!=node->cases.end();i++){
			stream<<'|'<<(*i).pattern<<" => ";
			if((*i).consequence) stream<<(*i).consequence<<' ';
			if((*i).fallThrough) stream<<"fallthrough";
		}
		return node;
	}
	Node* visit(TupleExpression* node){
		auto i = node->children.begin();
		while(1){
			stream<<(*i);
			++i;
			if( i == node->children.end() ) break;
			stream<<',';
		}
		return node;
	}
	Node* visit(BlockExpression* node){
		stream<<"{\n  ";
		for(auto i =node->children.begin();i != node->children.end(); ++i) stream<<(*i)<<";\n  "; 
		stream<<"}";
		return node;
	}
	Node* visit(WhileExpression* node){
		stream<<"while "<<node->condition<<" do "<<node->body;
		return node;
	}
	Node* visit(TypeExpression* node){
		stream<<node;
		return node;
	}
};
std::ostream& operator<< (std::ostream& stream,Node* node){
	stream<<'(';
	NodeToString visitor(stream);
	node->accept(&visitor);
	stream<<"):"<<node->_returnType();
	return stream;
}


TypeExpression* Node::_returnType() const {
	return intrinsics::types::Void;
}

// Error expression
TypeExpression* ErrorExpression::_returnType() const {
	return intrinsics::types::Unresolved;
}
ErrorExpression* errorInstance = nullptr;
Node* ErrorExpression::duplicate() const {
	return errorInstance;
};
ErrorExpression* ErrorExpression::getInstance() {
	if(errorInstance) return errorInstance;
	else return errorInstance = new ErrorExpression;
}

// Integer literals
IntegerLiteral::IntegerLiteral(const BigInt& integer){
	this->integer = integer;
	_type = nullptr;
}
TypeExpression* IntegerLiteral::_returnType() const{
	if(_type) return _type;
	//TODO <0 integers
	if(integer <= intrinsics::types::int32->integer->max) return intrinsics::types::int32;
	else if(integer <= intrinsics::types::uint32->integer->max) return intrinsics::types::uint32;
	else if(integer <= intrinsics::types::int64->integer->max) return intrinsics::types::int64;
	else return intrinsics::types::uint64;
}
Node* IntegerLiteral::duplicate() const {
	auto dup = new IntegerLiteral(integer);
	dup->_type = _type;
	return dup;
};

// Unit expression
TypeExpression* UnitExpression::_returnType() const {
	return intrinsics::types::Void;
}
UnitExpression* unitInstance = nullptr;
Node* UnitExpression::duplicate() const {
	return unitInstance;
};
UnitExpression* UnitExpression::getInstance() {
	if(unitInstance) return unitInstance;
	else return unitInstance = new UnitExpression;
}

// Wildcard expression
TypeExpression* WildcardExpression::_returnType() const {
	return intrinsics::types::Void;//???
}
WildcardExpression* wildInstance = nullptr;
Node* WildcardExpression::duplicate() const {
	return wildInstance;
};
WildcardExpression* WildcardExpression::getInstance() {
	if(wildInstance) return wildInstance;
	else return wildInstance = new WildcardExpression;
}

// Expression reference
ExpressionReference::ExpressionReference(Node* node) : expression(node){}
TypeExpression* ExpressionReference::_returnType() const {
	return intrinsics::types::Expression;
}
Node* ExpressionReference::duplicate() const {
	return new ExpressionReference(expression->duplicate());
};

//Scope reference
ImportedScopeReference::ImportedScopeReference(ImportedScope* scope){
	this->scope = scope;
}
TypeExpression* ImportedScopeReference::_returnType() const {
	return intrinsics::types::Unresolved;
}
Node* ImportedScopeReference::duplicate() const {
	return scope->reference();
}


// Variable reference
VariableReference::VariableReference(Variable* variable){
	this->variable = variable;
}
TypeExpression* VariableReference::_returnType() const {
	return variable->type.type();
}
Node* VariableReference::duplicate() const {
	return variable->reference();
};

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

TypeExpression* TupleExpression::_returnType() const {
	assert(type);
	return type;
}
Node* TupleExpression::duplicate() const {
	auto dup = new TupleExpression;
	for(auto i = children.begin();i!=children.end();i++){
		dup->children.push_back((*i)->duplicate());
	}
	dup->type = type;
	return dup;
};

// Assignment expression
AssignmentExpression::AssignmentExpression(Node* object,Node* value){
	this->object = object;
	this->value = value;
}
TypeExpression* AssignmentExpression::_returnType() const {
	return object->_returnType();
}
Node* AssignmentExpression::duplicate() const {
	return new AssignmentExpression(object->duplicate(),value->duplicate());
}

// Return expression
ReturnExpression::ReturnExpression(Node* expression) : value(expression) {}
Node* ReturnExpression::duplicate() const {
	return new ReturnExpression(value?value->duplicate():nullptr);
}

//Pointer operation
PointerOperation::PointerOperation(Node* expression,int type){
	this->expression = expression;
	kind = type;
}
TypeExpression* PointerOperation::_returnType() const {
	return intrinsics::types::Unresolved;//TODO
}
Node* PointerOperation::duplicate() const {
	return new PointerOperation(expression->duplicate(),kind);
}

// Match expression
MatchExpression::MatchExpression(Node* object){
	this->object = object;
}
TypeExpression* MatchExpression::_returnType() const {
	return intrinsics::types::Void;//TODO
}
Node* MatchExpression::duplicate(){
	auto dup = new MatchExpression(object);
	dup->cases.reserve(cases.size());
	for(auto i = cases.begin();i!=cases.end();i++){
		dup->cases.push_back(Case((*i).pattern->duplicate(),(*i).consequence ? (*i).consequence->duplicate() : nullptr,(*i).fallThrough));
	}
	return dup;
}


FunctionReference::FunctionReference(Function* function) : _function(function) {
}
TypeExpression* FunctionReference::_returnType() const {
	return intrinsics::types::Unresolved;//TODO;
}
Function* FunctionReference::function() const {
	assert(_function->resolved());
	return _function;
}

// Field access expression
FieldAccessExpression::FieldAccessExpression(Node* object,int field){
	this->object = object;
	this->field = field;
	assert(object->_returnType()->type == TypeExpression::RECORD);
}
TypeExpression* FieldAccessExpression::_returnType() const {
	return object->_returnType()->record->fields[field].type.type();
}
Node* FieldAccessExpression::duplicate() const {
	return new FieldAccessExpression(object->duplicate(),field);
}

// Access expression
AccessExpression::AccessExpression(Node* object,SymbolID symbol){
	this->object = object;
	this->symbol = symbol;
	this->passedFirstEval = false;
}
TypeExpression* AccessExpression::_returnType() const{
	return intrinsics::types::Unresolved;
}
Node* AccessExpression::duplicate() const {
	return new AccessExpression(object->duplicate(),symbol);//duplicate passed first eval??? - no
}



TypeExpression* CallExpression::_returnType() const {
	if( auto refFunc = object->asFunctionReference()){
		return refFunc->function()->returnType;
	}
	return intrinsics::types::Unresolved;
}

//While expression
WhileExpression::WhileExpression(Node* condition,Node* body){
	this->condition = condition;
	this->body = body;
}
Node* WhileExpression::duplicate() const{
	return new WhileExpression(condition->duplicate(),body->duplicate());
}

// Block expression
BlockExpression::BlockExpression(Scope* scope){
	this->scope = scope;
}
Node* BlockExpression::duplicate() const {
	auto dup = new BlockExpression(scope);//scope->dup???
	dup->children.reserve(children.size());//Single alocation
	for(auto i=children.begin();i!=children.end();i++) dup->children.push_back((*i)->duplicate());
	return dup;
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

//Constructors

OverloadSetExpression* OverloadSetExpression::create(SymbolID symbol,Scope* scope){
	auto e = new OverloadSetExpression;
	e->scope = scope;
	e->symbol = symbol;
	return e;
}
CallExpression* CallExpression::create(Node* object,Node* argument){
	auto e = new CallExpression;
	e->object = object;
	e->arg = argument;
	return e;
}

//

TypeExpression* InferredUnresolvedTypeExpression::type(){
	return kind == Type ? _type : intrinsics::types::Unresolved;
}
void InferredUnresolvedTypeExpression::infer(TypeExpression* type){
	assert(kind == Inferred);
	assert(type != intrinsics::types::Unresolved);
	assert(type->resolved());
	kind = Type;
	_type = type;
}

// TypeExpression
TypeExpression::TypeExpression(IntrinsicType* intrinsic) : type(INTRINSIC) {
	this->intrinsic = intrinsic;
}
TypeExpression::TypeExpression(IntegerType* integer) : type(INTEGER) {
	this->integer = integer;
}
TypeExpression::TypeExpression(Record* record): type(RECORD) { 
	this->record = record; 
}

bool TypeExpression::resolved() const {
	switch(type){
		case RECORD: return record->resolved();
	}
	return true;
}
TypeExpression* TypeExpression::_returnType() const {
	return resolved() ? intrinsics::types::Type : intrinsics::types::Unresolved ;
}
Node* TypeExpression::duplicate() const {
	switch(type){
		case RECORD: return record->reference();
		case INTEGER: return integer->reference();
		case INTRINSIC: return intrinsic->reference();
		default:
			throw std::runtime_error("Can't evaluate size of an unresolved type expression!");
			return nullptr;
	}
}


size_t TypeExpression::size() const {
	switch(type){
		case RECORD: return record->size();
		case INTEGER: return integer->size();
		case INTRINSIC: return intrinsic->size();
		default:
			throw std::runtime_error("Can't evaluate size of an unresolved type expression!");
	}
}
bool TypeExpression::isSame(TypeExpression* other){
	return this == other;//like a boss
}
Node* TypeExpression::assignableFrom(Node* expression,TypeExpression* type) {
	if(this == type) return expression;//like a baws

	else if(this->type == INTEGER && type->type == INTEGER){
		//literal integer constants.. check to see if the type can accept it's value
		if(auto intConst = expression->asIntegerLiteral()){
			if(!intConst->_type && this->integer->isValid(intConst->integer)) return expression;
		}
	}else if(type->type == RECORD){
		//Extenders fields
		for(size_t i = 0;i < type->record->fields.size();i++){
			Record::Field* field = &type->record->fields[i];
			if(field->isExtending && field->type.resolved()){
				auto dummyFieldAcess = new FieldAccessExpression(expression,i);
				if(auto assigns = this->assignableFrom(dummyFieldAcess,field->type.type())) return assigns;
				delete dummyFieldAcess;
			}
		}
	}

	return nullptr;
}
std::ostream& operator<< (std::ostream& stream,TypeExpression* node){
	switch(node->type){
		case TypeExpression::RECORD: stream<<node->record; break;
		case TypeExpression::INTEGER: stream<<node->integer->id; break;
		case TypeExpression::INTRINSIC: stream<<node->intrinsic->id; break;
	}
	return stream;
}
