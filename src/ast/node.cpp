#include "../base/base.h"
#include "../base/format.h"
#include "../base/bigint.h"
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
	Node* visit(ExpressionReference* node){
		stream<<"eref "<<node->expression;
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
	Node* visit(AccessExpression* node){
		stream<<node->object<<" . "<<node->symbol;
		return node;
	}
	Node* visit(AssignmentExpression* node){
		stream<<node->object<<" = "<<node->value;
		return node;
	}
	Node* visit(ReturnExpression* node){
		stream<<"return "<<node->value;
		return node;
	}
	Node* visit(MatchExpression* node){
		stream<<"match "<<node->object;
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

// Expression reference
ExpressionReference::ExpressionReference(Node* node) : expression(node){}
TypeExpression* ExpressionReference::_returnType() const {
	return intrinsics::types::Expression;
}
Node* ExpressionReference::duplicate() const {
	return new ExpressionReference(expression->duplicate());
};

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
	return new ReturnExpression(value);
}

MatchExpression::MatchExpression(Node* object){
	this->object = object;
}

// Match expression
TypeExpression* MatchExpression::_returnType() const {
	return intrinsics::types::Void;//TODO
}
Node* MatchExpression::duplicate(){
	auto dup = new MatchExpression(object);
	for(auto i = cases.begin();i!=cases.end();i++){
		Case _case;
		_case.node = (*i).node->duplicate();
		_case.consequence = (*i).consequence ? (*i).consequence->duplicate() : nullptr;
		dup->cases.push_back(_case);
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


TypeExpression* CallExpression::_returnType() const {
	if( auto refFunc = object->asFunctionReference()){
		return refFunc->function()->returnType;
	}
	return intrinsics::types::Unresolved;
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
AccessExpression* AccessExpression::create(Node* object,SymbolID symbol,Scope* scope){
	auto e = new AccessExpression;
	e->object = object;
	e->symbol = symbol;
	e->scope = scope;
	e->passedFirstEval = false;
	return e;
}

BlockExpression* BlockExpression::create(Scope* scope){
	auto e = new BlockExpression;
	e->scope = scope;
	return e;
}
WhileExpression* WhileExpression::create(Node* condition,Node* body){
	auto e = new WhileExpression;
	e->condition = condition;
	e->body = body;
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
