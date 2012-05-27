#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../compiler.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "../intrinsics/ast.h"
#include "../intrinsics/types.h"

TypeExpression* Node::_returnType() const {
	return intrinsics::types::Void;
}
Node* Node::copyProperties(Node* dest) const {
	dest->location = location;
	dest->_label = _label;
	return dest;
}

// Integer literals
IntegerLiteral::IntegerLiteral(const BigInt& integer){
	this->integer = integer;
	_type = nullptr;
}
TypeExpression* IntegerLiteral::_returnType() const{
	if(_type) return _type;
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
};
bool IntegerLiteral::isConst() const {
	return true;
}

StringLiteral::StringLiteral(memory::Block& block){
	this->block.aquire(block);
}
TypeExpression* StringLiteral::_returnType() const{
	return intrinsics::types::int32;//TODO
}
Node* StringLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new StringLiteral(block.duplicate()));
}
bool StringLiteral::isConst() const {
	return true;
}


// Unit expression
TypeExpression* UnitExpression::_returnType() const {
	return intrinsics::types::Void;
}
Node* UnitExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnitExpression);
};

// Wildcard expression
TypeExpression* WildcardExpression::_returnType() const {
	return intrinsics::types::Void;//TODO???
}
Node* WildcardExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new WildcardExpression);
};

// Expression reference
ExpressionReference::ExpressionReference(Node* node) : expression(node){}
TypeExpression* ExpressionReference::_returnType() const {
	return intrinsics::types::Expression;
}
Node* ExpressionReference::duplicate(DuplicationModifiers* mods) const {
	return new ExpressionReference(expression->duplicate(mods));
};

//Scope reference
ImportedScopeReference::ImportedScopeReference(ImportedScope* scope){
	this->scope = scope;
}
TypeExpression* ImportedScopeReference::_returnType() const {
	return intrinsics::types::Void;//TODO
}
Node* ImportedScopeReference::duplicate(DuplicationModifiers* mods) const {
	return scope->reference();
}

// Variable reference
VariableReference::VariableReference(Variable* variable){
	this->variable = variable;
}
bool VariableReference::isResolved() const {
	return variable->isResolved();
}
bool VariableReference::isLocal() const {
	return variable->isLocal();
}
TypeExpression* VariableReference::_returnType() const {
	return variable->type.type();
}
Node* VariableReference::duplicate(DuplicationModifiers* mods) const {
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

TypeExpression* TupleExpression::_returnType() const {
	assert(type);
	return type;
}
bool TupleExpression::isConst() const{
	for(auto i = children.begin();i != children.end();i++){
		if(!(*i)->isConst()) return false;
	}
	return true;
}
bool TupleExpression::isResolved() const {
	return type != nullptr;
}
Node* TupleExpression::duplicate(DuplicationModifiers* mods) const {
	auto dup = new TupleExpression;
	for(auto i = children.begin();i!=children.end();i++){
		dup->children.push_back((*i)->duplicate(mods));
	}
	dup->type = type ? type->duplicate(mods)->asTypeExpression() : nullptr;
	return copyProperties(dup);
};

// Assignment expression
AssignmentExpression::AssignmentExpression(Node* object,Node* value){
	this->object = object;
	this->value = value;
	isInitializingAssignment = false;
	_resolved = false;
}
TypeExpression* AssignmentExpression::_returnType() const {
	assert(_resolved);
	return object->_returnType();
}
bool AssignmentExpression::isResolved() const {
	return _resolved;
}
Node* AssignmentExpression::duplicate(DuplicationModifiers* mods) const {
	auto e = new AssignmentExpression(object->duplicate(mods),value->duplicate(mods));
	e->isInitializingAssignment = isInitializingAssignment;
	e->_resolved = _resolved;
	return copyProperties(e);
}

// Return expression
ReturnExpression::ReturnExpression(Node* expression) : value(expression),_resolved(false) {}
bool ReturnExpression::isResolved() const {
	return _resolved;
}
Node* ReturnExpression::duplicate(DuplicationModifiers* mods) const {
	if(mods->returnValueRedirector){
		auto assign = new AssignmentExpression(new VariableReference(mods->returnValueRedirector),value->duplicate(mods));
		assign->isInitializingAssignment = true;//NB when mixing in we assign the return to a immutable value
		return copyProperties(assign);
	}
	auto r = new ReturnExpression(value->duplicate(mods));
	r->_resolved = _resolved;
	return copyProperties(r);
}

//Pointer operation
PointerOperation::PointerOperation(Node* expression,int type){
	this->expression = expression;
	kind = type;
}
TypeExpression* PointerOperation::_returnType() const {
	assert(expression->isResolved());
	auto next = expression->_returnType();//TODO
	assert(next->type != TypeExpression::INTRINSIC);
	if(kind == ADDRESS){
		auto x = new TypeExpression((PointerType*)nullptr,next);
		x->_localSemantics = expression->isLocal();
		return x;
	}
	else if(kind == DEREFERENCE && next->type == TypeExpression::POINTER){
		return next->argument;
	}
	return intrinsics::types::Void;
}
bool PointerOperation::isResolved() const {
	return expression->isResolved();
}
Node* PointerOperation::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new PointerOperation(expression->duplicate(mods),kind));
}

// Match expression
MatchExpression::MatchExpression(Node* object){
	this->object = object;
}
TypeExpression* MatchExpression::_returnType() const {
	return intrinsics::types::Void;//TODO
}
bool MatchExpression::isResolved() const {
	return false;
}
Node* MatchExpression::duplicate(DuplicationModifiers* mods) const{
	auto dup = new MatchExpression(object->duplicate(mods));
	dup->cases.reserve(cases.size());
	for(auto i = cases.begin();i!=cases.end();i++){
		dup->cases.push_back(Case((*i).pattern->duplicate(mods),(*i).consequence ? (*i).consequence->duplicate(mods) : nullptr,(*i).fallThrough));
	}
	return copyProperties(dup);
}

// Function reference
FunctionReference::FunctionReference(Function* func) : function(func) {
	if(!func->intrinsicEvaluator) assert(!func->_hasGenericArguments );
}
TypeExpression* FunctionReference::_returnType() const {
	assert(isResolved());
	return new TypeExpression(function->argumentType(),function->returnType());
}
bool FunctionReference::isResolved() const {
	return function->isResolved();//TODO kinds pointless, since function refrences are only obtained from resolved functions?
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
	auto type = object->_returnType();
	if(type->type == TypeExpression::RECORD) return type->record;
	else if(type->type == TypeExpression::POINTER && type->argument->type == TypeExpression::RECORD) return type->argument->record;
	return nullptr;
}
bool FieldAccessExpression::isLocal() const {
	return object->isLocal(); //for when using (local var x Foo).field
}
TypeExpression* FieldAccessExpression::_returnType() const {
	return objectsRecord()->fields[field].type.type();
}
Node* FieldAccessExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FieldAccessExpression(object->duplicate(mods),field));
}

// Call expression

CallExpression::CallExpression(Node* object,Node* argument){
	this->object = object;
	this->arg = argument;
	_resolved = false;
}

TypeExpression* CallExpression::_returnType() const {
	assert(isResolved());
	if( auto refFunc = object->asFunctionReference()){
		return refFunc->function->_returnType.isResolved() ? refFunc->function->_returnType.type() : intrinsics::types::Void;//TODO fix when function has unresolved return type!
	}
	return intrinsics::types::Void;//TODO only allow functions?
}
bool CallExpression::isResolved() const {
	return _resolved;
}
Node* CallExpression::duplicate(DuplicationModifiers* mods) const {
	auto e = new CallExpression(object->duplicate(mods),arg->duplicate(mods));
	e->_resolved = _resolved;
	return copyProperties(e);
}

//While expression
WhileExpression::WhileExpression(Node* condition,Node* body){
	this->condition = condition;
	this->body = body;
}
bool WhileExpression::isResolved() const {
	return condition->isResolved() && body->isResolved();
}
Node* WhileExpression::duplicate(DuplicationModifiers* mods) const{
	return copyProperties(new WhileExpression(condition->duplicate(mods),body->duplicate(mods)));
}

// Block expression
BlockExpression::BlockExpression(Scope* scope){
	this->scope = scope;
	_resolved = false;
}
void BlockExpression::_duplicate(BlockExpression* dest,DuplicationModifiers* mods) const {
	dest->children.reserve(children.size());//Single alocation
	for(auto i=children.begin();i!=children.end();i++) dest->children.push_back((*i)->duplicate(mods));
	copyProperties(dest);
}
Node* BlockExpression::duplicate(DuplicationModifiers* mods) const {
	auto scp = mods->target;
	mods->target = new Scope(scp);
	scope->duplicate(mods);
	auto dup = new BlockExpression(mods->target);
	_duplicate(dup,mods);
	mods->target = scp;
	return dup;
}
bool BlockExpression::isResolved() const {
	return _resolved;
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


//

TypeExpression* InferredUnresolvedTypeExpression::type(){
	assert(kind == Type);
	return  _type;
}
InferredUnresolvedTypeExpression InferredUnresolvedTypeExpression::duplicate(DuplicationModifiers* mods){
	InferredUnresolvedTypeExpression result;
	if(kind == Type) result._type = _type->duplicate(mods)->asTypeExpression();
	else if(kind == Unresolved) result.unresolvedExpression = unresolvedExpression->duplicate(mods);
	result.kind = kind;
	return result;
}
void InferredUnresolvedTypeExpression::infer(TypeExpression* type){
	assert(kind == Inferred);
	assert(type->isResolved());
	kind = Type;
	DuplicationModifiers mods;
	_type = type->duplicate(&mods)->asTypeExpression();
}

// TypeExpression
TypeExpression::TypeExpression(IntrinsicType* intrinsic) : type(INTRINSIC),_localSemantics(false) {
	this->intrinsic = intrinsic;
}
TypeExpression::TypeExpression(IntegerType* integer) : type(INTEGER),_localSemantics(false) {
	this->integer = integer;
}
TypeExpression::TypeExpression(Record* record): type(RECORD),_localSemantics(false) { 
	this->record = record; 
}
TypeExpression::TypeExpression(PointerType* pointer,TypeExpression* next) : type(POINTER),_localSemantics(false) {
	this->argument = next;
}
TypeExpression::TypeExpression(TypeExpression* argument,TypeExpression* returns) : type(FUNCTION),_localSemantics(false) {
	this->argument = argument;
	this->returns = returns;
}
bool TypeExpression::isConst() const{
	return true;
}
bool TypeExpression::isResolved() const {
	switch(type){
		case RECORD: return record->isResolved();
		case POINTER: return argument->isResolved();
		case FUNCTION: return argument->isResolved() && returns->isResolved();
	}
	return true;
}
TypeExpression* TypeExpression::_returnType() const {
	assert(isResolved());
	return intrinsics::types::Type;
}
Node* TypeExpression::duplicate(DuplicationModifiers* mods) const {
	TypeExpression* x;
	switch(type){
		case RECORD: return record->reference();
		case INTEGER: return integer->reference();
		case INTRINSIC: return intrinsic->reference();
		case POINTER:
			x = new TypeExpression((PointerType*)nullptr,argument->duplicate(mods)->asTypeExpression());
			x->_localSemantics = _localSemantics;
			return x;
		case FUNCTION:
			x = new TypeExpression(argument->duplicate(mods)->asTypeExpression(),returns->duplicate(mods)->asTypeExpression());
			x->_localSemantics = _localSemantics;
			return x;
		default:
			throw std::runtime_error("TypeExpression type invariant failed");
			return nullptr;
	}
}

//TODO non references
size_t TypeExpression::size() const {
	switch(type){
		case RECORD: return record->size();
		case INTEGER: return integer->size();
		case INTRINSIC: return intrinsic->size();
		case POINTER: return compiler::pointerSize;
		case FUNCTION: return compiler::pointerSize;
		default:
			throw std::runtime_error("TypeExpression type invariant failed");
	}
}
bool TypeExpression::isSame(TypeExpression* other){
	if(this->type != other->type) return false;
	if(this->_localSemantics != other->_localSemantics) return false;
	switch(type){
		case RECORD: return record == other->record;
		case INTEGER: return integer == other->integer;
		case POINTER: return argument->isSame(other->argument);
		case INTRINSIC: return intrinsic == other->intrinsic;
		case FUNCTION: return argument->isSame(other->argument) && returns->isSame(other->returns);
		default:
			throw std::runtime_error("TypeExpression type invariant failed");	
			return false;
	}
}
enum {
	LITERAL_CONVERSION = 3,
	RECORD_SUBTYPE,
	EXACT
};
int TypeExpression::canAssignFrom(Node* expression,TypeExpression* type){
	if(this->isSame(type)) return EXACT;

	else if(this->type == INTEGER && type->type == INTEGER){
		//literal integer constants.. check to see if the type can accept it's value
		if(auto intConst = expression->asIntegerLiteral()){
			if(!intConst->_type && this->integer->isValid(intConst->integer)) return LITERAL_CONVERSION;
		}
	}else if(type->type == RECORD){
		//Extenders fields
		for(size_t i = 0;i < type->record->fields.size();i++){
			Record::Field* field = &type->record->fields[i];
			if(field->isExtending && field->type.isResolved()){
				auto dummyFieldAcess = new FieldAccessExpression(expression,i);
				if(this->canAssignFrom(dummyFieldAcess,field->type.type()) != -1){
					delete dummyFieldAcess;
					return RECORD_SUBTYPE;
				}
				delete dummyFieldAcess;
			}
		}
	}
	else if(this->type == POINTER){
		if(type->type == POINTER){		
			//Extender records on pointers to records
			if(type->argument->type == RECORD){
				auto dummyDeref = new PointerOperation(expression,PointerOperation::DEREFERENCE);
				if(this->argument->canAssignFrom(dummyDeref,type->argument) != -1){
					delete dummyDeref;
					return RECORD_SUBTYPE;
				}
				delete dummyDeref;
			}
		}
	}
	return -1;
}
Node* TypeExpression::assignableFrom(Node* expression,TypeExpression* type) {
	if(this->isSame(type)) return expression;//like a baws

	else if(this->type == INTEGER && type->type == INTEGER){
		//literal integer constants.. check to see if the type can accept it's value
		if(auto intConst = expression->asIntegerLiteral()){
			if(!intConst->_type && this->integer->isValid(intConst->integer)) return expression;
		}
	}else if(type->type == RECORD){
		//Extenders fields
		for(size_t i = 0;i < type->record->fields.size();i++){
			Record::Field* field = &type->record->fields[i];
			if(field->isExtending && field->type.isResolved()){
				auto dummyFieldAcess = new FieldAccessExpression(expression,i);
				if(auto assigns = this->assignableFrom(dummyFieldAcess,field->type.type())) return assigns;
				delete dummyFieldAcess;
			}
		}
	}else if(this->type == POINTER){
		if(type->type == POINTER){
			//TODO local semantics interaction
			
			//Extender records on pointers to records
			if(type->argument->type == RECORD){
				auto dummyDeref = new PointerOperation(expression,PointerOperation::DEREFERENCE);
				if(auto assigns = this->argument->assignableFrom(dummyDeref,type->argument)){
					debug("YES for pointer exetnder records!");
					return new PointerOperation(assigns,PointerOperation::ADDRESS);
				}
				delete dummyDeref;
			}
		}
	}

	return nullptr;
}
std::ostream& operator<< (std::ostream& stream,TypeExpression* node){
	if(node->hasLocalSemantics()) stream<<"local ";
	switch(node->type){
		case TypeExpression::RECORD: stream<<node->record; break;
		case TypeExpression::INTEGER: stream<<node->integer->id; break;
		case TypeExpression::INTRINSIC: stream<<node->intrinsic->id; break;
		case TypeExpression::POINTER: 
			stream<<"Pointer("<<node->argument<<')'; break;
		case TypeExpression::FUNCTION: 
			stream<<"FuncType("<<node->argument<<','<<node->returns<<')'; break;
	}
	return stream;
}

//Other,temporary nodes

ExpressionVerifier::ExpressionVerifier(const Location& loc,Node* child,TypeExpression* typeExpected) : expression(child),expectedType(typeExpected) {
	assert(child);assert(typeExpected);
	location = loc;
}
Node* ExpressionVerifier::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new ExpressionVerifier(location,expression->duplicate(mods),expectedType/* NB no duplicate */));
}
std::ostream& operator<< (std::ostream& stream,ExpressionVerifier* node){
	stream<<node->expression<<" with expected type "<<node->expectedType;
	return stream;
}

UnresolvedSymbol::UnresolvedSymbol(const Location& loc,SymbolID sym,Scope* scope) : symbol(sym),explicitLookupScope(scope) {
	assert(!sym.isNull());
	location = loc;
}
Node* UnresolvedSymbol::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnresolvedSymbol(location,symbol,explicitLookupScope));
}
std::ostream& operator<< (std::ostream& stream,UnresolvedSymbol* node){
	stream<<node->symbol;
	return stream;
}

AccessExpression::AccessExpression(Node* object,SymbolID symbol){
	this->object = object;
	this->symbol = symbol;
	this->passedFirstEval = false;
}
Node* AccessExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new AccessExpression(object->duplicate(mods),symbol));//NB duplicate passed first eval? - no
}
std::ostream& operator<< (std::ostream& stream,AccessExpression* node){
	stream<<node->object<<" u. "<<node->symbol;
	return stream;
}

ErrorExpression* errorInstance = nullptr;
Node* ErrorExpression::duplicate(DuplicationModifiers* mods) const {
	return errorInstance;//NB an instance is already created, so we dont have to use getInstance
};
ErrorExpression* ErrorExpression::getInstance() {
	if(errorInstance) return errorInstance;
	else return errorInstance = new ErrorExpression;
}

/**
* Node tracer
*/

struct NodeToString: NodeVisitor {
	std::ostream& stream;
	NodeToString(std::ostream& ostream) : stream(ostream) {}

	Node* visit(UnresolvedSymbol* node){
		stream<<node;
		return node;
	}
	Node* visit(ExpressionVerifier* node){
		stream<<node;
		return node;
	}
	Node* visit(AccessExpression* node){
		stream<<node;
		return node;
	}
	Node* visit(IntegerLiteral* node){
		stream<<node->integer;
		return node;
	}
	Node* visit(ErrorExpression* node){
		stream<<"error";
		return node;
	}
	Node* visit(StringLiteral* node){
		stream<<'"';
		for(size_t i = 0;i<node->block.length();i++){
			stream<<node->block[i];
		}
		stream<<'"';
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
		stream<<(node->variable->isLocal()?"local ":"")<<"variable "<<node->variable->id;
		return node;
	}
	Node* visit(FunctionReference* node){
		stream<<"ref func "<<node->function->id;
		return node;
	}

	Node* visit(CallExpression* node){
		stream<<"call "<<node->object<<" with "<<node->arg;
		return node;
	}
	Node* visit(FieldAccessExpression* node){
		stream<<node->object<<" f. "<<node->objectsRecord()->fields[node->field].name;
		return node;
	}

	Node* visit(AssignmentExpression* node){
		stream<<node->object<<" = "<<node->value;
		return node;
	}
	Node* visit(ReturnExpression* node){
		stream<<"return"<<' '<<node->value;
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
	if(!node->label().isNull()) stream<<node->label()<<':';
	NodeToString visitor(stream);
	node->accept(&visitor);
	stream<<')';
	stream<<"::";
	if(node->isResolved()) stream<<node->_returnType();
	else stream<<"unresolved";
	return stream;
}
