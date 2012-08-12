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
	if(redefine && !original->label().isNull()) target->define(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(Function* original,Function* duplicate){
	if(redefine && !original->label().isNull()) target->defineFunction(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(TypeDeclaration* original,TypeDeclaration* duplicate){
	if(redefine) target->define(duplicate);
	redirectors[reinterpret_cast<void*>(original)] = std::make_pair(reinterpret_cast<void*>(duplicate),false);
}
void DuplicationModifiers::duplicateDefinition(PrefixMacro* original,PrefixMacro* duplicate){
	if(redefine) target->define(duplicate);
}
void DuplicationModifiers::duplicateDefinition(InfixMacro* original,InfixMacro* duplicate){
	if(redefine) target->define(duplicate);
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
	return isFlagSet(IS_PRIVATE) ? data::ast::PRIVATE : data::ast::PUBLIC;
}
bool  DefinitionNode::isPublic() const {
	return !isFlagSet(IS_PRIVATE);
}
void DefinitionNode::visibilityMode(uint8 mode){
	if(mode == data::ast::PRIVATE) setFlag(IS_PRIVATE);
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
IntegerLiteral::IntegerLiteral(const BigInt& integer,Type* t){
	this->integer = integer;
	explicitType = t? t : Type::getBestFitIntegerType(integer);
	if(t) setFlag(LiteralNode::EXPLICIT_TYPE);
}
Type* IntegerLiteral::returnType() const{
	return explicitType;
}
Node* IntegerLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new IntegerLiteral(integer,explicitType));
}

FloatingPointLiteral::FloatingPointLiteral(const double v,Type* t): value(v) {
	explicitType = t? t : Type::getFloatType(64);
	if(t) setFlag(LiteralNode::EXPLICIT_TYPE);
}
Type* FloatingPointLiteral::returnType() const {
	return explicitType;
}
Node* FloatingPointLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FloatingPointLiteral(value,explicitType));
}

CharacterLiteral::CharacterLiteral(UnicodeChar c,Type* t) : value(c) {
	explicitType = t? t : Type::getCharType(32);
	if(t) setFlag(LiteralNode::EXPLICIT_TYPE);
}
Type* CharacterLiteral::returnType() const {
	return explicitType;
}
Node* CharacterLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new CharacterLiteral(value,explicitType));
}

BoolExpression::BoolExpression(const bool v) : value(v) { 
	setFlag(RESOLVED | CONSTANT);
}
Type* BoolExpression::returnType() const {
	return intrinsics::types::boolean;
}
Node* BoolExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new BoolExpression(value));
}

StringLiteral::StringLiteral(memory::Block& block,Type* t) {
	explicitType = t? t : Type::getLinearSequence(Type::getCharType(8));
	if(t) setFlag(LiteralNode::EXPLICIT_TYPE);
	this->block.aquire(block);
}

StringLiteral::StringLiteral(SymbolID symbol){
	if(!symbol.isNull())
		block = memory::Block::construct(symbol.ptr(),symbol.length());
	else block = memory::Block::construct("",0);
	explicitType = Type::getLinearSequence(Type::getCharType(8));
}
Type* StringLiteral::returnType() const{
	return explicitType;
}
Node* StringLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new StringLiteral(block.duplicate(),explicitType));
}

ArrayExpression::ArrayExpression() :explicitType(nullptr) {}
Type* ArrayExpression::returnType() const {
	return explicitType? explicitType : intrinsics::types::Void;//TODO
}
Node* ArrayExpression::duplicate(DuplicationModifiers* mods) const {
	auto dup = new ArrayExpression;
	dup->explicitType = explicitType;
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
	setFlag(CONSTANT | RESOLVED);
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

ThrowExpression::ThrowExpression(Node* node) : expression(node) {}
Node* ThrowExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new ThrowExpression(expression->duplicate(mods)));
}


//Pointer operation
PointerOperation::PointerOperation(Node* expression,int type){
	this->expression = expression;
	kind = type;
}
Type* PointerOperation::returnType() const {
	assert(isResolved());
	auto next = expression->returnType();
	if(isAddress()) return isFlagSet(ADDRESS_RETURNS_REF)? Type::getReferenceType(next) : Type::getPointerType(next);
	assert(next->isPointer() || next->isReference());
	return next->argument;
}
Node* PointerOperation::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new PointerOperation(expression->duplicate(mods),kind));
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
	if(function->isFlagSet(Function::HAS_EXPENDABLE_ARGUMENTS) || function->isFlagSet(Function::HAS_PATTERN_ARGUMENTS)) return intrinsics::types::Void;
	assert(isResolved());
	if(function->isIntrinsic()) return intrinsics::types::Void;//NB: hack for function reference totext printing
	else return FunctionPointer::get(function->argumentType(),function->returns(),function->callingConvention());
}
Node* FunctionReference::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new FunctionReference(function));
}

// Field access expression
FieldAccessExpression::FieldAccessExpression(Node* object,int field){
	this->object = object;
	this->field = field;
	assert(fieldsType());
}
Type*    FieldAccessExpression::fieldsType() const{
	auto type = object->returnType();
	if(type->isPointer()) type = type->next();
	if(auto record = type->asRecord()) return record->fields[field].type.type();
	else if(auto rec = type->asAnonymousRecord()) return rec->types[field];
	assert(false);
	return nullptr;
}
SymbolID FieldAccessExpression::fieldsName() const{
	auto type = object->returnType();
	if(type->isPointer()) type = type->next();
	if(auto record = type->asRecord()) return record->fields[field].name;
	else if(auto rec = type->asAnonymousRecord()) return rec->fields? rec->fields[field] : SymbolID();
	assert(false);
	return nullptr;
}
Type* FieldAccessExpression::returnType() const {
	return fieldsType();
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
		auto func = refFunc->function;
		if(func->isIntrinsicReturningPattern()){

			Type* firstArg;

			if(auto tuple = arg->asTupleExpression()){
				firstArg = (*tuple->begin())->returnType()->stripQualifiers();
			}
			else firstArg = arg->returnType()->stripQualifiers();
			if(firstArg->isPointer()) firstArg = firstArg->next()->stripQualifiers();

			return Function::getIntrinsicOperationReturnType(firstArg,func->getOperation());
			//if(op == data::ast::Operations::ELEMENT_GET){
			//	return Type::getReferenceType(firstArg->next()->next());
			//}
		}
		else return func->_returnType.type();
	}
	else if( auto vref = object->asVariableReference()){
		return vref->variable->type.type();//TODO function pointer type returns
	}

	assert(false && "Invalid call expression - can't calculate the return type");
	return intrinsics::types::Void;
}
Node* CallExpression::duplicate(DuplicationModifiers* mods) const {
	auto e = new CallExpression(object->duplicate(mods),arg->duplicate(mods));
	return copyProperties(e);
}

LogicalOperation::LogicalOperation(Node* x,Node* y,bool isOr){
	parameters[0] =x;parameters[1] = y;
	if(isOr) setFlag(IS_OR);
}
LogicalOperation::LogicalOperation(Node* x,Node* y){
	parameters[0] =x;parameters[1] = y;
}
Type* LogicalOperation::returnType() const { return intrinsics::types::boolean; }
Node* LogicalOperation::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new LogicalOperation(parameters[0]->duplicate(mods),parameters[1]->duplicate(mods)));
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
	this->parentNode = nullptr;
}
BlockExpression::BlockExpression(Scope* scope){
	this->scope        = scope;
}
Type *BlockExpression::returnType() const {
	if(isFlagSet(RETURNS_LAST_EXPRESSION) && size()>0) return (*(end()-1))->returnType();
	return intrinsics::types::Void;
}
void BlockExpression::addChildPotentiallyDisturbingIteration(Node* child){
	if(isFlagSet(ITERATING)){
		setFlag(ITERATION_MODIFIED_CHILDREN);
	}
	addChild(child);
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

ScopedCommand::ScopedCommand(data::ast::VisibilityMode mode,Node* expression){
	setFlag(mode == data::ast::PRIVATE? PRIVATE:PUBLIC);
	child = expression;
}
ScopedCommand::ScopedCommand(){
	setFlag(WHERE);
	child = nullptr;
}
Node* ScopedCommand::duplicate(DuplicationModifiers* mods) const {
	ScopedCommand* dup = new ScopedCommand();
	if(isFlagSet(WHERE)) {
		dup->parameters.reserve(parameters.size());
		for(auto i = parameters.begin(); i!=parameters.end(); ++i)
			dup->parameters.push_back(std::make_pair(i->first,i->second->duplicate(mods)));
	};

	if(child) dup->child = child->duplicate(mods);
	return copyProperties(dup);
}


NodeReference::NodeReference(Node* node) : _node(node) {
	setFlag(CONSTANT | RESOLVED);
}
Type* NodeReference::returnType() const { 
	return intrinsics::types::NodePointer; 
}
Node* NodeReference::duplicate(DuplicationModifiers* mods) const {
	auto old = mods->redefine;
	mods->redefine = false;
	auto expr = copyProperties(new NodeReference(_node->duplicate(mods)));
	mods->redefine = old;
	return expr;
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

TraitParameterReference::TraitParameterReference(size_t i){
	this->index = i;
}
Node* TraitParameterReference::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new TraitParameterReference(index));
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

//Node comparison

bool  IntegerLiteral::isSame(Node* other){
	if(auto node = other->asIntegerLiteral()) return integer == node->integer;
	return false;
}
bool  FloatingPointLiteral::isSame(Node* other){
	if(auto node = other->asFloatingPointLiteral()) return value == node->value;
	return false;
}
bool  CharacterLiteral::isSame(Node* other){
	if(auto node = other->asCharacterLiteral()) return value == node->value;
	return false;
}
bool  BoolExpression::isSame(Node* other){
	if(auto node = other->asBoolExpression()) return value == node->value;
	return false;
}
bool  StringLiteral::isSame(Node* other){
	if(auto node = other->asStringLiteral()) return strcmp(block.ptr(),node->block.ptr()) == 0;
	return false;
}
bool  UnitExpression::isSame(Node* other){
	return other->asUnitExpression()? true : false;
}
bool  TypeReference::isSame(Node* other){
	if(auto node = other->asTypeReference()) return type->isSame(node->type);
	return false;
}
bool  ScopedCommand::isSame(Node* other){
	if(auto node = other->asScopedCommand()){
		if(isFlagSet(WHERE)) return false;
		return isFlagSet(PRIVATE) == other->isFlagSet(PRIVATE) || isFlagSet(PUBLIC) == other->isFlagSet(PUBLIC);
	}
	return false;
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

