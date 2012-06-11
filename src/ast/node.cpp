#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../compiler.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "interpret.h"
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
	if(_type) return new TypeExpression(_type);
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

BoolExpression::BoolExpression(const bool v) : value(v) {}
TypeExpression* BoolExpression::_returnType() const {
	return intrinsics::types::boolean;
}
Node* BoolExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new BoolExpression(value));
}

StringLiteral::StringLiteral(memory::Block& block){
	this->block.aquire(block);
}
#include <cstring>
StringLiteral::StringLiteral(SymbolID symbol){
	if(!symbol.isNull())
		block.construct(symbol.ptr(),strlen(symbol.ptr()));
	else block.construct("",0);
}
TypeExpression* StringLiteral::_returnType() const{
	return intrinsics::types::StringLiteral->reference();//TODO
}
Node* StringLiteral::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new StringLiteral(block.duplicate()));
}



// Unit expression
TypeExpression* UnitExpression::_returnType() const {
	return intrinsics::types::Void;
}
bool UnitExpression::isConst() const {
	return true;
}
Node* UnitExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new UnitExpression);
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
	return !variable->isFlagSet(Variable::HIDDEN_TYPE) ? variable->type.type() : variable->hiddenType();
}
Node* VariableReference::duplicate(DuplicationModifiers* mods) const {
	if(mods->expandedMacroOptimization){
		if(auto value = mods->expandedMacroOptimization->getValue(variable)){
			if(auto v = value->asValueExpression()){
				if(v->type->isSame(intrinsics::ast::ExprPtr)){
					//Simplify blocks
					auto node = reinterpret_cast<Node*>(v->data);
					if(auto block = node->asBlockExpression()){
						if(block->scope->numberOfDefinitions() == 0 && block->children.size() == 1) node = block->children[0];
					}
					return node->duplicate(mods);
				}
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

ControlFlowExpression::ControlFlowExpression(int type) : kind(type) {
}
Node* ControlFlowExpression::duplicate(DuplicationModifiers* mods) const {
	return copyProperties(new ControlFlowExpression(kind));
}

//Pointer operation
PointerOperation::PointerOperation(Node* expression,int type){
	this->expression = expression;
	kind = type;
	_resolved = false;
}
TypeExpression* PointerOperation::_returnType() const {
	assert(_resolved);
	auto next = expression->_returnType();
	assert(next->type != TypeExpression::INTRINSIC);
	if(kind == ADDRESS){
		auto x = new TypeExpression(TypeExpression::POINTER,next);
		x->_localSemantics = expression->isLocal();
		return x;
	}
	assert(next->type == TypeExpression::POINTER);
	return next->argument;
}
bool PointerOperation::isResolved() const {
	return _resolved;
}
Node* PointerOperation::duplicate(DuplicationModifiers* mods) const {
	auto result = new PointerOperation(expression->duplicate(mods),kind);
	result->_resolved = _resolved;
	return result;
}

IfExpression::IfExpression(Node* condition,Node* consequence,Node* alternative){
	this->condition = condition;
	this->consequence = consequence;
	this->alternative = alternative;
	_resolved = false;
}
TypeExpression* IfExpression::_returnType() const {
	auto cr = consequence->_returnType();
	if(cr->isSame(alternative->_returnType())) return cr;
	return intrinsics::types::Void;
}
bool IfExpression::isResolved() const {
	return _resolved;
}
Node* IfExpression::duplicate(DuplicationModifiers* mods) const{
	auto x = new IfExpression(condition->duplicate(mods),consequence->duplicate(mods),alternative->duplicate(mods));
	x->_resolved = _resolved;
	return copyProperties(x);
}

// Function reference
FunctionReference::FunctionReference(Function* func) : function(func) {
	if(!func->constInterpreter) assert(!func->_hasGenericArguments );
}
TypeExpression* FunctionReference::_returnType() const {
	assert(isResolved());
	return new TypeExpression(function->argumentType(),function->returnType());
}
bool FunctionReference::isResolved() const {
	return function->isResolved();//TODO kinds pointless, since function refrences are only obtained from resolved functions?
}
bool FunctionReference::isConst() const {
	return true;
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

//Loop expression
LoopExpression::LoopExpression(Node* body){
	this->body = body;
}
bool LoopExpression::isResolved() const {
	return body->isResolved();
}
Node* LoopExpression::duplicate(DuplicationModifiers* mods) const{
	return copyProperties(new LoopExpression(body->duplicate(mods)));
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

TypeExpression* TypePatternUnresolvedExpression::type() const {
	assert(kind == Type);
	return  _type;
}
TypePatternUnresolvedExpression TypePatternUnresolvedExpression::duplicate(DuplicationModifiers* mods) const {
	TypePatternUnresolvedExpression result;
	if(kind == Type) result._type = _type->duplicate(mods)->asTypeExpression();
	else if(kind == Unresolved) result.unresolvedExpression = unresolvedExpression->duplicate(mods);
	else if(kind == Pattern) result.pattern = pattern ? pattern->duplicate(mods) : nullptr;
	result.kind = kind;
	return result;
}
void TypePatternUnresolvedExpression::specify(TypeExpression* givenType){
	assert(kind == Pattern && pattern == nullptr);
	assert(givenType->isResolved());
	kind = Type;
	_type = givenType;
}
bool TypePatternUnresolvedExpression::deduce(TypeExpression* givenType,Scope* container){
	assert(isPattern());
	assert(givenType->isResolved());

	if(pattern){
		PatternMatcher matcher(container);
		if(!matcher.match(givenType,pattern)) return false;
	}
	kind  = Type;
	DuplicationModifiers mods;
	_type = givenType->duplicate(&mods)->asTypeExpression();//NB: type expresssion is duplicated
	return true;
}
TypePatternUnresolvedExpression::PatternMatcher::IntroducedDefinition* TypePatternUnresolvedExpression::PatternMatcher::lookupDefinition(SymbolID name){
		for(auto i = introducedDefinitions.begin();i!=introducedDefinitions.end();i++){
			if(name == (*i).name) return &(*i);
		}
		return nullptr;
}
void TypePatternUnresolvedExpression::PatternMatcher::introduceDefinition(SymbolID name,Location location,Node* value){
	for(auto i = introducedDefinitions.begin();i!=introducedDefinitions.end();i++){
		if(name == (*i).name) error(location,"Multiple type labels with same name %s exist in this type pattern",name);
	}
	introducedDefinitions.push_back(IntroducedDefinition(name,location,value));
}
bool TypePatternUnresolvedExpression::PatternMatcher::check(Node* expression){
	if(expression->asTypeExpression()){ //| int32
		return true;
	}else if(auto unresolved = expression->asUnresolvedSymbol()){
		auto symbol = unresolved->symbol;
		if(symbol == "_"){
			if(!unresolved->label().isNull()) introduceDefinition(unresolved->label(),unresolved->location);
			return true; //|_ | T:_
		}
		//T
		if(lookupDefinition(symbol)) return true;
	} else if(auto ref = expression->asFunctionReference()){
		//Constraint
		if(ref->function->isFlagSet(Function::CONSTRAINT_FUNCTION) && ref->function->isResolved()) return true;
		if(!ref->label().isNull()) introduceDefinition(ref->label(),ref->location);
	} else if(auto call = expression->asCallExpression()){
		//| Pointer(_) i.e. a Type generated by a function
		bool resolvedObject = false;
		if(auto callingUnresolvedFunction = call->object->asUnresolvedSymbol()){
			auto def = container->lookupPrefix(callingUnresolvedFunction->symbol);
			Overloadset* os = def ? def->asOverloadset() : nullptr;
			if(os && os->isFlagSet(Overloadset::TYPE_GENERATOR_SET)) resolvedObject = true;//TODO deep search?
		} else if(auto ref = call->object->asFunctionReference()){
			if(ref->function->isFlagSet(Function::CONSTRAINT_FUNCTION) && ref->function->isResolved()) resolvedObject = true;
		}
		if(resolvedObject){
			if(!call->label().isNull()) introduceDefinition(call->label(),call->location);
			//Check the parameters..
			if(auto argTuple = call->arg->asTupleExpression()){
				for(auto i = argTuple->children.begin();i!=argTuple->children.end();i++){ if(!check(*i)) return false; }
				return true;
			}
			else return check(call->arg);
		}
	} else if(auto var = expression->asVariableReference()){
		if(lookupDefinition(var->variable->id)) return true;
	}
	return false;
}
//Evaluates the verifier to see if an expression satisfies a constraint
bool satisfiesConstraint(Node* arg,Function* constraint){
	assert(constraint->arguments.size() == 1);
	
	InterpreterInvocation i(compiler::currentUnit()->interpreter,constraint,arg);
	if(i.succeded()){
		if(auto resolved = i.result()->asBoolExpression()){
			return resolved->value;
		}
	}
	error(arg->location,"Can't evaluate constraint %s with argument %s at compile time!",constraint->id,arg);
	return false;
}
bool TypePatternUnresolvedExpression::PatternMatcher::match(Node* object,Node* pattern){
	TypeExpression* type = object->asTypeExpression();
	//Match(non-type) TODO
	if(!type){
		//match non type
		assert(false);
		return false;
	}
	//Match(type)
	if(auto type2 = pattern->asTypeExpression()) return type->isSame(type2); //| int32
	else if(auto unresolved = pattern->asUnresolvedSymbol()){
		auto symbol = unresolved->symbol;
		if(symbol == "_"){
			if(!unresolved->label().isNull()) introduceDefinition(unresolved->label(),unresolved->location,type);
			return true; //|_ | T:_
		}
		//T
		if(auto def = lookupDefinition(symbol)){
			if(auto vt = def->value->asTypeExpression())
				return type->isSame(vt);
			else return false;
		}
	} else if(auto ref = pattern->asFunctionReference()){
		//Constraint (We can safely assume this is a constraint if check passed)
		if(satisfiesConstraint(type,ref->function)){
			if(!ref->label().isNull()) introduceDefinition(ref->label(),ref->location,type);
			return true;
		}
	} else if(auto call = pattern->asCallExpression()){
		if(!type->wasGenerated()) return false;
		bool matchedObject = false;
		//| Pointer(_)
		if(auto callingUnresolvedFunction = call->object->asUnresolvedSymbol()){
			auto def = container->lookupPrefix(callingUnresolvedFunction->symbol);
			Overloadset* os = def ? def->asOverloadset() : nullptr;
			if(os && os->isFlagSet(Overloadset::TYPE_GENERATOR_SET)){//TODO better search?
				for(auto i = os->functions.begin();i!=os->functions.end();i++){
					if(type->wasGeneratedBy(*i)) matchedObject = true;
				}
			}
		} else if(auto ref = call->object->asFunctionReference()){
			//Constraint (We can safely assume this is a constraint if check passed)
			if(satisfiesConstraint(type,ref->function)) matchedObject = true;
		}
		if(matchedObject){
			//Match parameters..
			if(!call->label().isNull()) introduceDefinition(call->label(),call->location,type);
			if(auto argTuple = call->arg->asTupleExpression()){
				size_t j = 0;
				for(auto i = argTuple->children.begin();i!=argTuple->children.end();i++,j++){ if(!match(type->generatedArgument(j),*i)) return false; }
				return true;
			}
			else return match(type->generatedArgument(0),call->arg);
		}
	} else if(auto var = pattern->asVariableReference()){
		if(auto def = lookupDefinition(var->variable->id)){
			if(auto vt = def->value->asTypeExpression())
					return type->isSame(vt);
				else return false;
		}
	}
	return false;
}
void TypePatternUnresolvedExpression::PatternMatcher::defineIntroducedDefinitions(){
	//TODO check if the scope already contains them..
	for(auto i= introducedDefinitions.begin();i!=introducedDefinitions.end();i++){
		auto var = new Variable((*i).name,(*i).location,container);
		var->isMutable = false;
		if((*i).value){
			var->specifyType((*i).value->_returnType());
			var->setImmutableValue((*i).value);
		}else{
			//TODO def f(x T:_) = T define T with no value
		}
		container->define(var);
	}
}

// TypeExpression
TypeExpression::TypeExpression(int kind) : type(kind),_localSemantics(false) {
	assert(kind == VOID || kind == TYPE || kind == BOOL);
}
TypeExpression::TypeExpression(IntrinsicType* intrinsic) : type(INTRINSIC),_localSemantics(false) {
	this->intrinsic = intrinsic;
}
TypeExpression::TypeExpression(IntegerType* integer) : type(INTEGER),_localSemantics(false) {
	this->integer = integer;
}
TypeExpression::TypeExpression(Record* record): type(RECORD),_localSemantics(false) { 
	this->record = record; 
}
TypeExpression::TypeExpression(int kind,TypeExpression* next) : type(POINTER),_localSemantics(false) {
	assert(kind == POINTER);
	this->argument = next;
}
TypeExpression::TypeExpression(TypeExpression* argument,TypeExpression* returns) : type(FUNCTION),_localSemantics(false) {
	this->argument = argument;
	this->returns = returns;
}
bool TypeExpression::isValidTypeForVariable(){
	return true;
}
bool TypeExpression::isValidTypeForArgument(){
	return true;
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
bool TypeExpression::matchRecord(Record* record) const {
	return type == RECORD && this->record == record;
}
Node* TypeExpression::duplicate(DuplicationModifiers* mods) const {
	TypeExpression* x;
	
	switch(type){
		case VOID:
		case TYPE:
		case BOOL:
			return copyProperties(new TypeExpression(type));
		case RECORD: 
			{
			auto red = mods->redirectors.find(record);
			if(red != mods->redirectors.end())
				return copyProperties(new TypeExpression(reinterpret_cast<Record*>((*red).second.first)));
			return copyProperties(new TypeExpression(record));
			}
		case INTEGER: return copyProperties(new TypeExpression(integer));
		case INTRINSIC: return intrinsic->reference();
		case POINTER:
			x = new TypeExpression(TypeExpression::POINTER,argument->duplicate(mods)->asTypeExpression());
			x->_localSemantics = _localSemantics;
			return copyProperties(x);
		case FUNCTION:
			x = new TypeExpression(argument->duplicate(mods)->asTypeExpression(),returns->duplicate(mods)->asTypeExpression());
			x->_localSemantics = _localSemantics;
			return copyProperties(x);
		default:
			throw std::runtime_error("TypeExpression type invariant failed");
			return nullptr;
	}
}

//TODO non references
size_t TypeExpression::size() const {
	switch(type){
		case VOID: case TYPE: return 0;
		case BOOL: return 1;
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
		case VOID: case TYPE: case BOOL: return type == other->type;
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
bool TypeExpression::wasGenerated() const {
	switch(type){
	case POINTER: return true;
	case FUNCTION: return true;
	case RECORD: return record->isFlagSet(Record::GENERATED);
	default: return false;
	}
}
bool TypeExpression::wasGeneratedBy(Function* function) const {
	switch(type){
	case POINTER: return function == intrinsics::types::PointerTypeGenerator;
	case FUNCTION: return function == intrinsics::types::FunctionTypeGenerator;
	case RECORD: return record->wasGeneratedBy(function);
	default:
		return false;
	}
}
Node* TypeExpression::generatedArgument(size_t i) const {
	switch(type){
	case POINTER: return argument;
	case FUNCTION: return i == 0 ? argument : returns;
	case RECORD: return record->generatedArgument(i);
	default:
		throw std::runtime_error("TypeExpression generatedArgument failed");	
		return nullptr;
	}
}

enum {
	LITERAL_CONVERSION = 4,
	RECORD_SUBTYPE,
	EXACT
};
int TypeExpression::canAssignFrom(Node* expression,TypeExpression* type){
	if(this->isSame(type)) return EXACT;

	else if(this->type == INTEGER && type->type == INTEGER){
		//literal integer constants.. check to see if the type can accept it's value
		if(auto intConst = expression->asIntegerLiteral()){
			//literal match
			if(!intConst->_type && this->integer->isValid(intConst->integer)) return LITERAL_CONVERSION;
		}
		if(type->integer->isSubset(this->integer)) return RECORD_SUBTYPE;
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
			//Pointer to intrinsics => base
			else if(this->argument->type == INTRINSIC && type->argument->type == INTRINSIC && type->argument->intrinsic->_base == this->argument->intrinsic) return RECORD_SUBTYPE;
		}
	}
	return -1;
}
Node* TypeExpression::assignableFrom(Node* expression,TypeExpression* type) {
	if(this->isSame(type)) return expression;//like a baws

	else if(this->type == INTEGER && type->type == INTEGER){
		//literal integer constants.. check to see if the type can accept it's value
		if(auto intConst = expression->asIntegerLiteral()){
			if(!intConst->_type && this->integer->isValid(intConst->integer)){
				intConst->_type = this->integer;
				return expression;
			}
		}
		if(type->integer->isSubset(this->integer)) return expression;
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
			//Pointer to intrinsics => base
			else if(this->argument->type == INTRINSIC && type->argument->type == INTRINSIC && type->argument->intrinsic->_base == this->argument->intrinsic) return expression;
		}
	}

	return nullptr;
}
std::ostream& operator<< (std::ostream& stream,TypeExpression* node){
	if(node->hasLocalSemantics()) stream<<"local ";
	if(node->hasConstSemantics()) stream<<"const ";
	switch(node->type){
		case TypeExpression::VOID: stream<<"void"; break;
		case TypeExpression::TYPE: stream<<"type"; break;
		case TypeExpression::BOOL: stream<<"bool"; break;
		case TypeExpression::RECORD: stream<<node->record; break;
		case TypeExpression::INTEGER: stream<<node->integer->id; break;
		case TypeExpression::INTRINSIC: stream<<node->intrinsic->id; break;
		case TypeExpression::POINTER: 
			stream<<"Pointer("<<node->argument<<')'; break;
		case TypeExpression::FUNCTION: 
			stream<<"FuncType("<<node->argument<<"->"<<node->returns<<')'; break;
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

ValueExpression::ValueExpression(void* d,TypeExpression* type){
	data = d;
	this->type = type;
}
TypeExpression* ValueExpression::_returnType() const { return type; }
Node* ValueExpression::duplicate(DuplicationModifiers* mods) const {
	auto result = copyProperties(new ValueExpression(reinterpret_cast<Node*>(data)->duplicate(mods),type->duplicate(mods)->asTypeExpression()));
	return result;
}

MatchResolver::MatchResolver(Node* object){ this->object = object; }
Node* MatchResolver::duplicate(DuplicationModifiers* mods) const {
	auto dup = new MatchResolver(object->duplicate(mods));
	for(auto i = children.begin();i!=children.end();i++) dup->children.push_back((*i)->duplicate(mods));
	return copyProperties(dup);
}

/**
* Node tracer
*/

struct NodeToString: NodeVisitor {
	std::ostream& stream;
	NodeToString(std::ostream& ostream) : stream(ostream) {}

	Node* visit(ValueExpression* node){
		stream<<"A constant value";
		if(node->type->isSame(intrinsics::ast::ExprPtr)) stream<<' '<<reinterpret_cast<Node*>(node->data);
		return node;
	}
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
	Node* visit(BoolExpression* node){
		stream<<(node->value?"true":"false");
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
	Node* visit(ControlFlowExpression* node){
		stream<<(node->isContinue() ? "continue" : (node->isBreak() ? "break" : "fallthrough"));
		return node;
	}
	Node* visit(PointerOperation* node){
		stream<<(node->kind==PointerOperation::ADDRESS?'&':'*')<<node->expression;
		return node;
	}
	Node* visit(IfExpression* node){
		stream<<"if("<<node->condition<<") "<<node->consequence<<" else "<<node->alternative;
		return node;
	}
	Node* visit(TupleExpression* node){
		auto i = node->children.begin();
		if( i == node->children.end() ) return node;
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
	Node* visit(LoopExpression* node){
		stream<<"loop "<<node->body;
		return node;
	}
	Node* visit(TypeExpression* node){
		stream<<node;
		return node;
	}
};
std::ostream& operator<< (std::ostream& stream,Node* node){
	auto d = compiler::currentUnit()->printingDecorationLevel;
	if(d) stream<<'(';
	if(!node->label().isNull()) stream<<node->label()<<':';
	NodeToString visitor(stream);
	node->accept(&visitor);
	if(d) {
		stream<<')';
		stream<<"::";
		if(node->isResolved()) stream<<node->_returnType();
		else stream<<"unresolved";
	}
	return stream;
}
