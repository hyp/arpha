#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../base/system.h"
#include "../compiler.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "interpret.h"
#include "../intrinsics/types.h"


//TODO
bool  typeSatistiesTrait(Type* type,Trait* trait,Scope* lookupScope){
	return true;
}

/**
  This range iterates over all the possible meanings of a given type( direct, subtyping, implicit conversions etc)
*/
struct TypeMeaningsRange {
private:

	void getLiteralTypesConversions(Type* type);
	void getRecordSubtypes(Record* record);

	void gatherMeanings(Type* type);

	struct Match {
		Type* type;
		int   weight;
	};
	std::vector<Match> meanings;
	std::vector<Match>::iterator currMeaning;

	inline void add(Type* type,int weight){ Match m = { type,weight };meanings.push_back(m); }
public:

	TypeMeaningsRange(Type* type);

	inline bool isEmpty()       { return currMeaning == meanings.end(); }
	inline void advance()       { ++currMeaning; }
	inline Type* currentType()  { return currMeaning->type; }
	inline int   currentWeight(){ return currMeaning->weight; }
};
TypeMeaningsRange::TypeMeaningsRange(Type* type){
	gatherMeanings(type);
	currMeaning = meanings.begin();
}

Type* TypePatternUnresolvedExpression::type() const {
	assert(kind == TYPE);
	return  _type;
}
TypePatternUnresolvedExpression TypePatternUnresolvedExpression::duplicate(DuplicationModifiers* mods) const {
	TypePatternUnresolvedExpression result;
	if(kind == TYPE) result._type = _type;
	else if(kind == UNRESOLVED) result.unresolvedExpression = unresolvedExpression->duplicate(mods);
	else if(kind == PATTERN) result.pattern = pattern ? pattern->duplicate(mods) : nullptr;
	result.kind = kind;
	return result;
}
void TypePatternUnresolvedExpression::specify(Type* givenType){
	assert(kind == PATTERN && pattern == nullptr);
	assert(givenType->isResolved());
	kind = TYPE;
	_type = givenType;
}
bool TypePatternUnresolvedExpression::deduce(Type* givenType,Scope* container){
	assert(isPattern());
	assert(givenType->isResolved());

	if(pattern){
		PatternMatcher matcher(container);
		if(!matcher.match(givenType,pattern)) return false;
	}
	kind  = TYPE;
	_type = givenType;
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
	if(expression->asTypeReference()){ //| int32
		return true;
	}else if(auto unresolved = expression->asUnresolvedSymbol()){
		auto symbol = unresolved->symbol;
		if(symbol == "_"){
			if(!unresolved->label().isNull()) introduceDefinition(unresolved->label(),unresolved->location());
			return true; //|_ | T:_
		}
		//T
		if(lookupDefinition(symbol)) return true;
	} else if(auto ref = expression->asFunctionReference()){
		//Constraint
		if(ref->function->isFlagSet(Function::CONSTRAINT_FUNCTION) && ref->function->isResolved()){
			if(!ref->label().isNull()) introduceDefinition(ref->label(),ref->location());
			return true;
		}
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
			if(!call->label().isNull()) introduceDefinition(call->label(),call->location());
			//Check the parameters..
			if(auto argTuple = call->arg->asTupleExpression()){
				for(auto i = argTuple->children.begin();i!=argTuple->children.end();i++){ if(!check(*i)) return false; }
				return true;
			}
			else return check(call->arg);
		}
	} else if(auto var = expression->asVariableReference()){
		if(lookupDefinition(var->variable->label())) return true;
	}
	return false;
}
//Evaluates the verifier to see if an expression satisfies a constraint
bool satisfiesConstraint(Node* arg,Function* constraint,Scope* expansionScope){
	assert(constraint->arguments.size() == 1);
	
	CTFEinvocation i(compiler::currentUnit(),constraint);
	if(i.invoke(arg)){
		if(auto resolved = i.result()->asBoolExpression()){
			return resolved->value;
		}
	}
	error(arg,"Can't evaluate constraint %s with argument %s at compile time:\n\tCan't evaluate expression %s!",constraint->label(),arg,i.result());
	return false;
}
bool TypePatternUnresolvedExpression::PatternMatcher::match(Type* type,Node* pattern){
	auto ref = new TypeReference(type);
	return match(ref,pattern);
}
bool TypePatternUnresolvedExpression::PatternMatcher::match(Node* object,Node* pattern){
	auto typeRef = object->asTypeReference();
	auto type = typeRef ? typeRef->type : nullptr;
	//_ | T
	if(auto unresolved = pattern->asUnresolvedSymbol()){
		auto symbol = unresolved->symbol;
		if(symbol == "_"){
			if(!unresolved->label().isNull()) introduceDefinition(unresolved->label(),unresolved->location(),object);
			return true; //|_ | T:_
		}
		//T
		if(auto def = lookupDefinition(symbol)){
			if(auto vt = def->value->asTypeReference())
				return type && type->isSame(vt->type);
			else return false;
		}
	} else if(auto var = pattern->asVariableReference()){ //shadowed T
		if(auto def = lookupDefinition(var->variable->label())){
			if(auto vt = def->value->asTypeReference())
					return type && type->isSame(vt->type);
				else return false;
		}
	}

	//Match(non-type) TODO
	if(!type){
		//match non type
		assert(false);
		return false;
	}
	//Match(type)
	if(auto type2 = pattern->asTypeReference()) return type->isSame(type2->type); //| int32
	else if(auto ref = pattern->asFunctionReference()){
		//Constraint (We can safely assume this is a constraint if check passed)
		if(satisfiesConstraint(typeRef,ref->function,container)){
			if(!ref->label().isNull()) introduceDefinition(ref->label(),ref->location(),typeRef);
			return true;
		}
	} 
	else if(auto call = pattern->asCallExpression()){
		//TODO: NB: maybe check if a type has other meanings if(!type->wasGenerated()) return false;
		bool matchedObject = false;
		Type* matchedType = type;
		//| Pointer(_)
		if(auto callingUnresolvedFunction = call->object->asUnresolvedSymbol()){
			auto def = container->lookupPrefix(callingUnresolvedFunction->symbol);
			Overloadset* os = def ? def->asOverloadset() : nullptr;
			if(os && os->isFlagSet(Overloadset::TYPE_GENERATOR_SET)){//TODO better search?
				for(auto i = os->functions.begin();i!=os->functions.end();i++){
					if(type->wasGeneratedBy(*i)) matchedObject = true;
					else {
						for(TypeMeaningsRange meanings(type);!meanings.isEmpty();meanings.advance()){
							if(meanings.currentType()->wasGeneratedBy(*i)){
								matchedObject = true;
								matchedType = meanings.currentType();
								break;
							}
						}
						
					}
				}
			}
		} 
		else if(auto ref = call->object->asFunctionReference()){
			//Constraint (We can safely assume this is a constraint if check passed)
			if(satisfiesConstraint(typeRef,ref->function,container)) matchedObject = true;
		}
		if(matchedObject){
			//Match parameters..
			if(!call->label().isNull()) introduceDefinition(call->label(),call->location(),typeRef);
			if(auto argTuple = call->arg->asTupleExpression()){
				size_t j = 0;
				for(auto i = argTuple->children.begin();i!=argTuple->children.end();i++,j++){ if(!match(matchedType->generatedArgument(j),*i)) return false; }
				return true;
			}
			else return match(matchedType->generatedArgument(0),call->arg);
		}
	} 
	return false;
}

void TypePatternUnresolvedExpression::PatternMatcher::defineIntroducedDefinitions(){
	//TODO check if the scope already contains them..
	for(auto i= introducedDefinitions.begin();i!=introducedDefinitions.end();i++){
		auto var = new Variable((*i).name,(*i).location);
		var->_owner = container;
		var->setFlag(Variable::IS_IMMUTABLE);
		if((*i).value){
			var->specifyType((*i).value->returnType());
			var->setImmutableValue((*i).value);
		}else{
			//TODO def f(x T:_) = T define T with no value
		}
		container->define(var);
	}
}


/**
* The type
*/
Type::Type(int kind) : type(kind),flags(0) {
}
Type::Type(int kind,Type* next) : type(kind),flags(0) {
	assert(kind == POINTER || kind == LINEAR_SEQUENCE);
	this->argument = next;
}
Type::Type(int kind,Type* T,size_t N) : type(kind),flags(0) {
	assert(kind == STATIC_ARRAY);
	argument = T;
	this->N  = N;
}
Type::Type(int kind,int subtype) : type(kind),flags(0) {
	nodeSubtype = subtype;
}

Type* Type::getIntegerType(int bits,bool isSigned){
	return new Type(INTEGER,isSigned? -bits:bits);
}
Type* Type::getIntegerLiteralType(){
	return new Type(LITERAL_INTEGER);
}
Type* Type::getFloatType(int bits){
	return new Type(FLOAT,bits);
}
Type* Type::getFloatLiteralType(){
	return new Type(LITERAL_FLOAT);
}
Type* Type::getCharType(int bits){
	return new Type(CHAR,bits);
}
Type* Type::getCharLiteralType(){
	return new Type(LITERAL_CHAR);
}
Type* Type::getStringLiteralType(){
	return new Type(LITERAL_STRING);
}
Type* Type::getNaturalType(){
	auto t = new Type(NATURAL);
	t->bits = 32;//TODO
	return t;
}
Type* Type::getUintptrType(){
	auto t = new Type(UINTPTRT);
	t->bits = 32;//TODO
	return t;
}
Type* Type::getLinearSequence(Type* next){
	return new Type(LINEAR_SEQUENCE,next);
}

void Type::setFlag(uint16 flag){
	flags |= flag;
}
bool Type::isFlagSet(uint16 flag) const {
	return (flags & flag) == flag;
}
bool Type::requiresDestructorCall() const {
	switch(type){
		case RECORD:  return true;
	}
	return false;
}
bool Type::isResolved() const {
	switch(type){
		case RECORD:  return isFlagSet(IS_RESOLVED);
		case VARIANT: return isFlagSet(IS_RESOLVED);
		case POINTER:
		case LINEAR_SEQUENCE:
		case STATIC_ARRAY:
			return argument->isResolved();
		case FUNCTION_POINTER:
			return argument->isResolved() && static_cast<const FunctionPointer*>(this)->returns()->isResolved();
		default:
			return true;
	}
}
//Partially resolved means sizeof(T) would succeed
bool Type::isPartiallyResolved() const {
	switch(type){
	case RECORD:  return isFlagSet(IS_RESOLVED);
	case VARIANT: return isFlagSet(IS_RESOLVED);
	default:
		return true;
	}
}

bool Type::isSame(Type* other){
	if(this->type != other->type) return false;
	switch(type){
		case VOID: case TYPE: case BOOL: return true;
		case RECORD :
		case VARIANT:
			return this == other;
		case INTEGER:
		case FLOAT:
		case CHAR:  
			return bits == other->bits;
		case NATURAL:
		case UINTPTRT:
			return true;
		
		case POINTER: 
		case LINEAR_SEQUENCE:
			return argument->isSame(other->argument);
		case STATIC_ARRAY: return argument->isSame(other->argument) && N == other->N;
		
		case FUNCTION_POINTER: 
			return argument->isSame(other->argument) && 
				static_cast<FunctionPointer*>(this)->returns()->isSame(static_cast<FunctionPointer*>(other)->returns());
		
		case NODE: return nodeSubtype == other->nodeSubtype;
		
		case ANONYMOUS_RECORD:
		case ANONYMOUS_VARIANT:
			return static_cast<AnonymousAggregate*>(this)->types  == static_cast<AnonymousAggregate*>(other)->types &&
				   static_cast<AnonymousAggregate*>(this)->fields == static_cast<AnonymousAggregate*>(other)->fields;

		case LITERAL_INTEGER:
		case LITERAL_FLOAT :
		case LITERAL_CHAR  :
		case LITERAL_STRING:  return true;
		default:
			throw std::runtime_error("TypeExpression type invariant failed");	
			return false;
	}
}
bool Type::wasGenerated() const {
	switch(type){
	case POINTER: return true;
	case LINEAR_SEQUENCE: return true;
	case STATIC_ARRAY: return true;
	case FUNCTION_POINTER: return true;
	//case RECORD: return record->wasGenerated();
	default: return false;
	}
}
bool Type::wasGeneratedBy(Function* function) const {
	switch(type){
	case POINTER:         return function == intrinsics::types::PointerTypeGenerator;
	case LINEAR_SEQUENCE: return function == intrinsics::types::LinearSequenceTypeGenerator;
	case STATIC_ARRAY:    return function == intrinsics::types::StaticArrayTypeGenerator;
	case FUNCTION_POINTER:        return function == intrinsics::types::FunctionTypeGenerator;
	//case RECORD: return record->wasGeneratedBy(function);

	default:
		return false;
	}
}
Node* Type::generatedArgument(size_t i) const {
	switch(type){
	case POINTER:
	case LINEAR_SEQUENCE:
		return new TypeReference(argument);
	case STATIC_ARRAY: 
		return i == 0 ? new TypeReference(argument) : (Node*) new IntegerLiteral((uint64)N,intrinsics::types::natural);
	case FUNCTION_POINTER: return new TypeReference(i == 0 ? argument : static_cast<const FunctionPointer*>(this)->returns());
	//case RECORD: return record->generatedArgument(i);

	default:
		throw std::runtime_error("TypeExpression generatedArgument failed");	
		return nullptr;
	}
}

enum {
	LITERAL_TYPE_SPECIFICATION = 4, //Untyped literal to typed
	LITERAL_CONVERSION,      //Untyped literal to another untyped literal
	RECORD_SUBTYPE,
	EXACT
};

inline Node* int2float(IntegerLiteral* node,Type* t){
	return new FloatingPointLiteral((double)node->integer.u64,t);
}
inline Node* int2char(IntegerLiteral* node,Type* t){
	return new CharacterLiteral((UnicodeChar)node->integer.u64,t);
}
inline Node* char2int(CharacterLiteral* node,Type* t){
	return new IntegerLiteral((uint64)node->value,t);
}

inline bool characterFits(int bits,uint32 value){
	if(value < 256) return true;
	if(bits == 16 && value <= std::numeric_limits<uint16>::max() ) return true;//TODO is this correct???
	return bits == 32 && value <= 0x10FFFF? true:false; //UNICODE_MAX
}

bool Type::integerFits(uint64 value,bool isNegative){
	if(isNegative && bits>0) return false;

	int64 min;
	uint64 max;
#define RANGE(T) { min = std::numeric_limits<T>::min() ; max = std::numeric_limits<T>::max() ; }
	if(bits == 32)       RANGE(uint32)
	else if(bits == 64)  RANGE(uint64)
	else if(bits == -32) RANGE(int)
	else if(bits == 16)  RANGE(uint16)
	else if(bits == -16) RANGE(short)
	else if(bits == 8)   RANGE(unsigned char)
	else if(bits == -8)  RANGE(signed char)
	else if(bits == -64) RANGE(int64);
	
	if(bits > 0) return value <= max;
	else return ((int64)value) >= min && value <= max;
#undef RANGE
}

bool   Type::doesLiteralFit(IntegerLiteral* node){
	if(isInteger() ||  isPlatformInteger() || isUintptr()){
		return integerFits(node->integer.u64,node->integer.isNegative());
	}
	else {
		assert(isChar());
		if(node->integer.isNegative()) return false;
		return characterFits(bits,node->integer.u64);
	}
}

inline bool doesIntegerLiteralFitInLiteralChar(IntegerLiteral* node){
	return node->integer.isPositive() && node->integer.u64 <= 0x10FFFF;
}

bool   Type::doesLiteralFit(CharacterLiteral* node){
	if(isInteger()){
		return integerFits(node->value,false);
	}
	else {
		assert(isChar());
		return characterFits(bits,node->value);
	}
}



/*
Given literalNodeType is literal
Scenarios:
  def a = 1 :: literal.integer                => 1 :: literal.integer
  def a int32 = 1 :: literal.integer          => 1 :: int32
  def a literal.float = 1 :: literal.integer  => 1.0 :: literal.float
  def a float = 1 :: literal.integer          => 1.0 :: float
  def c literal.char = 1 :: literal.integer   => '\x01' :: literal.char
  def c char32 = 65 :: literal.integer        => 'A' :: char32

  def x natural = 65 :: literal.integer       => 65  :: natural
  def x uintptr = 65 :: literal.integer       => 65  :: uintptr

  def f float = 2.71 :: literal.float         => 2.71 :: float

  def c char32 = 'A' :: literal.char          => 'A' :: char32
  def c literal.integer = 'A' :: literal.char => 65 :: literal.integer
  def c int32  = 'A' :: literal.char          => 65 :: int32
  def x natural = 'A' :: literal.char         => 65 :: natural

  def s LinearSequence(char8) = "foo" :: literal.string => "foo" :: LinearSequence(char8)
*/
int literalTypeAssignment(Type* givenType,Node** literalNode,Type* literalNodeType,bool doTransform){
	auto expression = *literalNode;

	assert(literalNodeType->isLiteral());
	if( auto integerLiteral = expression->asIntegerLiteral() ){
		// a int32 = 1
		if(givenType->isInteger() && givenType->doesLiteralFit(integerLiteral)){
			if(doTransform) integerLiteral->explicitType = givenType;
			return LITERAL_TYPE_SPECIFICATION;
		}
		// a float = 1
		else if(givenType->type == Type::LITERAL_FLOAT || givenType->isFloat()){
			if(doTransform) *literalNode = int2float(integerLiteral,givenType);
			return givenType->type == Type::LITERAL_FLOAT? LITERAL_CONVERSION : LITERAL_TYPE_SPECIFICATION;
		}
		// a char = 1
		else if( (givenType->type == Type::LITERAL_CHAR && doesIntegerLiteralFitInLiteralChar(integerLiteral) ) 
			|| (givenType->isChar() && givenType->doesLiteralFit(integerLiteral)) ){
			if(doTransform) *literalNode = int2char(integerLiteral,givenType);
			return givenType->type == Type::LITERAL_CHAR? LITERAL_CONVERSION : LITERAL_TYPE_SPECIFICATION;
		}
		// a natural/uintptr = 1
		else if( (givenType->isPlatformInteger() || givenType->isUintptr()) && givenType->doesLiteralFit(integerLiteral)){
			if(doTransform) integerLiteral->explicitType = givenType;
			return LITERAL_TYPE_SPECIFICATION;
		}
	}
	else if( auto floatingLiteral = expression->asFloatingPointLiteral() ){
		//a float = 1.0
		if(givenType->isFloat()){
			if(doTransform) floatingLiteral->explicitType = givenType;
			return LITERAL_TYPE_SPECIFICATION;
		}
	}
	else if( auto characterLiteral = expression->asCharacterLiteral() ){
		//a char32 = 'A'
		if(givenType->isChar() && givenType->doesLiteralFit(characterLiteral) ){
			if(doTransform) characterLiteral->explicitType = givenType;
			return LITERAL_TYPE_SPECIFICATION;
		}
		//a int32 = 'A'
		else if( givenType->type == Type::LITERAL_INTEGER || (givenType->isInteger() && givenType->doesLiteralFit(characterLiteral)) ){
			if(doTransform) *literalNode = char2int(characterLiteral,givenType);
			return givenType->type == Type::LITERAL_INTEGER? LITERAL_CONVERSION : LITERAL_TYPE_SPECIFICATION;
		}
		// a natural = 'A'
		else if(givenType->isPlatformInteger() /*NB: assume platform integers are >= char32 && givenType->doesLiteralFit(characterLiteral)*/){
			if(doTransform) *literalNode = char2int(characterLiteral,givenType);
			return LITERAL_TYPE_SPECIFICATION;
		}
	}
	else if( auto stringLiteral = expression->asStringLiteral() ){
		//a LinearSequence(char8) = "fooo"
		if(givenType->isLinearSequence() && givenType->next()->isChar8()){
			if(doTransform) stringLiteral->explicitType = givenType;
			return LITERAL_TYPE_SPECIFICATION;
		}
	}

	return -1;
}
void TypeMeaningsRange::getLiteralTypesConversions(Type* type){
	switch(type->type){
	case Type::LITERAL_INTEGER:
		break;
	case Type::LITERAL_FLOAT:
		break;
	case Type::LITERAL_CHAR:
		break;
	case Type::LITERAL_STRING:
		add(Type::getLinearSequence(Type::getCharType(8)),LITERAL_TYPE_SPECIFICATION);
		break;
	}
}

/**
TODO: adjust conversion weights

Scenarios:
  def x uint64 = uint8,uint16,uint32
        uint32 = uint8,uint16
		uint16 = uint8

		int64  = (u)int8,(u)int16,(u)int32
		int32  = (u)int8,(u)int16
		int16  = (u)int8,(u)int16

		natural = uint16,uint32,(m64: uint64)

  def x float  = (u)intX
        double = (u)intX
		double = float

  def c char16 = char8
      c char32 = char8,char16
*/
int automaticTypeCast(Type* givenType,Node** node,Type* nodeType,bool doTransform){
	int weight = -1;

	if(givenType->isInteger() && nodeType->isInteger()){
		//uintX = uint(X-1)
		if(givenType->bits > 0){
			if(nodeType->bits > 0 && nodeType->bits < givenType->bits)
				weight = LITERAL_CONVERSION;
		}
		//intX = (u)int(X-1)
		else {
			if(std::abs(nodeType->bits) < -givenType->bits)
				weight = LITERAL_CONVERSION;
		}
	}
	else if(givenType->isFloat()){
		if(nodeType->isFloat() && nodeType->bits < givenType->bits)
			weight = LITERAL_CONVERSION;
		else if(nodeType->isInteger())
			weight = LITERAL_TYPE_SPECIFICATION;
	}
	else if(givenType->isPlatformInteger()){
		//natural = uintX
		if(nodeType->isInteger()){
			if(nodeType->bits > 0 && nodeType->bits < givenType->bits)
				weight = LITERAL_CONVERSION;
		}
	}
	else if(givenType->isChar()){
		//charX = char(X-1)
		if(nodeType->isChar()){
			if(nodeType->bits < givenType->bits)
				weight = LITERAL_CONVERSION;
		}
	}

	if(weight != -1 && doTransform) *node = new CastExpression(*node,givenType);
	return weight;
}

/**
Scenarios:
  type Foo { extends var x int32 ; }
  var foo Foo

  var x int32 = foo , &foo
      x int64 = foo , &foo
  TODO:    x int8  = foo as int8 , &foo as int8

  TODO: type Foo : Bar {} ; def bar(x *Bar) ; var foo Foo ; foo.bar &foo.bar

  TODO: NB: The weights are a bit dodgy now
*/
int recordSubtyping(Type* givenType,Node** node,Type* nodeType,bool doTransform){
	Record* record = nodeType->asRecord();
	if(!record){
		if(nodeType->isPointer()){
			record = nodeType->next()->asRecord();
			if(!record) return -1;
		}
		else return -1;
	}

	for(auto field = record->fields.begin(); field != record->fields.end();++field){
		if((*field).isExtending){
			//NB: try to assign without transformations first
			//NB: when transforming make sure to transform the field access as required
			auto assigns = givenType->assignFrom(node,(*field).type.type(),false);
			if(assigns != -1){
				if(doTransform){
					*node = new FieldAccessExpression(*node,(int)(field - record->fields.begin()));
					givenType->assignFrom(node,(*field).type.type(),true);
				}
				return assigns == EXACT? RECORD_SUBTYPE : assigns;
			}
		}
	}
	return -1;
}
void TypeMeaningsRange::getRecordSubtypes(Record* record){
	for(auto field = record->fields.begin(); field != record->fields.end();++field){
		if((*field).isExtending){
			add((*field).type.type(),RECORD_SUBTYPE);
			gatherMeanings((*field).type.type());
		}
	}
}


/**
Implicit type conversion system.
Returns the weight of the conversion or -1 if the conversion failed.
Scenarios:
	As per above literal,auto and record type casts
	*ast.Call , etc => *ast.Node
*/
int Type::assignFrom(Node** expression,Type* type,bool doTransform){
	if(this->isSame(type)) return EXACT;
	int assigns;

	if(type->isLiteral()){
		return literalTypeAssignment(this,expression,type,doTransform);
	}
	else if( (assigns = automaticTypeCast(this,expression,type,doTransform)) != -1 ){
		return assigns;
	}
	else if( (assigns = recordSubtyping(this,expression,type,doTransform)) != -1){
		return assigns;
	}
	else if( isNodePointer() && type->isNodePointer() ){
		return RECORD_SUBTYPE;
	}

	return -1;
}
void TypeMeaningsRange::gatherMeanings(Type* type){
	if(type->isLiteral()) getLiteralTypesConversions(type);

	Record* record = type->asRecord();
	if(!record){
		if(type->isPointer()){
			record = type->next()->asRecord();
			if(record) getRecordSubtypes(record);
		}
	}
	else getRecordSubtypes(record);
}



int   Type::canAssignFrom(Node* expression,Type* type){
	return assignFrom(&expression,type,false);
}
Node* Type::assignableFrom(Node* expression,Type* type) {
	if(assignFrom(&expression,type,true) != -1) return expression;
	return nullptr;
}

inline bool isIntLike(Type* t){
	return t->isInteger() || t->isPlatformInteger() || t->isUintptr() || t->isChar();
}

/**
Scenarios:
  integers <=> integers
  integers <=> floats
  integers <=> characters
  floats   <=> floats
  characters <=> characters
  integers <=> bool
  Pointer => uintptr
*/
bool  Type::canCastTo(Type* other){
	auto thisIsIntLike  = isIntLike(this);
	auto otherIsIntLike = isIntLike(other);

	if(thisIsIntLike){
		if(otherIsIntLike) return true;
		else if(other->isBool()) return true;
		else if(other->isFloat()) return true;
	}
	else if(this->isFloat()){
		if(otherIsIntLike) return true;
		else if(other->isFloat()) return true;
	}
	else if(this->isBool()){
		if(otherIsIntLike) return true;
	}
	else if(this->isPointer()){
		if(other->isUintptr()) return true;
	}
	return false;
}

inline Node* bool2int(BoolExpression* node,Type* givenType){
	return new IntegerLiteral((uint64)node->value,givenType);
}
inline Node* char2float(CharacterLiteral* node,Type* givenType){
	return new FloatingPointLiteral(node->value,givenType);
}
inline Node* float2int(FloatingPointLiteral* node,Type* givenType){
	return new IntegerLiteral((uint64)node->value,givenType);
}
inline Node* float2char(FloatingPointLiteral* node,Type* givenType){
	return new CharacterLiteral((UnicodeChar)node->value,givenType);
}

//TODO: proper int -x to uint x
Node* evaluateConstantCast(Node* expression,Type* givenType){
	if( auto assigns = givenType->assignableFrom(expression,expression->returnType()) ) return assigns;

	if(auto integerLiteral = expression->asIntegerLiteral()){
		if(givenType->isInteger() || givenType->isPlatformInteger() || givenType->isUintptr()){
			integerLiteral->explicitType = givenType;
		}
		else if(givenType->isFloat())
			return int2float(integerLiteral,givenType);
		else if(givenType->isChar())
			return int2char(integerLiteral,givenType);
	}
	else if( auto floatingLiteral = expression->asFloatingPointLiteral() ){
		if(givenType->isFloat()) floatingLiteral->explicitType = givenType;
		else if(givenType->isInteger() || givenType->isPlatformInteger() || givenType->isUintptr())
			return float2int(floatingLiteral,givenType);
		else if(givenType->isChar())
			return float2char(floatingLiteral,givenType);
	}
	else if( auto characterLiteral = expression->asCharacterLiteral() ){
		if(givenType->isChar()) characterLiteral->explicitType = givenType;
		else if(givenType->isInteger() || givenType->isPlatformInteger() || givenType->isUintptr())
			return char2int(characterLiteral,givenType);
		else if(givenType->isFloat())
			return char2float(characterLiteral,givenType);
	}
	else if( auto stringLiteral = expression->asStringLiteral() ){
		if(givenType->isLinearSequence()) stringLiteral->explicitType = givenType;
	}
	else if( auto boolLiteral = expression->asBoolExpression() ){
		if(givenType->isInteger() || givenType->isPlatformInteger() || givenType->isUintptr())
			return bool2int(boolLiteral,givenType);
	}
	return expression;
}

/**
* Type validity checks
*/
bool isValidType(Type* type){
	if(type->type == Type::NODE) return false;
	return true;
}
bool Type::isValidTypeForVariable(){
	if(type == NODE) return false;
	return true;
}
bool Type::isValidTypeForArgument(){
	if(type == VOID) return false;
	if(type == NODE) return false;
	return true;
}
bool Type::isValidTypeForReturn(){
	bool result = isValidType(this);
	if(type == TYPE) result = false;
	return result;
}
bool Type::isValidTypeForField(){
	bool result = isValidType(this);
	if(type == VOID)      result = false;
	else if(type == TYPE) result = false;
	return result;
}

/**
* Function pointer type
*/
FunctionPointer* FunctionPointer::get(Type* argument,Type* ret,data::ast::Function::CallConvention cc){
	auto type = new FunctionPointer();
	type->argument = argument;
	type->_returns = ret;
	type->cc = cc;
	return type;
}
FunctionPointer* Type::asFunctionPointer(){
	return type == FUNCTION_POINTER? static_cast<FunctionPointer*>(this) : nullptr;
}

/**
* Anonymous records/variants
*/
std::vector<std::pair<Type**,size_t>>    anonymousRecordTypes ;
std::vector<std::pair<SymbolID*,size_t>> anonymousRecordFields;

AnonymousAggregate::AnonymousAggregate(Type** t,SymbolID* fs,size_t n,bool isVariant): Type(isVariant? ANONYMOUS_VARIANT: ANONYMOUS_RECORD),types(t),fields(fs),numberOfFields(n) {
}
AnonymousAggregate* Type::asAnonymousRecord(){
	return type == ANONYMOUS_RECORD? static_cast<AnonymousAggregate*>(this) : nullptr;
}
int AnonymousAggregate::lookupField(const SymbolID fieldName) const {
	if(fields){
		for(auto i = fields;i!= (fields+numberOfFields);i++){
			if( (*i) == fieldName ) return int(i - fields);
		}
	}
	return -1;
}
AnonymousAggregate* AnonymousAggregate::create(Field* fields,size_t fieldsCount,bool isVariant){
	assert(fieldsCount > 1);
	auto end = fields + fieldsCount;

	//Check to see if the record has named fields
	auto areFieldsUnnamed = true;
	for(auto i = fields;i!=end;i++){
		if(!(*i).name.isNull()) areFieldsUnnamed = false;
	}

	//find the corresponding type array
	Type** typeArray = nullptr;
	for(auto i = anonymousRecordTypes.begin();i!=anonymousRecordTypes.end();i++){
		if((*i).second == fieldsCount){
			auto otherFields = (*i).first;
			bool allTypesSame = true;
			for(size_t j=0;j<fieldsCount;j++){
				if(!(otherFields[j]->isSame(fields[j].type))){
					allTypesSame = false;
					break;
				}
			}
			if(allTypesSame){
				typeArray = otherFields;
				break;
			}
		}
	}
	if(!typeArray){
		typeArray = (Type**)System::malloc(sizeof(Type*)*fieldsCount);
		for(size_t j=0;j<fieldsCount;j++) typeArray[j] = fields[j].type;
		anonymousRecordTypes.push_back(std::make_pair(typeArray,fieldsCount));
	}

	if(areFieldsUnnamed) return new AnonymousAggregate(typeArray,nullptr,fieldsCount,isVariant);

	//find the corresponding field array
	SymbolID* symbolArray = nullptr;
	for(auto i = anonymousRecordFields.begin();i!=anonymousRecordFields.end();i++){
		if((*i).second == fieldsCount){
			auto otherFields = (*i).first;
			bool allSymbolsSame = true;
			for(size_t j=0;j<fieldsCount;j++){
				if(!(otherFields[j] == fields[j].name)){
					allSymbolsSame = false;
					break;
				}
			}
			if(allSymbolsSame){
				symbolArray = otherFields;
				break;
			}
		}
	}
	if(!symbolArray){
		symbolArray = (SymbolID*)System::malloc(sizeof(SymbolID)*fieldsCount);
		for(size_t j=0;j<fieldsCount;j++) symbolArray[j] = fields[j].name;
		anonymousRecordFields.push_back(std::make_pair(symbolArray,fieldsCount));
	}
	return new AnonymousAggregate(typeArray,symbolArray,fieldsCount,isVariant);
}

/**
* Record(class like) type
*/
Record::Record() : DeclaredType(Type::RECORD) {
}
Record* Type::asRecord(){
	return type == RECORD? static_cast<Record*>(this) : nullptr;
}
int Record::lookupField(const SymbolID fieldName) const {
	for(size_t i = 0;i<fields.size();i++){
		if( fields[i].name == fieldName ) return int(i);
	}
	return -1;
}
void Record::add(const Field& var){
	assert(!isResolved());
	fields.push_back(var);
}
Record::Field Record::Field::duplicate(DuplicationModifiers* mods) const{
	Field result;
	result.name = name;
	result.type = type.duplicate(mods);
	result.isExtending = isExtending;
	result.initializer = initializer;
	return result;
}
DeclaredType* Record::duplicate(DuplicationModifiers* mods) const {
	auto rec= new Record();
	rec->flags = flags;
	rec->fields.reserve(fields.size());
	for(auto i = fields.begin();i!=fields.end();++i) rec->fields.push_back((*i).duplicate(mods));
	return rec;
}

/**
* Trait(concept like) type
*/
Trait::Trait() : DeclaredType(Type::VARIANT) {
}
DeclaredType* Trait::duplicate(DuplicationModifiers* mods) const {
	auto dup = new Trait();
	dup->flags = flags;
	return dup;
}

/**
* Variant type
*/
Variant::Variant() : DeclaredType(Type::VARIANT) {
}
int Variant::lookupField(const SymbolID fieldName) const {
	for(size_t i = 0;i<fields.size();i++){
		if( fields[i].name == fieldName ) return int(i);
	}
	return -1;
}
void Variant::add(const Field& field) {
	assert(!isResolved());
	fields.push_back(field);
}
DeclaredType* Variant::duplicate(DuplicationModifiers* mods) const{
	auto dup = new Variant();
	dup->flags = flags;
	return dup;
}

/**
* Type declaration node
*/
TypeDeclaration::TypeDeclaration(DeclaredType* type,SymbolID name) : PrefixDefinition(name,Location()), _type(type),optionalStaticBlock(nullptr) { 
	_type->declaration = this; 
}
Type*  TypeDeclaration::type()  const { 
	return _type; 
}
Node*  TypeDeclaration::createReference(){
	return new TypeReference(_type);
}
Node*  TypeDeclaration::duplicate(DuplicationModifiers* mods) const {
	BlockExpression* sb = optionalStaticBlock? optionalStaticBlock->duplicate(mods)->asBlockExpression() : nullptr;
	auto dup = new TypeDeclaration(_type->duplicate(mods),label());
	dup->optionalStaticBlock = sb;
	mods->duplicateDefinition(const_cast<TypeDeclaration*>(this),dup);
	return copyProperties(dup);
}
