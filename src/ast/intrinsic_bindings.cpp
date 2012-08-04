#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "interpret.h"
#include "../syntax/parser.h"
#include "../intrinsics/types.h"


std::map<std::string,Node*> variableMapping; //mapping constant variables to values

struct FunctionBinder {
	bool   isOperation;
	uint16 extraFlags; //extra flags to apply to this intrinsic function
	union {
		Function::CTFE_Binder binder;
		data::ast::Operations::Kind operation;
	};
	

	FunctionBinder() : isOperation(false),binder(nullptr),extraFlags(0) {}
	FunctionBinder(Function::CTFE_Binder f,uint16 flags) : isOperation(false),binder(f),extraFlags(flags) {}
	FunctionBinder(data::ast::Operations::Kind operation,uint16 flags) : isOperation(true),extraFlags(flags),operation(operation) {}

	void bind(Function* function){
		if(isOperation) function->makeIntrinsicOperation(operation);
		else function->intrinsicCTFEbinder = binder;
		function->setFlag(extraFlags);
	}
};
std::map<std::string,FunctionBinder> functionMapping;

bool mappingInitialized = false;

#define MAPOP(sig,op) functionMapping[sig] = FunctionBinder(op,Function::PURE);



std::string mangleArgType(Type* type){
	if(type->isType()) return "Type";
	else if(type->isBool()) return "bool";
	else if(type->isNodePointer()) return "Expression";
	else if(type->isInteger()){
		std::stringstream str;
		str<<(type->bits<0?"int":"uint")<<std::abs(type->bits);
		return str.str();
	}
	else if(type->isChar()){
		return "char8";
	}
	else if(type->isPlatformInteger()){
		return "natural";
	}
	else if(type->isUintptr()){
		return "uintptr";
	}
	else if(type->isPointer()){
		return std::string("*")+mangleArgType(type->next());
	} else if(type->isLinearSequence()){
		return std::string("[]")+mangleArgType(type->next());
	}
	else if(type->isLiteral()){
		if(type->type == Type::LITERAL_INTEGER) return "literal.integer";
		else if(type->type == Type::LITERAL_FLOAT) return "literal.real";
		else if(type->type == Type::LITERAL_CHAR) return "literal.char";
		else if(type->type == Type::LITERAL_STRING) return "literal.string";
	}
	return "";
}

void mapIntegerOperations(const char* t1){
	std::string base1 = std::string("(") + t1 + ")";
	std::string base2 = std::string("(") + t1 + "," + t1 + ")";
	MAPOP(std::string("minus")+base1,data::ast::Operations::NEGATION);

	MAPOP(std::string("add")+base2,data::ast::Operations::ADDITION);
	MAPOP(std::string("subtract")+base2,data::ast::Operations::SUBTRACTION);
	MAPOP(std::string("multiply")+base2,data::ast::Operations::MULTIPLICATION);
	MAPOP(std::string("divide")+base2,data::ast::Operations::DIVISION);
	MAPOP(std::string("remainder")+base2,data::ast::Operations::REMAINDER);

	MAPOP(std::string("bit_not")+base1,data::ast::Operations::BIT_NOT);
	MAPOP(std::string("bit_and")+base2,data::ast::Operations::BIT_AND);
	MAPOP(std::string("bit_or")+base2,data::ast::Operations::BIT_OR);
	MAPOP(std::string("bit_xor")+base2,data::ast::Operations::BIT_XOR);

	MAPOP(std::string("shl")+base2,data::ast::Operations::LEFT_SHIFT);
	MAPOP(std::string("shr")+base2,data::ast::Operations::RIGHT_SHIFT);

	MAPOP(std::string("equals")+base2,data::ast::Operations::EQUALITY_COMPARISON);
	MAPOP(std::string("less")+base2,data::ast::Operations::LESS_COMPARISON);
	MAPOP(std::string("greater")+base2,data::ast::Operations::GREATER_COMPARISON);
	MAPOP(std::string("lessEquals")+base2,data::ast::Operations::LESS_EQUALS_COMPARISON);
	MAPOP(std::string("greaterEquals")+base2,data::ast::Operations::GREATER_EQUALS_COMPARISON);
}
void mapRealOperations(const char* t1){
	std::string base1 = std::string("(") + t1 + ")";
	std::string base2 = std::string("(") + t1 + "," + t1 + ")";
	MAPOP(std::string("minus")+base1,data::ast::Operations::NEGATION);

	MAPOP(std::string("add")+base2,data::ast::Operations::ADDITION);
	MAPOP(std::string("subtract")+base2,data::ast::Operations::SUBTRACTION);
	MAPOP(std::string("multiply")+base2,data::ast::Operations::MULTIPLICATION);
	MAPOP(std::string("divide")+base2,data::ast::Operations::DIVISION);
	MAPOP(std::string("remainder")+base2,data::ast::Operations::REMAINDER);

	MAPOP(std::string("equals")+base2,data::ast::Operations::EQUALITY_COMPARISON);
	MAPOP(std::string("less")+base2,data::ast::Operations::LESS_COMPARISON);
	MAPOP(std::string("greater")+base2,data::ast::Operations::GREATER_COMPARISON);
	MAPOP(std::string("lessEquals")+base2,data::ast::Operations::LESS_EQUALS_COMPARISON);
	MAPOP(std::string("greaterEquals")+base2,data::ast::Operations::GREATER_EQUALS_COMPARISON);

	MAPOP(std::string("abs")+base1,data::ast::Operations::MATH_ABS);
	MAPOP(std::string("pow")+base2,data::ast::Operations::MATH_POW);
	MAPOP(std::string("sqrt")+base1,data::ast::Operations::MATH_SQRT);
	MAPOP(std::string("exp")+base1,data::ast::Operations::MATH_EXP);
	MAPOP(std::string("log")+base1,data::ast::Operations::MATH_LOG);

	MAPOP(std::string("sin")+base1,data::ast::Operations::TRIG_SIN);
	MAPOP(std::string("cos")+base1,data::ast::Operations::TRIG_COS);
	MAPOP(std::string("tan")+base1,data::ast::Operations::TRIG_TAN);
	MAPOP(std::string("asin")+base1,data::ast::Operations::TRIG_ASIN);
	MAPOP(std::string("acos")+base1,data::ast::Operations::TRIG_ACOS);
	MAPOP(std::string("atan")+base1,data::ast::Operations::TRIG_ATAN);
	MAPOP(std::string("atan2")+base2,data::ast::Operations::TRIG_ATAN2);
}
void mapCharOperations(const char* t1){
	std::string base1 = std::string("(") + t1 + ")";
	std::string base2 = std::string("(") + t1 + "," + t1 + ")";

	MAPOP(std::string("equals")+base2,data::ast::Operations::EQUALITY_COMPARISON);
}

void mapStandartOperations(Type* t){
	auto s =mangleArgType(t);
	if(t->isInteger() || t->type == Type::LITERAL_INTEGER || t->isPlatformInteger() || t->isUintptr() ){
		mapIntegerOperations(s.c_str());
	}
	else if(t->isChar() || t->type == Type::LITERAL_CHAR){
		mapCharOperations(s.c_str());
	}
	else if(t->isFloat() || t->type == Type::LITERAL_FLOAT){
		mapRealOperations(s.c_str());
	}
	else if(t->isBool()){
		MAPOP("not(bool)",data::ast::Operations::NEGATION);
		MAPOP("equals(bool,bool)",data::ast::Operations::EQUALITY_COMPARISON);
	}
}

/**
* This function initializes the mapping between intrinsic definitions in arpha and the appropriate value or handler from the source code
* TODO bind sizeof etc with backend
*/
static void initMapping(){

#define MAP_PROP(sig,prop,fun) \
	{ \
		struct Binder { \
			static void f(CTFEintrinsicInvocation* invocation) fun\
		}; \
		functionMapping[sig] = FunctionBinder(Binder::f,prop);\
	} 	

#define MAP(sig,fun) MAP_PROP(sig,Function::PURE,fun)

	/**
	* arpha.types
	*/
	MAP("equals(Type,Type)",   { invocation->ret(invocation->getTypeParameter(0)->isSame(invocation->getTypeParameter(1))); });
	MAP("isParametrized(Type)",{ invocation->ret(invocation->getTypeParameter(0)->wasGenerated()); });

#define MAP_LITERAL_TYPE(s,T) variableMapping[std::string("literal.")+s] = new TypeReference(T)
	MAP_LITERAL_TYPE("integer",Type::getIntegerLiteralType());
	MAP_LITERAL_TYPE("real"   ,Type::getFloatLiteralType());
	MAP_LITERAL_TYPE("char"   ,Type::getCharLiteralType());
	MAP_LITERAL_TYPE("string" ,Type::getStringLiteralType());

	//arpha.ast.ast
	int nodeSubtype = -1;
#define MAP_NODETYPE(s,T) variableMapping[std::string("ast.")+s] = new TypeReference(new Type(Type::NODE,nodeSubtype++))
	
	MAP_NODETYPE("Expression",Node);
	MAP_NODETYPE("IntegerLiteral",IntegerLiteral);
	MAP_NODETYPE("FloatingPointLiteral",FloatingPointLiteral);
	MAP_NODETYPE("BoolLiteral",BoolExpression);
	MAP_NODETYPE("StringLiteral",StringLiteral);
	MAP_NODETYPE("TypeReference",TypeReference);
	MAP_NODETYPE("Unit",UnitExpression);
	MAP_NODETYPE("Tuple",TupleExpression);
	MAP_NODETYPE("VariableReference",VariableReference);
	MAP_NODETYPE("PointerOperation",PointerOperation);
	MAP_NODETYPE("FieldAccess",FieldAccessExpression);
	MAP_NODETYPE("Call",CallExpression);
	MAP_NODETYPE("LogicalOperation",LogicalOperation);
	MAP_NODETYPE("Cast",CastExpression);
	MAP_NODETYPE("Assignment",AssignmentExpression);
	MAP_NODETYPE("IfElse",IfExpression);
	MAP_NODETYPE("ControlFlow",ControlFlowExpression);
	MAP_NODETYPE("Loop",LoopExpression);
	MAP_NODETYPE("Return",ReturnExpression);
	MAP_NODETYPE("Block",BlockExpression);
	MAP_NODETYPE("UnresolvedSymbol",UnresolvedSymbol);
	MAP_NODETYPE("Function",Function);

	MAP("returnType(Expression)",{ invocation->ret(invocation->getNodeParameter(0)->returnType()); });
	MAP("isConst(Expression)",{ invocation->ret(invocation->getNodeParameter(0)->isConst()); });
	MAP("location(Expression)",{ auto node = invocation->getNodeParameter(0); invocation->retNaturalNatural(node->location().line(),node->location().column); });

	MAP_PROP("newTypeReference(Type)",0,{ invocation->ret(new TypeReference(invocation->getTypeParameter(0))); });
	MAP_PROP("newLoop(Expression)",0,{ invocation->ret(new LoopExpression(invocation->getNodeParameter(0))); });
	MAP_PROP("newReturn(Expression)",0,{ invocation->ret(new ReturnExpression(invocation->getNodeParameter(0))); });
	MAP_PROP("newIfElse(Expression,Expression,Expression)",0,{ invocation->ret(new IfExpression(invocation->getNodeParameter(0),invocation->getNodeParameter(1),invocation->getNodeParameter(2))); });
	MAP_PROP("newCast(Expression,Expression)",0,{ invocation->ret(new CastExpression(invocation->getNodeParameter(0),invocation->getNodeParameter(1)->asTypeReference()->type)); });
	MAP_PROP("newControlFlow(continue:bool)",0,{ invocation->ret(new ControlFlowExpression(ControlFlowExpression::CONTINUE)); });
	MAP_PROP("newControlFlow(break:bool)",0,{ invocation->ret(new ControlFlowExpression(ControlFlowExpression::BREAK)); });
	MAP_PROP("newControlFlow(fallthrough:bool)",0,{ invocation->ret(new ControlFlowExpression(ControlFlowExpression::FALLTHROUGH)); });
	MAP_PROP("newPointerOperation(expression:Expression,addressof:bool)",0,{ invocation->ret(new PointerOperation(invocation->getNodeParameter(0),PointerOperation::ADDRESS)); });
	MAP_PROP("newPointerOperation(expression:Expression,dereference:bool)",0,{ invocation->ret(new PointerOperation(invocation->getNodeParameter(0),PointerOperation::DEREFERENCE)); });
	MAP_PROP("newLogicalOperation(a:Expression,b:Expression,and:bool)",0,{ invocation->ret(new LogicalOperation(invocation->getNodeParameter(0),invocation->getNodeParameter(1),false)); });
	MAP_PROP("newLogicalOperation(a:Expression,b:Expression,or:bool)",0,{ invocation->ret(new LogicalOperation(invocation->getNodeParameter(0),invocation->getNodeParameter(1),true)); });
	MAP_PROP("newFunction(body:Expression)",0,{
		auto body = invocation->getNodeParameter(0);
		auto func = new Function("foo",body->location());
		func->body.addChild(body);
		invocation->ret(func);
	});
	MAP_PROP("newFunction(body:Expression,returnType:Type)",0,{
		auto body = invocation->getNodeParameter(0);
		auto func = new Function("foo",body->location());
		func->_returnType.specify(invocation->getTypeParameter(1));
		func->body.addChild(body);
		invocation->ret(func);
	});

	/**
	* arpha.syntax.parser
	*/
	MAP_PROP("parse(int32,bool)",Function::INTERPRET_ONLY_INSIDE,{ 
		auto parser = invocation->getParser();
		if(invocation->getBoolParameter(1)) parser->ignoreNewlines();
		invocation->ret(parser->parse(invocation->getInt32Parameter(0))); 
	});
	MAP_PROP("expect([]char8,bool)",Function::INTERPRET_ONLY_INSIDE,{
		auto parser = invocation->getParser();
		if(invocation->getBoolParameter(1)) parser->ignoreNewlines();
		parser->expect(invocation->getStringParameterAsSymbol(0));
		invocation->ret();
	});
	MAP_PROP("match([]char8,bool)",Function::INTERPRET_ONLY_INSIDE,{
		auto parser = invocation->getParser();
		Parser::NewlineIgnorer i(invocation->getBoolParameter(1),parser);
		auto match = parser->match(invocation->getStringParameterAsSymbol(0));
		if(!match) i.rollback();
		invocation->ret(match);
	});

	/**
	*arpha.functionality.operations.integer
	*/

	mapStandartOperations(Type::getIntegerLiteralType());
	mapStandartOperations(intrinsics::types::int32);
	mapStandartOperations(intrinsics::types::int8);
	mapStandartOperations(intrinsics::types::int16);
	mapStandartOperations(intrinsics::types::int64);
	mapStandartOperations(intrinsics::types::uint32);
	mapStandartOperations(intrinsics::types::uint8);
	mapStandartOperations(intrinsics::types::uint16);
	mapStandartOperations(intrinsics::types::uint64);
	mapStandartOperations(Type::getNaturalType());
	mapStandartOperations(Type::getUintptrType());

	mapStandartOperations(Type::getFloatLiteralType());
	mapStandartOperations(Type::getFloatType(32));
	mapStandartOperations(Type::getFloatType(64));

	mapStandartOperations(intrinsics::types::boolean);

	mapStandartOperations(Type::getCharLiteralType());
	mapStandartOperations(Type::getCharType(8));
	mapStandartOperations(Type::getCharType(16));
	mapStandartOperations(Type::getCharType(32));

	using namespace data::ast::Operations;

	MAPOP("length(*LinearSequence)",LENGTH);
	MAPOP("element(*LinearSequence,natural)",ELEMENT_GET);

	MAPOP("empty(*LinearSequence)",SEQUENCE_EMPTY);
	MAPOP("current(*LinearSequence)",ELEMENT_GET);
	MAPOP("moveNext(*LinearSequence)",SEQUENCE_MOVENEXT);

	MAP_PROP("length(*Array)",Function::MACRO_FUNCTION,{ 
		invocation->ret(new IntegerLiteral((uint64)invocation->getNodeParameter(0)->returnType()->next()->asStaticArray()->length(),intrinsics::types::natural)); 
	});
	MAPOP("element(*Array,natural)",ELEMENT_GET);

	//Core vector operations
	MAPOP("element(Vector,natural)",ELEMENT_GET);
	MAPOP("shuffle(Vector,Vector)",VECTOR_SHUFFLE);
	MAPOP("shuffle(Vector,Vector,Vector)",VECTOR_SHUFFLE);
	MAPOP("add(Vector,Vector)",ADDITION);
	MAPOP("subtract(Vector,Vector)",SUBTRACTION);
	MAPOP("multiply(Vector,Vector)",MULTIPLICATION);
	MAPOP("divide(Vector,Vector)",DIVISION);
	MAPOP("remainder(Vector,Vector)",REMAINDER);

	MAPOP("equals(Vector,Vector)",EQUALITY_COMPARISON);
	MAPOP("less(Vector,Vector)",LESS_COMPARISON);
	MAPOP("greater(Vector,Vector)",GREATER_COMPARISON);

	/**
	* arpha.functionality.misc
	*/
	MAP_PROP("typeof(_)",Function::MACRO_FUNCTION,{ 
		invocation->ret(new TypeReference(invocation->getNodeParameter(0)->returnType())); 
	});
	MAP("sizeof(Type)",{ 
		invocation->retNatural(4); 
	});
	MAP("alignof(Type)",{ 
		invocation->retNatural(4); 
	});
	MAP_PROP("offsetof(_)",Function::MACRO_FUNCTION,{
		auto node = invocation->getNodeParameter(0);
		if(auto field= node->asFieldAccessExpression()){
			invocation->ret(new IntegerLiteral(BigInt((uint64)0),intrinsics::types::natural));
		} else {
			invocation->retError("The parameter to the function 'offsetof' must be a valid field access expression!");
		}
	});
	mappingInitialized = true;
}

Type* Function::getIntrinsicOperationReturnType(Type* operand1,data::ast::Operations::Kind op){
	using namespace data::ast::Operations;

	if(operand1->isLinearSequence()){
		if(op == ELEMENT_GET) return Type::getReferenceType(operand1->next());
	}
	else if(auto aggr = operand1->asAnonymousRecord()){
		if(!aggr->isFlagSet(AnonymousAggregate::GEN_REWRITE_AS_VECTOR)) return nullptr;

		if(op == ELEMENT_GET) return aggr->types[0];
		else if(op >= EQUALITY_COMPARISON && op <= LESS_EQUALS_COMPARISON) return AnonymousAggregate::getVector(intrinsics::types::boolean,aggr->numberOfFields);
		else return operand1;
	}

	assert(false && "Invalid operation");
	return nullptr;
}


std::string intrinsicMangle(Function* function,bool argNames){
	std::string result = function->label().ptr();
	result += "(";
	for(auto i = function->arguments.begin();i!=function->arguments.end();i++){
		if(argNames){
			result+=(*i)->label().ptr();
			result+=':';
		}
		//magle arguments type
		if((*i)->type.isPattern()){
			if((*i)->type.pattern == nullptr) result+="_";
			else {
				auto pattern = (*i)->type.pattern;
				if(auto ptr = pattern->asPointerOperation()){
					result+="*";
					if(auto call2 = ptr->expression->asCallExpression()){
						result+=call2->object->asUnresolvedSymbol()->symbol.ptr();
					}
				}
				else if(auto call = pattern->asCallExpression()){
					result+=call->object->asUnresolvedSymbol()->symbol.ptr();
					if(auto call2 = call->arg->asCallExpression()){
						result+=std::string("(")+call2->object->asUnresolvedSymbol()->symbol.ptr()+")";
					}
				}
			}
		}
		else result += mangleArgType((*i)->type.type());
		if((i+1) != function->arguments.end()) result += ",";
	}
	result+=")";
	return result;
}

void Function::getIntrinsicFunctionBinder(Function* function){
	if(!mappingInitialized) initMapping();
	assert(function->isIntrinsic());

	std::string signature = intrinsicMangle(function,false);
	auto value = functionMapping.find(signature);
	if(value != functionMapping.end()){
		(*value).second.bind(function);
		return;
	}

	signature = intrinsicMangle(function,true);
	value = functionMapping.find(signature);
	if(value != functionMapping.end()){
		(*value).second.bind(function);
		return;
	}
	
	compiler::intrinsicFatalError(function->location(),format(" Intrinsic binding failure - The function '%s' isn't intrinsic(man: %s)!\nPlease don't use the intrinsic property on it!",function->label(),signature));
}

void Function::getIntrinsicTypeTemplateBinder(Function* function){
	assert(function->isIntrinsic());
	bool error = false;

	struct Generator { 
		static void linearSequence(CTFEintrinsicInvocation* invocation){
			invocation->ret(Type::getLinearSequence(invocation->getTypeParameter(0)));
		}
		static void functionPointer(CTFEintrinsicInvocation* invocation){
			invocation->ret(FunctionPointer::get(invocation->getTypeParameter(0),invocation->getTypeParameter(1)));
		}
		static void reference(CTFEintrinsicInvocation* invocation){
			invocation->ret(Type::getReferenceType(invocation->getTypeParameter(0)));
		}
		static void staticArray(CTFEintrinsicInvocation* invocation){
			invocation->ret(StaticArray::get(invocation->getTypeParameter(0),invocation->getUint32Parameter(1)));
		}
		static void vector(CTFEintrinsicInvocation* invocation){
			auto t = invocation->getTypeParameter(0);
			if(t->isInteger() || t->isFloat() || t->isChar() || t->isBool()){
				invocation->ret(AnonymousAggregate::getVector(invocation->getTypeParameter(0),invocation->getUint32Parameter(1)));
			}
			else invocation->retError("Invalid element type for a vector type!");
		}
	};

	if(function->label() == "LinearSequence" && !Type::generators::linearSequence){
		Type::generators::linearSequence = function;
		function->intrinsicCTFEbinder = Generator::linearSequence;
	}
	else if(function->label() == "FunctionPointer" && !Type::generators::functionPointer){
		Type::generators::functionPointer = function;
		function->intrinsicCTFEbinder = Generator::functionPointer;
	}
	else if(function->label() == "Reference" && !Type::generators::reference){
		Type::generators::reference = function;
		function->intrinsicCTFEbinder = Generator::reference;
	}
	else if(function->label() == "Array" && !Type::generators::staticArray){
		Type::generators::staticArray = function;
		function->intrinsicCTFEbinder = Generator::staticArray;
	}
	else if(function->label() == "Vector" && !Type::generators::vector){
		Type::generators::vector = function;
		function->intrinsicCTFEbinder = Generator::vector;
	}
	else {
		error = true;
	}


	if(error)
		compiler::intrinsicFatalError(function->location(),format(" Intrinsic binding failure - The type '%s' isn't intrinsic!\nPlease don't use the intrinsic property on it!",function->label()));
}

Node* Variable::getIntrinsicValue(Variable* variable){
	if(!mappingInitialized) initMapping();

	std::string signature = std::string("ast.") + variable->label().ptr();
	auto value = variableMapping.find(signature);
	if(value != variableMapping.end()) return (*value).second;
	signature = std::string("literal.") + variable->label().ptr();
	value = variableMapping.find(signature);
	if(value != variableMapping.end()) return (*value).second;
	compiler::intrinsicFatalError(variable->location(),format(" Intrinsic binding failure - The variable '%s' isn't intrinsic!\nPlease don't use the intrinsic property on it!",variable->label()));
	return new UnitExpression();
}

