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
	Function::CTFE_Binder binder;
	uint16 extraFlags; //extra flags to apply to this intrinsic function

	FunctionBinder() : binder(nullptr),extraFlags(0) {}
	FunctionBinder(Function::CTFE_Binder f,uint16 flags) : binder(f),extraFlags(flags) {}
};
std::map<std::string,FunctionBinder> functionMapping;

bool mappingInitialized = false;


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
	MAP_NODETYPE("UnaryOperation",UnaryOperation);
	MAP_NODETYPE("BinaryOperation",BinaryOperation);
	MAP_NODETYPE("Cast",CastExpression);
	MAP_NODETYPE("Assignment",AssignmentExpression);
	MAP_NODETYPE("IfElse",IfExpression);
	MAP_NODETYPE("ControlFlow",ControlFlowExpression);
	MAP_NODETYPE("Loop",LoopExpression);
	MAP_NODETYPE("Return",ReturnExpression);
	MAP_NODETYPE("Block",BlockExpression);
	MAP_NODETYPE("UnresolvedSymbol",UnresolvedSymbol);

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
	MAP_PROP("newUnaryOperation(uint32,Expression)",0,{ invocation->ret(new UnaryOperation(invocation->getUint32Parameter(0),invocation->getNodeParameter(1))); });
	MAP_PROP("newBinaryOperation(uint32,Expression,Expression)",0,{ invocation->ret(new BinaryOperation(invocation->getUint32Parameter(0),invocation->getNodeParameter(1),invocation->getNodeParameter(2))); });
	
	/**
	* arpha.syntax.parser
	*/
	MAP_PROP("parse(int32,bool)",Function::INTERPRET_ONLY_INSIDE,{ 
		auto parser = invocation->getParser();
		if(invocation->getBoolParameter(1)) parser->ignoreNewlines();
		invocation->ret(parser->parse(invocation->getInt32Parameter(0))); 
	});
	MAP_PROP("expect([]uint8,bool)",Function::INTERPRET_ONLY_INSIDE,{
		auto parser = invocation->getParser();
		if(invocation->getBoolParameter(1)) parser->ignoreNewlines();
		parser->expect(invocation->getStringParameterAsSymbol(0));
		invocation->ret();
	});
	MAP_PROP("match([]uint8,bool)",Function::INTERPRET_ONLY_INSIDE,{
		auto parser = invocation->getParser();
		Parser::NewlineIgnorer i(invocation->getBoolParameter(1),parser);
		auto match = parser->match(invocation->getStringParameterAsSymbol(0));
		if(!match) i.rollback();
		invocation->ret(match);
	});

	/**
	* arpha.functionality.bounded_pointer
	*/
	MAP_PROP("length(BoundedPointer)",Function::MACRO_FUNCTION,{
		invocation->ret(new UnaryOperation(UnaryOperation::BOUNDED_POINTER_LENGTH,invocation->getNodeParameter(0)));
	});
	MAP_PROP("element(BoundedPointer,natural)",Function::MACRO_FUNCTION,{
		invocation->ret(new BinaryOperation(BinaryOperation::BOUNDED_POINTER_ELEMENT,invocation->getNodeParameter(0),invocation->getNodeParameter(1)));
	});
	MAP_PROP("add(BoundedPointer,natural)",Function::MACRO_FUNCTION,{
		invocation->ret(new BinaryOperation(BinaryOperation::ADD,invocation->getNodeParameter(0),invocation->getNodeParameter(1)));
	});

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

std::string mangleArgType(Type* type,bool nat){
	if(type->isType()) return "Type";
	else if(type->isBool()) return "bool";
	else if(type->isNodePointer()) return "Expression";
	else if(type->isInteger()){
		if(nat && type->isSame(intrinsics::types::natural)) return "natural";
		else {
			std::stringstream str;
			str<<(type->bits<0?"int":"uint")<<std::abs(type->bits);
			return str.str();
		}
	}
	else if(type->isPointer()){
		return std::string("*")+mangleArgType(type->next(),nat);
	} else if(type->isBoundedPointer()){
		return std::string("[]")+mangleArgType(type->next(),nat);
	}
	else if(type->isLiteral()){
		std::string result = "literal.";
		return result;
	}
	return "";
}


std::string intrinsicMangle(Function* function,bool argNames,bool nat = false){
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
				if(auto call = pattern->asCallExpression()){
					result+=call->object->asUnresolvedSymbol()->symbol.ptr();
				}
			}
		}
		else result += mangleArgType((*i)->type.type(),nat);
		if((i+1) != function->arguments.end()) result += ",";
	}
	result+=")";
	return result;
}

Function::CTFE_Binder Function::getIntrinsicFunctionBinder(Function* function){
	if(!mappingInitialized) initMapping();
	assert(function->isIntrinsic());
	std::string signature = intrinsicMangle(function,false);


	auto value = functionMapping.find(signature);
	if(value != functionMapping.end()){
		if((*value).second.extraFlags) function->setFlag((*value).second.extraFlags);
		return (*value).second.binder;
	}
	signature = intrinsicMangle(function,true);
	value = functionMapping.find(signature);
	if(value != functionMapping.end()){
		if((*value).second.extraFlags) function->setFlag((*value).second.extraFlags);
		return (*value).second.binder;
	}
	//uint32 => natural mangle
	signature = intrinsicMangle(function,false,true);
	value = functionMapping.find(signature);
	if(value != functionMapping.end()){
		if((*value).second.extraFlags) function->setFlag((*value).second.extraFlags);
		return (*value).second.binder;
	}
	compiler::intrinsicFatalError(function->location(),format(" Intrinsic binding failure - The function '%s' isn't intrinsic!\nPlease don't use the intrinsic property on it!",function->label()));
	return nullptr;
}

Node* Variable::getIntrinsicValue(Variable* variable){
	if(!mappingInitialized) initMapping();

	std::string signature = std::string("ast.") + variable->label().ptr();
	auto value = variableMapping.find(signature);
	if(value != variableMapping.end()) return (*value).second;
	compiler::intrinsicFatalError(variable->location(),format(" Intrinsic binding failure - The variable '%s' isn't intrinsic!\nPlease don't use the intrinsic property on it!",variable->label()));
	return new UnitExpression();
}

