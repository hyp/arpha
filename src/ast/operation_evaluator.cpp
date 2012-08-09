/**
* This module implements compile time evaluation of various operations
*/
#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "resolve.h"
#include "interpret.h"
#include "../intrinsics/types.h"

using namespace data::ast::Operations;

/**
* TODO proper implementation,unittests
*/
void   doCalculation(data::ast::Operations::Kind op,BigInt& operand1,BigInt& operand2){
	switch(op){
	case NEGATION:
		operand1.changeSign();
		break;

	case ADDITION:
		operand1.u64 = operand1.u64 + operand2.u64;
		break;
	case SUBTRACTION:
		operand1.u64 = operand1.u64 - operand2.u64;
		break;
	case MULTIPLICATION:
		operand1.u64 = operand1.u64 * operand2.u64;
		break;
	case DIVISION:
		operand1.u64 = operand1.u64 / operand2.u64;
		break;
	case REMAINDER:
		operand1.u64 = operand1.u64 % operand2.u64;
		break;

	case BIT_NOT:
		operand1.u64 = ~operand1.u64;
		break;
	case BIT_AND:
		operand1.u64 = operand1.u64 & operand2.u64;
		break;
	case BIT_OR:
		operand1.u64 = operand1.u64 | operand2.u64;
		break;
	case BIT_XOR:
		operand1.u64 = operand1.u64 ^ operand2.u64;
		break;

	case LEFT_SHIFT:
		operand1.u64 = operand1.u64 << operand2.u64;
		break;
	case RIGHT_SHIFT:
		operand1.u64 = operand1.u64 >> operand2.u64;
		break;
	}
}

bool   integerOverflowOccured(Type* resultingType,data::ast::Operations::Kind op,BigInt& result){
	if( (op >= NEGATION && op <= REMAINDER) || op == LEFT_SHIFT){
		return !resultingType->integerFits(result.u64,result.isNegative());
	}
	return false;
}

void   doCalculation(data::ast::Operations::Kind op,double& operand1,double operand2){
	switch(op){
	case NEGATION:
		operand1 = -operand1;
		break;

	case ADDITION:
		operand1 = operand1 + operand2;
		break;
	case SUBTRACTION:
		operand1 = operand1 - operand2;
		break;
	case MULTIPLICATION:
		operand1 = operand1 * operand2;
		break;
	case DIVISION:
		operand1 = operand1 / operand2;
		break;
	}
}


double doOtherRealCalculation(data::ast::Operations::Kind op,double operand1){
	switch(op){
	case MATH_ABS:
		return fabs(operand1);
	case MATH_SQRT:
		return sqrt(operand1);
	case MATH_EXP:
		return exp(operand1);
	case MATH_LOG:
		return log(operand1);

	case TRIG_SIN:
		return sin(operand1);
	case TRIG_COS:
		return cos(operand1);
	case TRIG_TAN:
		return tan(operand1);
	case TRIG_ASIN:
		return asin(operand1);
	case TRIG_ACOS:
		return acos(operand1);
	case TRIG_ATAN:
		return atan(operand1);
	}
}

//TODO
#define DO_COMPARISONS \
	switch(op){ \
	case EQUALITY_COMPARISON: return operand1 == operand2; \
	case LESS_COMPARISON:     return operand1 < operand2; \
	/*case GREATER_COMPARISON:  return operand1 > operand2;*/ \
	} \
	assert(false);

bool doComparison(data::ast::Operations::Kind op,BigInt& operand1,BigInt& operand2){
	DO_COMPARISONS
}

bool doComparison(data::ast::Operations::Kind op,double operand1,double operand2){
	DO_COMPARISONS
}

bool doComparison(data::ast::Operations::Kind op,UnicodeChar operand1,UnicodeChar operand2){
	switch(op){
	case EQUALITY_COMPARISON:
		return operand1 == operand2;
	}
	return false;
}

void doBooleanOperation(data::ast::Operations::Kind op,bool& operand1,bool operand2){
	switch(op){
	case NEGATION:
		operand1 = !operand1;
		break;
	
	case EQUALITY_COMPARISON:
		operand1 = operand1 == operand2;
		break;
	}
}

inline bool isCalculationOperation(data::ast::Operations::Kind op){
	return op >= NEGATION && op <= RIGHT_SHIFT;
}
inline int  calculationOperationNumberOfParameters(data::ast::Operations::Kind op){
	return op == NEGATION || op == BIT_NOT ? 1 : 2;
}
inline bool isComparisonOperation(data::ast::Operations::Kind op){
	return op >= EQUALITY_COMPARISON && op <= GREATER_EQUALS_COMPARISON;
}
inline bool isOtherRealCalculation(data::ast::Operations::Kind op){
	return op>=MATH_ABS && op<=TRIG_ATAN2;
}

Node* evaluateConstantOperation(data::ast::Operations::Kind op,Node* parameter){
	Node** params;
	if(auto tuple = parameter->asTupleExpression()) params = tuple->childrenPtr();
	else params = &parameter;
	
	auto ret = params[0]->returnType();

	if(ret->isInteger() || ret->isPlatformInteger() || ret->isUintptr()){
		auto int0= params[0]->asIntegerLiteral();
		BigInt& operand1 = int0->integer; 
		if(isCalculationOperation(op)){ 
			doCalculation(op,operand1,calculationOperationNumberOfParameters(op) == 1? operand1 : params[1]->asIntegerLiteral()->integer); 
			if(int0->isUntypedLiteral()){
				int0->explicitType = Type::getBestFitIntegerType(int0->integer); //NB: required after operations which overflow, e.g. 2billion :: int32 + 1billion :: int32 = 3billion :: uint32
			}
			else{
				if(integerOverflowOccured(ret,op,operand1)) error(params[0],"Integer overflow occured when performing an integer calculation at compile time");
			}
		} 
		else if(isComparisonOperation(op)){ 
			auto value = doComparison(op,operand1,params[1]->asIntegerLiteral()->integer); 
			return new BoolExpression(value); 
		}
	}
	else if(ret->isChar()){
			auto value = doComparison(op,params[0]->asCharacterLiteral()->value,params[1]->asCharacterLiteral()->value); 
			return new BoolExpression(value); 
	}
	else if(ret->isFloat()){
		double& operand1 = params[0]->asFloatingPointLiteral()->value; 
		if(isCalculationOperation(op)){ 
			doCalculation(op,operand1,calculationOperationNumberOfParameters(op) == 1? operand1 : params[1]->asFloatingPointLiteral()->value); 
		}
		else if(isOtherRealCalculation(op)){
			if(op == TRIG_ATAN2) operand1 = atan2(operand1,params[1]->asFloatingPointLiteral()->value);
			else if(op == MATH_POW) operand1 = pow(operand1,params[1]->asFloatingPointLiteral()->value);
			else operand1 = doOtherRealCalculation(op,operand1);
		}
		else if(isComparisonOperation(op)){ 
			auto value = doComparison(op,operand1,params[1]->asFloatingPointLiteral()->value); 
			return new BoolExpression(value); 
		}
	}
	else if(ret->isBool()){
		bool& operand1 = params[0]->asBoolExpression()->value; 
		doBooleanOperation(op,operand1,op==NEGATION? operand1 : params[1]->asBoolExpression()->value);
	}

	return params[0];
}

