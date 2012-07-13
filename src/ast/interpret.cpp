#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/system.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "resolve.h"
#include "interpret.h"
#include "../intrinsics/types.h"

/**
* Walks the AST node and checks if the node can be interpreted at compile-time.
* This checks all expressions in the AST node and doesn't account for branches that won't be reached when interpreting
*/
struct Interpreter : NodeVisitor {
	Node* returnValueRegister;
	Node** registers;
	Function* currentFunction;

	Interpreter() {
		status = WALKING;
		walkEverywhere = false;
	}
	enum {
		WALKING,
		RETURN, //unroll the stack till the function epilogue
		BREAK,
		CONTINUE,
		FAILURE //unroll the stack to base
	};
	int status;
	bool walkEverywhere;

	Node* failureNode;
	const char* failureReason;

	Node* fail(Node* node,const char* reason = nullptr){
		status = FAILURE;
		failureNode = node;
		failureReason = reason;
		return nullptr;
	}

	//Interpreting expressions
	Node* visit(ReturnExpression* node){
		auto result = node->expression->accept(this);
		if(!walkEverywhere){
			returnValueRegister = result;
			status = RETURN;
		}
		return result;
	}
	static bool isConst(Node* node){
		if(node->isConst()) return true;
		if(auto tuple = node->asTupleExpression()){
			for(auto i = tuple->begin();i!= tuple->end(); i++){
				if(!isConst(*i)) return false;
			}
			return true;
		}
		return false;
	}
	Node* visit(LogicalOperation* node){
		auto a = node->parameters[0]->accept(this);
		auto b = node->parameters[1]->accept(this);
		if(status == FAILURE) return nullptr;
		//TODO
		return fail(node);
	}
	Node* visit(VariableReference* node){
		if(node->variable->functionOwner() == currentFunction) return registers[node->variable->ctfeRegisterID];
		else return fail(node,"This variable is outside the function's scope!");
	}
	Node* visit(AssignmentExpression* node){
		auto obj = node->object;//NB: Don't interpret the object!
		auto val = node->value->accept(this);
		if(status == FAILURE) return nullptr;
		if(isConst(val)){
			Variable* variable;
			if(auto var = obj->asVariableReference()) variable = var->variable;
			else variable = obj->asVariable();
			if(variable){
				if(variable->functionOwner() == currentFunction){
					registers[variable->ctfeRegisterID] = val;
					return val;
				}
				else return fail(node,"This variable is outside the function's scope!");
			}
		}
		return fail(node);
	}
	Node* visit(TupleExpression* node){
		bool createNew = false;
		std::vector<Node*> newChildren;
		newChildren.reserve(node->children.size());
		for(auto i = node->children.begin();i!=node->children.end();i++){
			auto res = (*i)->accept(this);
			newChildren.push_back(res);
			if(res != (*i)) createNew = true;
		}
		if(status == FAILURE) return nullptr;
		if(createNew){
			auto tuple = new TupleExpression();
			node->copyLocationSymbol(tuple);
			tuple->children = newChildren;
			return tuple;
		}
		else return node;
	}


	Node* interpretCall(Function* f,Node* parameter){	
		//Set arguments' values
		if(parameter){	
			auto argsBegin = parameter->asTupleExpression() && f->arguments.size()>1 ? parameter->asTupleExpression()->children.begin()._Ptr : &(parameter);
			for(size_t i = 0;i<f->arguments.size();i++){
				registers[f->arguments[i]->ctfeRegisterID] = argsBegin[i];
			}
		}
		//intepret body
		return f->body.accept(this);
	}
	Node* visit(CallExpression* node){
		auto arg = node->arg->accept(this);
		auto obj = node->object->accept(this);
		if(status == FAILURE) return nullptr;
		if(auto func = obj->asFunctionReference()){
			if(func->function->intrinsicCTFEbinder && isConst(arg)){
				CTFEintrinsicInvocation i(compiler::currentUnit());
				i.invoke(func->function,arg);
				return i.result();
			}
			return fail(node,"The function can't be interpreted!");
		}
		else return fail(node);
	}
	Node* visit(BlockExpression* node){
		auto i = node->children.begin();
		for(i;i!=node->children.end();i++){
			(*i)->accept(this);
			if(status != WALKING) break;
		}
		return nullptr;	
	}
	Node* visit(IfExpression* node){
		auto cond = node->condition->accept(this);
		if(status == FAILURE) return nullptr;
		if(cond->asBoolExpression()->value){
			if(walkEverywhere){
				node->alternative->accept(this);
				if(status == FAILURE) return nullptr;
			}
			return node->consequence->accept(this);
		}else {
			if(walkEverywhere){
				node->consequence->accept(this);
				if(status == FAILURE) return nullptr;
			}
			return node->alternative->accept(this);
		}
	}
	Node* visit(ControlFlowExpression* node){
		if(node->isBreak()) status = BREAK;
		else if(node->isContinue()) status = CONTINUE;
		return node;
	}
	Node* visit(LoopExpression* node){
		while(true){
			node->body->accept(this);
			if(status == CONTINUE){
				status = WALKING;
				continue;
			}
			if(status != WALKING) break;
		}
		if(status == BREAK) status = WALKING;
		return status == FAILURE ? nullptr : node;
	}

};

CTFEinvocation::CTFEinvocation(CompilationUnit* compilationUnit,Function* function) : _compilationUnit(compilationUnit),func(function) {
	if(!function->isFlagSet(Function::CANT_CTFE)){
		if(function->ctfeRegisterCount > 0) registers.resize(function->ctfeRegisterCount);
	}
}
CTFEinvocation::~CTFEinvocation(){
}
bool CTFEinvocation::invoke(Node* parameter){
	if(parameter)
		assert(parameter->isConst());

	if(func->isFlagSet(Function::CANT_CTFE)){
		return false;
	} 
	auto interpreter = _compilationUnit->interpreter;
	auto oldFunc = interpreter->currentFunction;
	auto oldRegisters = interpreter->registers;
	if(func->ctfeRegisterCount > 0){
		interpreter->registers = &registers[0];
	}
	interpreter->currentFunction = func;
	interpreter->interpretCall(func,parameter);
	auto success = interpreter->status != Interpreter::FAILURE;
	_result = success ? interpreter->returnValueRegister : interpreter->failureNode;

	interpreter->registers = oldRegisters;
	interpreter->currentFunction = oldFunc;
	interpreter->status = Interpreter::WALKING;
	return success;
}
Node* CTFEinvocation::getValue(const Variable* variable){
	return variable->_owner && variable->functionOwner() == func ? registers[variable->ctfeRegisterID] : nullptr;
}

/**
*	API for intrinsic function bindings.
*/
CTFEintrinsicInvocation::CTFEintrinsicInvocation(CompilationUnit* compilationUnit) : _compilationUnit(compilationUnit) {
}
bool CTFEintrinsicInvocation::invoke(Function* function,Node* parameter){
	assert(function->isIntrinsic() && function->intrinsicCTFEbinder);
	auto t = parameter->asTupleExpression();
	_params = t ? t->childrenPtr() : &parameter;
	function->intrinsicCTFEbinder(this);
	return true;
}
Node* CTFEintrinsicInvocation::result() const {
	return _result;
}

int      CTFEintrinsicInvocation::getInt32Parameter(uint16 id) const {
	return (int)_params[id]->asIntegerLiteral()->integer.u64;
}
uint32   CTFEintrinsicInvocation::getUint32Parameter(uint16 id) const {
	return (uint32)_params[id]->asIntegerLiteral()->integer.u64;
}
bool     CTFEintrinsicInvocation::getBoolParameter(uint16 id) const {
	return _params[id]->asBoolExpression()->value;
}
Type*    CTFEintrinsicInvocation::getTypeParameter(uint16 id) const {
	return _params[id]->asTypeReference()->type;
}
Node*    CTFEintrinsicInvocation::getNodeParameter(uint16 id) const {
	return _params[id]->asNodeReference()->node();
}
SymbolID CTFEintrinsicInvocation::getStringParameterAsSymbol(uint16 id) const {
	auto str = _params[id]->asStringLiteral();
	return SymbolID(str->block.ptr(),str->block.length());
}
Parser*  CTFEintrinsicInvocation::getParser() const {
	return _compilationUnit->parser;
}

void CTFEintrinsicInvocation::ret(){
	_result = new UnitExpression();
}
void CTFEintrinsicInvocation::ret(bool value ){
	_result = new BoolExpression(value);
}
void CTFEintrinsicInvocation::ret(Type* value){
	_result = new TypeReference(value);
}
void CTFEintrinsicInvocation::ret(Node* node ){
	_result = new NodeReference(node);
}
void CTFEintrinsicInvocation::ret(SymbolID symbolAsString){
	_result = new StringLiteral(symbolAsString);
}
void CTFEintrinsicInvocation::retNatural(size_t value){
	_result = new IntegerLiteral((uint64)value,intrinsics::types::natural);
}
void CTFEintrinsicInvocation::retNaturalNatural(size_t a,size_t b){
	_result = new TupleExpression(new IntegerLiteral((uint64)a,intrinsics::types::natural),new IntegerLiteral((uint64)b,intrinsics::types::natural));
}
void CTFEintrinsicInvocation::retError(const char* err){
	auto noderef = _params[0]->asNodeReference();
	compiler::onError(noderef? noderef->node():_params[0],err);
	_result = ErrorExpression::getInstance();
}

Interpreter* constructInterpreter(InterpreterSettings* settings){
	return new Interpreter;
}
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo){
	*currentNode = interpreter->failureNode;
	*extraInfo = interpreter->failureReason;
}
