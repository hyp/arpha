#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../base/system.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "evaluate.h"
#include "interpret.h"
#include "../intrinsics/ast.h"
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
		auto result = node->value->accept(this);
		if(!walkEverywhere){
			returnValueRegister = result;
			status = RETURN;
		}
		return result;
	}
	Node* visit(VariableReference* node){
		if(node->variable->functionOwner() == currentFunction) return registers[node->variable->registerID];
		else return fail(node,"This variable is outside the function's scope!");
	}
	Node* visit(AssignmentExpression* node){
		auto obj = node->object;//NB: Don't interpret the object!
		auto val = node->value->accept(this);
		if(status == FAILURE) return nullptr;
		if(val->isConst()){
			if(auto var = obj->asVariableReference()){
				if(var->variable->functionOwner() == currentFunction){
					registers[var->variable->registerID] = val;
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
			node->copyProperties(tuple);
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
				registers[f->arguments[i]->registerID] = argsBegin[i];
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
			if(auto f = func->function->constInterpreter){
				if(arg->isConst()) return f(arg);
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
	Node* visit(WhileExpression* node){
		while(true){
			auto cond = node->condition->accept(this);
			if(status != WALKING) break;
			if(!cond->asBoolExpression()->value) break;
			node->body->accept(this);
			if(status != WALKING) break;
		}
		return status == FAILURE ? nullptr : node;
	}

	//...
	Node* visit(ErrorExpression* node){
		return fail(node);
	}
};

InterpreterInvocation::InterpreterInvocation(Interpreter* interpreter,Function* f,Node* parameter,bool visitAllBranches) : func(f) {
	auto oldFunc = interpreter->currentFunction;
	auto oldRegisters = interpreter->registers;
	size_t numRegs = f->ctfeRegisterCount;
	if(numRegs > 0){
		registers.resize(numRegs);
		interpreter->registers = &registers[0];
	}
	interpreter->currentFunction = f;
	interpreter->walkEverywhere = visitAllBranches;
	interpreter->interpretCall(f,parameter);
	_succeded = interpreter->status != Interpreter::FAILURE;
	_result = interpreter->returnValueRegister;

	interpreter->registers = oldRegisters;
	interpreter->currentFunction = oldFunc;
	interpreter->status = Interpreter::WALKING;
}
InterpreterInvocation::~InterpreterInvocation(){
}
Node* InterpreterInvocation::getValue(const Variable* variable){
	return variable->functionOwner() == func ? registers[variable->registerID] : nullptr;
}

Interpreter* constructInterpreter(InterpreterSettings* settings){
	return new Interpreter;
}
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo){
	*currentNode = interpreter->failureNode;
	*extraInfo = interpreter->failureReason;
}
