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
* Walk the body of a function and assign a stack offset for each variable
* e.g for f(a) { var b ; { var c } } = {a:0,b:1,c:2}
*/
struct ValueRestorer : NodeVisitor {
	size_t offset;
	std::vector<Node*>* oldValues;

	ValueRestorer(size_t o,std::vector<Node*>* v) : offset(o),oldValues(v) {}
	//TODO more nodes
	Node* visit(IfExpression* node){
		node->consequence->accept(this);
		node->alternative->accept(this);
		return node;
	}
	Node* visit(WhileExpression* node){
		node->body->accept(this);
		return node;
	}
	Node* visit(BlockExpression* node){
		for(auto i = node->scope->prefixDefinitions.begin();i != node->scope->prefixDefinitions.end();i++){
			if(auto v = dynamic_cast<Variable*>((*i).second)){
				if(!dynamic_cast<Argument*>((*i).second)){
					v->value = (*oldValues)[offset];offset++;
				}
			}
		}
		for(auto i = node->children.begin();i!=node->children.end();i++){
			(*i)->accept(this);
		}
		return node;
	}


};

/**
* Walks the AST node and checks if the node can be interpreted at compile-time.
* This checks all expressions in the AST node and doesn't account for branches that won't be reached when interpreting
*/
struct Interpreter : NodeVisitor {
	std::vector<Node*>* oldValues;


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
		if(!walkEverywhere) status = RETURN;
		return result;
	}
	Node* visit(VariableReference* node){
		if(auto v =node->variable->value){
			return v;
		}
		else return fail(node);
	}
	Node* visit(AssignmentExpression* node){
		auto obj = node->object;//NB: Don't interpret the object!
		auto val = node->value->accept(this);
		if(status == FAILURE) return nullptr;
		if(val->isConst()){
			if(auto var = obj->asVariableReference()){
				var->variable->value = val;
				return val;
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
				oldValues->push_back(f->arguments[i]->value);
				f->arguments[i]->value = argsBegin[i];
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
		//The topmost scope of a function
		bool topmost = node->scope->functionOwner() != node->scope->parent->functionOwner();

		for(auto i = node->scope->prefixDefinitions.begin();i != node->scope->prefixDefinitions.end();i++){
			if(auto v = dynamic_cast<Variable*>((*i).second)){
				if(!topmost || !v->asArgument()) oldValues->push_back(v->value);
			}
		}

		Node* result = nullptr;
		Node* lastResult;
		auto i = node->children.begin();
		for(i;i!=node->children.end();i++){
			lastResult = (*i)->accept(this);
			if(status != WALKING) break;
		}
		if(status == RETURN) result = lastResult;

		if(topmost){
			if(status == RETURN) status = WALKING;//reset return status
			else if(status == WALKING) result = new UnitExpression();//return void when no return expressions are present
		}
		return result;	
	}
	Node* visit(IfExpression* node){
		auto cond = node->condition->accept(this);
		if(status == FAILURE) return nullptr;
		if(!cond->asIntegerLiteral()->integer.isZero()){
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
			if(auto i =cond->asIntegerLiteral()){
				if(i->integer.isZero()) break;
			}
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

InterpreterInvocation::InterpreterInvocation() {}
Node* InterpreterInvocation::interpret(Interpreter* interpreter,Function* f,Node* parameter,bool visitAllBranches){
	func = f;
	interpreter->oldValues = &oldValues;
	interpreter->walkEverywhere = visitAllBranches;
	auto result = interpreter->interpretCall(f,parameter);
	interpreter->status = Interpreter::WALKING;
	return result;
}
InterpreterInvocation::~InterpreterInvocation(){
	size_t i;
	for(i = 0;i<func->arguments.size();i++){
		func->arguments[i]->value = oldValues[i];
	}
	ValueRestorer restorer(i,&oldValues);
	func->body.accept(&restorer);
}
Node* InterpreterInvocation::getValue(const Variable* variable){
	return variable->value;
}

Interpreter* constructInterpreter(InterpreterSettings* settings){
	return new Interpreter;
}
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo){
	*currentNode = interpreter->failureNode;
	*extraInfo = interpreter->failureReason;
}
