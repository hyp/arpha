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

	Interpreter(InterpreterSettings* settings) {
	}
	enum {
		WALKING,
		RETURN, //unroll the stack till the function epilogue
		FAILURE //unroll the stack to base
	};
	int status;
	bool clearVars;
	bool walkEverywhere;

	bool failed() const { return status == FAILURE ; }

	void reset(){
		status = WALKING;
		clearVars = true;
		walkEverywhere = false;
	}

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
		if(node->variable->value){
			return node->variable->value;
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
		auto argsBegin = parameter->asTupleExpression() && f->arguments.size()>1 ? parameter->asTupleExpression()->children.begin()._Ptr : &(parameter);
		for(size_t i = 0;i<f->arguments.size();i++){
			f->arguments[i]->value = argsBegin[i];
		}
		//intepret body
		auto result = f->body.accept(this);
		//Reset arguments' values
		if(clearVars){
			for(size_t i = 0;i<f->arguments.size();i++){
				f->arguments[i]->value = nullptr;
			}
		}
		return result;
	}
	Node* visit(CallExpression* node){
		auto arg = node->arg->accept(this);
		auto obj = node->object->accept(this);
		if(status == FAILURE) return nullptr;
		if(auto func = obj->asFunctionReference()){
			if(auto f = func->function->constInterpreter){
				if(arg->isConst()) return f(func->function,arg);
			}
			return fail(node,"The function can't be interpreted!");
		}
		else return fail(node);
	}
	Node* visit(BlockExpression* node){
		Node* result = nullptr;
		Node* lastResult;
		auto i = node->children.begin();
		for(i;i!=node->children.end();i++){
			lastResult = (*i)->accept(this);
			if(status != WALKING) break;
		}
		if(status == RETURN) result = lastResult;
		//The topmost scope of a function
		bool topmost = node->scope->functionOwner() != node->scope->parent->functionOwner();
		if(topmost){
			if(status == RETURN) status = WALKING;//reset return status
			else if(status == WALKING) result = new UnitExpression();//return void when no return expressions are present
		}
		//Reset the values of variables inside this scope
		if(clearVars){
			for(auto i = node->scope->prefixDefinitions.begin();i != node->scope->prefixDefinitions.end();i++){
				if(auto v = dynamic_cast<Variable*>((*i).second)){
					v->value = nullptr;
				}
			}
		}
		return result;	
	}

	Node* evaluateIntegerMatch(IntegerLiteral* value,MatchExpression* node){
		
		//TODO
		for(auto i =node->cases.begin();i!=node->cases.end();i++){
			auto intLiteral = (*i).pattern->asIntegerLiteral();
			assert(intLiteral);
			if(value->integer == intLiteral->integer) return (*i).consequence;
		}
		return node;
	}
	Node* visit(MatchExpression* node){
		auto obj = node->object->accept(this);
		if(status == FAILURE) return nullptr;
		if(auto intLiteral = obj->asIntegerLiteral()){
			auto branch = evaluateIntegerMatch(intLiteral,node);
			return branch->accept(this);//TODO check if all cases can be interpreted..
		}
		return fail(node);
	}

	//...
	Node* visit(ErrorExpression* node){
		return fail(node);
	}
};

Interpreter* constructInterpreter(InterpreterSettings* settings){
	auto result = new Interpreter(settings);
	return result;
}
Node* interpretNode(Interpreter* interpreter,Node* node){
	interpreter->reset();
	return node->accept(interpreter);
}
bool interpretCheckFunctionCall(Interpreter* interpreter,Function* f,Node* parameter){
	interpreter->reset();
	interpreter->walkEverywhere = true;
	return interpreter->interpretCall(f,parameter) != nullptr;
}
Node* interpretFunctionCall(Interpreter* interpreter,Function* f,Node* parameter,bool clearVariables){
	interpreter->reset();
	interpreter->clearVars = clearVariables;
	return interpreter->interpretCall(f,parameter);
}
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo){
	*currentNode = interpreter->failureNode;
	*extraInfo = interpreter->failureReason;
}
