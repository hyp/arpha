/**
* This module implemements AST stack interpreter.
* It tries to evaluate function calls with constant arguments at compile time.

* Note: Try to use C style in this module for function declarations for future cross language interaction;
*/
#ifndef ARPHA_AST_INTERPRET_H
#define ARPHA_AST_INTERPRET_H

struct InterpreterSettings {
	size_t heapSize;
	size_t timeLimit;       //Maximum time(in ms) for a single interpreter invocation
	size_t instructionLimit;//Maximum number of instructions for a single interpreter invocation
};

struct Interpreter;

struct InterpreterInvocation {

	InterpreterInvocation();
	Node* interpret(Interpreter* interpreter,Function* f,Node* parameter,bool visitAllBranches = false);
	~InterpreterInvocation();
	Node* getValue(const Variable* variable);
private:
	std::vector<Node*> oldValues;
	Function* func;
};

//
Interpreter* constructInterpreter(InterpreterSettings* settings);
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo);


#endif
