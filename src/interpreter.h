#ifndef INTERPRETER_H
#define INTERPRETER_H

struct Interpreter {


	ExpressionFactory* expressionFactory;

	Node* interpret(Node* node);

	static void init(Scope* compilerScope,Scope* arphaScope);

};


#endif