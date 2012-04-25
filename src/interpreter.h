#ifndef INTERPRETER_H
#define INTERPRETER_H

struct Interpreter {


	ExpressionFactory* expressionFactory;

	Node* interpret(Node* node);

	static void init(Scope* arphaScope);

};


#endif