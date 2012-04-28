#ifndef INTERPRETER_H
#define INTERPRETER_H

struct Interpreter {



	Node* interpret(Node* node);

	static void init(Scope* compilerScope,Scope* arphaScope);

};


#endif