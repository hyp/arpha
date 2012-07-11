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

struct CompilationUnit;
struct Parser;
struct Interpreter;

struct CTFEinvocation {

	CTFEinvocation(CompilationUnit* compilationUnit,Function* function);
	~CTFEinvocation();

	bool invoke(Node* parameter);

	Node* getValue(const Variable* variable); //returns null, if this variable isn't expanded
	inline Node* result() { return _result; }

private:
	Node* _result;
	std::vector<Node*> registers;
	Function* func;
	CompilationUnit* _compilationUnit;
};

struct CTFEintrinsicInvocation {
	CTFEintrinsicInvocation(CompilationUnit* compilationUnit);

	bool  invoke(Function* function,Node* parameter);
	Node* result() const;

	//API For intrinsic bindings
	bool     getBoolParameter(uint16 id) const;
	Type*    getTypeParameter(uint16 id) const;
	Node*    getNodeParameter(uint16 id) const;
	SymbolID getStringParameterAsSymbol(uint16 id) const;
	int      getInt32Parameter(uint16 id)  const;
	uint32   getUint32Parameter(uint16 id) const;
	
	Parser* getParser() const; //for parser api

	void ret();
	void ret(bool value );
	void ret(Type* value);
	void ret(Node* node );
	void ret(SymbolID symbolAsString);
	void retNatural(size_t value);
	void retNaturalNatural(size_t a,size_t b);
	void retError(const char* err);
private:
	CompilationUnit* _compilationUnit;
	Node** _params;
	Node* _result;
};

//
Interpreter* constructInterpreter(InterpreterSettings* settings);
void getFailureInfo(const Interpreter* interpreter,Node** currentNode,const char** extraInfo);


#endif
