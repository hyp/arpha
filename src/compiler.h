#ifndef COMPLIER_H
#define COMPILER_H



namespace compiler {
	//settings

	extern Scope* scope;

	//types
	extern Type* expression;  //any expression
	extern Type* type;        //a reference to type constant expression
	extern Type* Nothing;     //expressions returning nothing are statements //compiler::Nothing(a statement) and arpha::Nothing(void) are diffrenent!
	extern Type* Unresolved;  //unresolved function overload | variable with unresolved type
	extern Type* anyType;     //a type which represents any given type


	void init();

	Scope* findModule(const char* name);

	//
	void compile(const char* name,const char* source);

	void onError(Location& location,std::string message);

};


#endif