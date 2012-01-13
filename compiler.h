#ifndef COMPLIER_H
#define COMPILER_H

struct TranslationUnit {
	const char* filename;
	bool compile;
	ExpressionFactory expressionFactory;
};

namespace compiler {
	//settings

	extern Scope* scope;

	//types
	extern Type* expression;//any expression
	extern Type* type;      //a reference to type constant expression
	extern Type* Nothing;   //expressions returning nothing are statements //compiler::Nothing(a statement) and arpha::Nothing(void) are diffrenent!
	extern Type* Error;     //an error constant expression
	extern Type* Unresolved;//unresolved function overload
	extern Type* inferred;  //an inferred type

	void init();

	//
	void compile(const char* name,const char* source);

	void onError(Location& location,std::string message);

};


#endif