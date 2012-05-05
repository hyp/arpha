#ifndef COMPLIER_H
#define COMPILER_H



namespace compiler {
	//settings

	extern Scope* scope;

	//types


	void init();

	Scope* findModule(const char* name);

	//
	void compile(const char* name,const char* source);

	void onError(Location& location,std::string message);

};


#endif