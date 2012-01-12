#ifndef COMPLIER_H
#define COMPILER_H

struct Compiler {

	struct Unit {
		Parser* parser;
		const char* filename;

		ExpressionFactory expressions;

		void markCurrentBlockAsUnresolved();
	};
private:
	Unit unit;
public:

	inline Unit* currentUnit() { return &unit; }

	void compile(const char* name,const char* source);

	void onError(Location& location,std::string message);

	//testing
	void testParse(const char* str,Scope* scope,Expression* expected,const char* file,int line);
};

extern Compiler* compiler;

#endif