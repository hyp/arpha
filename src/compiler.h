/**
* This module abstracts various compiler settings and functions.
*/
#ifndef ARPHA_COMPLIER_H
#define ARPHA_COMPILER_H


struct Scope;

#include "base/base.h"
#include "syntax/location.h"

struct Parser;
struct Resolver;
struct Interpreter;
struct Function;
struct Node;
struct BlockExpression;

//This structure contains the state of the module which is currently being compiled
struct CompilationUnit {
	Parser* parser;
	Resolver* resolver;
	Interpreter* interpreter;
	BlockExpression* moduleBody;

	int printingDecorationLevel;
};

namespace compiler {

	Scope* findModule(const char* name);

	//
	void compile(const char* name,const char* source);

	void registerResolvedIntrinsicModuleCallback(const char* name,void (* f)(Scope*));

	/// Reports a compilation error
	void onWarning(Location& location,const std::string& message);
	void onError(Location& location,const std::string& message);//a standalone error message
	void onError(Node* node,const std::string& message);
	void headError(Location& location,const std::string& message);
	void subError(Location& location,const std::string& message);
	// Corrupt intrinsic API, causes compiler exit
	void intrinsicFatalError(Location& location,const std::string& message);

	void onDebug(const std::string& message);

	enum {
		Silent = 0,
		ReportErrors,
		ReportDebug
	};
	extern int reportLevel; // = ReportDebug

	CompilationUnit *currentUnit();

	void addGeneratedExpression(Node* expr);

	extern BlockExpression* generatedFunctions;
};



#include "base/format.h"

// NB: Macroes are a necessary evil
#define warning(loc,...) compiler::onWarning(loc,format(__VA_ARGS__))
#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define debug(...) compiler::onDebug(format(__VA_ARGS__))

#endif