/**
* This module abstracts various compiler settings and functions.
*/
#ifndef ARPHA_COMPLIER_H
#define ARPHA_COMPILER_H


struct Scope;

#include "base/base.h"
#include "base/system.h"
#include "base/symbol.h"
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
};

// Dumps AST to console/file
struct Dumper {
private:
	System::OutputBuffer destination;
	std::ostream* stream;
	int indentation;
	enum {
		IS_STREAM = 0x1
	};
public:	
	uint32 flags;
	enum {
		VERBOSE = 0x2,
		REF_DECL_POINTERS = 0x4,
	};
	Dumper() : flags(0) { }
	Dumper(std::ostream* stream);

	void print(const char* str);
	inline void print(SymbolID symbol)  { print(symbol.ptr()); }
	inline void print(std::string &str) { print(str.c_str()); }

	//varios decorators
	inline bool isVerbose() { return (flags & VERBOSE) != 0; }
	inline bool refDeclPointers(){ return (flags & REF_DECL_POINTERS) != 0; }

	void printIndentation();
	void incIndentation();
	void decIndentation();

	static Dumper console();
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
	void onAmbiguosDeclarationError(Node* declaration);
	// Corrupt intrinsic API, causes compiler exit
	void intrinsicFatalError(Location& location,const std::string& message);

	void onDebug(const std::string& message);

	void dumpModule(Node* module);

	enum {
		Silent = 0,
		ReportErrors,
		ReportDebug
	};
	extern int reportLevel; // = ReportDebug

	CompilationUnit *currentUnit();

	void addGeneratedExpression(Node* expr);
	void addFunctionToTestsuite(Function* function);


	extern BlockExpression* generatedFunctions;
};



#include "base/format.h"

// NB: Macroes are a necessary evil
#define warning(loc,...) compiler::onWarning(loc,format(__VA_ARGS__))
#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define debug(...) compiler::onDebug(format(__VA_ARGS__))

#endif