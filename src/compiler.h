/**
* This module abstracts various compiler settings and functions.
*/
#ifndef ARPHA_COMPLIER_H
#define ARPHA_COMPILER_H


struct Scope;

#include "base/base.h"
#include "syntax/location.h"

struct Parser;
struct Evaluator;
struct Interpreter;
struct Function;

namespace compiler {
	//settings


	void init();

	Scope* findModule(const char* name);

	//
	void compile(const char* name,const char* source);

	void registerResolvedIntrinsicModuleCallback(const char* name,void (* f)(Scope*));

	/// Reports a compilation error
	void onError(Location& location,const std::string& message);
	void onDebug(const std::string& message);

	enum {
		Silent = 0,
		ReportErrors,
		ReportDebug
	};
	extern int reportLevel; // = ReportDebug
	//Target specific settings
	//4 or 8 bytes for 32 or 64 bits
	extern size_t wordSize,pointerSize;   

	//This structure contains the state of the module which is currently being compiled
	struct Unit {
		Parser* parser;
		Evaluator* evaluator;
		Interpreter* interpreter;

		//Current settings
		struct State {
			State();
			bool interpret; //Interpret constant function calls or not?
			int reportLevel;
			int decorationLevel;
		};
		
		void updateState(State& state);
		const State& state() const;
	private:
		State _state;
	};

	Unit *currentUnit();
};

#include "base/format.h"

#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define debug(...) compiler::onDebug(format(__VA_ARGS__))

#endif