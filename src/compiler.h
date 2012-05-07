/**
* This module abstracts various compiler settings and functions.
*/
#ifndef ARPHA_COMPLIER_H
#define ARPHA_COMPILER_H


struct Scope;

#include "base/base.h"
#include "syntax/location.h"

namespace compiler {
	//settings


	//types


	void init();

	Scope* findModule(const char* name);

	//
	void compile(const char* name,const char* source);

	/// Reports a compilation error
	void onError(Location& location,std::string message);

	//Target specific settings
	//4 or 8 bytes for 32 or 64 bits
	extern size_t wordSize,pointerSize;   
};

#include "base/format.h"
#include "base/system.h"

#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define debug(...) System::debugPrint(format(__VA_ARGS__))

#endif