#ifndef COMON_H
#define COMON_H

#include<stdio.h>
#include<string.h>
#include<math.h>

#include "base/base.h"
#include "base/symbol.h"
#include "base/format.h"
#include "base/system.h"


#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define printf(...) System::print(format(__VA_ARGS__))
#define debug(...) System::debugPrint(format(__VA_ARGS__))

//to implement
struct Location {
private:
	int lineNumber;
	
public:
	int column;

	inline int line(){ return lineNumber; }
	inline Location(){ lineNumber = -1;column =-1;}
	Location(int line,int column);
};


#endif