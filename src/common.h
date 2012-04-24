#ifndef COMON_H
#define COMON_H

#include<stdio.h>
#include<string.h>
#include<math.h>
#include <time.h>

#include "base/base.h"
#include "base/symbol.h"
#include "base/format.h"



void printFunc(std::string message);
void debugPrint(std::string message);

#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define printf(...) printFunc(format(__VA_ARGS__))
#define debug(...) debugPrint(format(__VA_ARGS__))

const char* readFile(const char* filename);

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

void errorFunc(Location& location,std::string message);

#endif