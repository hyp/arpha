#include "base.h"
#include "location.h"

Location::Location(int line,int column){
	lineNumber = line;
	this->column = column;
}

unittest(Location){
	auto loc = Location(2,3);
	assert(loc.line() == 2);
	assert(loc.column == 3);
}