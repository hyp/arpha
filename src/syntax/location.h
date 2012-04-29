/**
* This module provides a datatype which describes the location in the source file.
*/
#ifndef ARPHA_LOCATION_H
#define ARPHA_LOCATION_H

struct Location {
public:
	int lineNumber;
	int column;

	inline int line(){ return lineNumber; }

	inline Location() : lineNumber(-1),column(-1) { 
	}
	inline Location(int line,int _column) : lineNumber(line), column(_column) {
	}
};

#endif