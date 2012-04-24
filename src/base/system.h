/**
*  Provides abstraction over various system operations.
*/
#ifndef ARPHA_SYSTEM_H
#define ARPHA_SYSTEM_H

#include "base.h"

namespace System {
	//Debugging
	void debugPrint(const std::string& message);

	//Heap allocation
	void* malloc(size_t size);
	void free(void* ptr);

	//Filesystem
	const char* currentDirectory();
    bool fileExists(const char* filename);
	const char* fileToString(const char* filename);
}

#endif