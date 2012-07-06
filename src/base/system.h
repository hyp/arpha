/**
*  Provides abstraction over various system operations.
*/
#ifndef ARPHA_SYSTEM_H
#define ARPHA_SYSTEM_H

#include "base.h"

namespace System {
	//
	void init();
	void shutdown();

	//Console io
	void print(const std::string& message);
	void debugPrint(const std::string& message);

	//Heap allocation
	void* malloc(size_t size);
	void free(void* ptr);

	//Filesystem
	const char* currentDirectory();
    bool fileExists(const char* filename);
	bool directoryExists(const char* filename);
	const char* fileToString(const char* filename);

	//exe
	int execute(const char* file,const char* param,const char* dir = nullptr);

	//path
	namespace path {
		//Returns a/b from a/b/c.arp
		std::string directory(const char* filename);
		//Returns c.arp from a/b/c.arp
		std::string filename(const char* path);
		//Returns a from a/b/c.arp and advances the pointer to '/' or '\0'
		std::pair<const char*,const char*> firstComponent(const char** path);
	}
}

#endif