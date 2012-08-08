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
	void print(const char* message);
	void debugPrint(const std::string& message);

	struct OutputBuffer {
		FILE* file;

		void print(const char* message);
		void print(const std::string& message);
	};

	OutputBuffer console();


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

		//Returns arp from a/b/c.arp. Can return null.
		const char* extension(const char* path);

		//Returns a from a/b/c.arp and advances the pointer to '/' or '\0'
		std::pair<const char*,const char*> firstComponent(const char** path);
	}
}

#endif