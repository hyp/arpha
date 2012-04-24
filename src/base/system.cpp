#include <stdlib.h>
#include "system.h"
#include "utf.h"

#ifdef  _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#undef max
#undef min
#endif

void* System::malloc(size_t size){
	return ::malloc(size);
}
void System::free(void* ptr){
	::free(ptr);
}

void System::debugPrint(const std::string& message){
	std::cout<<"Debug: "<<message<<std::endl;
	#ifdef  _WIN32
		UTF16::StringBuffer wstr(message.c_str());
		OutputDebugStringW(wstr);
	#endif
}

bool System::fileExists(const char* filename){
	assert(filename);
	return true;
}

const char* System::fileToString(const char* filename){
	assert(filename);
	FILE* file = fopen(filename, "rb");
	assert(file);
	fseek(file, 0, SEEK_END);
	size_t size = (size_t) ftell(file);
	rewind(file);
	void* data = malloc(size+1);
	assert(data);
	fread(data, size, 1, file);
	((char*)data)[size]='\0';
	fclose(file);
	return (const char*)data;
}

unittest(System){
	//Nothing to test
}