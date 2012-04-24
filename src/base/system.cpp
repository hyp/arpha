#include "system.h"

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