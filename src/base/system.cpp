#include <stdlib.h>
#include "system.h"
#include "utf.h"

#ifdef  _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#undef max
#undef min
static UINT oldcp;
#endif


void System::init(){
	//Enable utf8 in console for windows
	#ifdef  _WIN32
		oldcp = GetConsoleOutputCP();
		SetConsoleOutputCP(CP_UTF8);
	#endif
}
void System::shutdown(){
	#ifdef  _WIN32
		SetConsoleOutputCP(oldcp);
	#endif
}

void* System::malloc(size_t size){
	return ::malloc(size);
}
void System::free(void* ptr){
	::free(ptr);
}

void System::print(const std::string& message){
	#ifdef  _WIN32
		wprintf(L"%S", message.c_str()); 
	#else
		std::cout<<message;
	#endif
}
void System::debugPrint(const std::string& message){
	#ifdef  _WIN32
		auto cstr = message.c_str();
		wprintf(L"Debug: %S\n", cstr); 
		UTF16::StringBuffer wstr(cstr);
		OutputDebugStringW(wstr);
		OutputDebugStringA("\n");
	#else
		std::cout<<"Debug: "<<message<<std::endl;
	#endif
}

bool System::fileExists(const char* filename){
	assert(filename);
	FILE* file = fopen(filename, "rb");
	if(!file) return false;
	fclose(file);
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
	auto p = System::malloc(16);
	assert(p);
	System::free(p);
}