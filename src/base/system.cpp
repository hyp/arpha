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

namespace System {
	namespace path {
		std::string directory(const char* filename){
			assert(filename);
			std::string path(filename);	
			size_t i = path.length();
			do {
				i--;
				if(path[i] == '/' || path[i] == '\\'){
					path = i ? path.substr(0,i) : "";
					break;
				}		
			}while(i != 0);
			return path;
		}	

		std::string filename(const char* path){
			assert(path);
			std::string filename(path);	
			size_t len = filename.length();
			size_t i = len;
			do {
				i--;
				if(filename[i] == '/' || filename[i] == '\\'){
					filename = (len-(i+1))!=0 ? filename.substr(i+1,len) : "";
					break;
				}		
			}while(i != 0);
			return filename;
		}

		std::pair<const char*,const char*> firstComponent(const char** path){
			assert(path);
			assert(*path);
			std::pair<const char*,const char*> result;
			const char* p = *path;
			result.first = p;
			for(;*p!='/' && *p!='\0';p++){}
			result.second = p;
			*path = p;
			return result;
		}
	}

	unittest(path){
		assert(path::directory("packages/arpha/arpha.arp") == "packages/arpha");
		assert(path::directory("/glasgow") == "");
		assert(path::directory("foo") == "foo");

		assert(path::filename("packages/arpha/arpha.arp") == "arpha.arp");
		assert(path::filename("/glasgow") == "glasgow");
		assert(path::filename("foo") == "foo");
		assert(path::filename("/washington/") == "");

		const char* name = "arpha/arpha.arp";
		auto component = path::firstComponent(&name);
		assert(std::string(component.first,size_t(component.second-component.first)) == "arpha");
		assert(std::string(name) == "/arpha.arp");
		name++;
		component = path::firstComponent(&name);
		assert(std::string(component.first,size_t(component.second-component.first)) == "arpha.arp");
		assert(*name == '\0');
	}
}


unittest(System){
	auto p = System::malloc(16);
	assert(p);
	System::free(p);
}