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
void System::print(const char* message){
	#ifdef  _WIN32
		wprintf(L"%S", message); 
	#else
		std::cout<<message;
	#endif
}
System::OutputBuffer System::console(){
	OutputBuffer con = { stdout };
	return con;
}
void System::OutputBuffer::print(const char* message){
	#ifdef  _WIN32
		fwprintf(file,L"%S", message); 
	#else
		fprintf(file,"%s",message);
	#endif
}
void System::OutputBuffer::print(const std::string& message){
	#ifdef  _WIN32
		fwprintf(file,L"%S", message.c_str()); 
	#else
		fprintf(file,"%s",message.c_str());
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

const char* System::currentDirectory(){
	return "";
}
bool System::fileExists(const char* filename){
	assert(filename);
	#ifdef  _WIN32
		UTF16::StringBuffer wfile(filename);
		DWORD dwAttrib = GetFileAttributesW(wfile);

		return (dwAttrib != INVALID_FILE_ATTRIBUTES && !(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
	#else
		FILE* file = fopen(filename, "rb");
		if(!file) return false;
		fclose(file);
		return true;
	#endif
}
bool System::directoryExists(const char* filename){
	assert(filename);
	#ifdef  _WIN32
		UTF16::StringBuffer wfile(filename);
		DWORD dwAttrib = GetFileAttributesW(wfile);

		return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
	#else
		//TODO
		return false;
	#endif
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
FILE* System::open(const char* filename,bool write){
	assert(filename);
	FILE* file = fopen(filename, write? "w" : "r");
	assert(file);
	return file;
}

int System::execute(const char* file,const char* param,const char* dir){
	#ifdef  _WIN32
		UTF16::StringBuffer wfile(file);
		UTF16::StringBuffer wparam(param);
		UTF16::StringBuffer wdir(dir?dir:"");


		//ShellExecuteW((HWND)0,L"open",wfile,wparam,dir?wdir:"",SW_SHOW);

		STARTUPINFOW si;
		ZeroMemory( &si, sizeof(si) );
		si.cb = sizeof(STARTUPINFOW);
		si.hStdError  = GetStdHandle(STD_OUTPUT_HANDLE);//g_hChildStd_OUT_Wr;
		si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);//g_hChildStd_OUT_Wr;
		si.hStdInput  = GetStdHandle(STD_INPUT_HANDLE);
		si.dwFlags |= STARTF_USESTDHANDLES;
		PROCESS_INFORMATION pi;
		ZeroMemory( &pi, sizeof(pi) );
		if(!CreateProcessW(wfile,(LPWSTR)(LPCWSTR)wparam,nullptr,nullptr,
			false,0,nullptr,dir?wdir.operator const U16Char *():nullptr,&si,&pi)){
				print("Couldn't execute process!");
		}

		// Wait until child process exits.
		WaitForSingleObject( pi.hProcess, INFINITE );

		// Close process and thread handles. 
		CloseHandle( pi.hProcess );
		CloseHandle( pi.hThread );


		return 0;
	#else
		//TODO
		return system(command);
	#endif
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

		const char* extension(const char* path){
			for(auto i = path + (strlen(path));i != path;--i){
				if(*i == '.') return (i+1);
			}
			return "";
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

		assert(std::string("arp") == path::extension("packages/arpha/arpha.arp"));
		assert(std::string("") == path::extension("/glasgow"));
		assert(std::string("") == path::extension("ny."));

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