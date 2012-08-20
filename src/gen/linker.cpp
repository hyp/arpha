#include "../base/base.h"
#include "../base/system.h"
#include "linker.h"

using namespace data::gen::native;

//TODO gcc driver
//TODO gcc(MinGW) driver for Windows

#ifdef _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <shlobj.h>
#include <shellapi.h>

#include "../base/utf.h"

std::string UTF16zToUTF8(WCHAR* b){
	std::string path;
	char buf[5];
	for(auto i = b; *i != 0;){
		auto s = UTF8::encode(UTF16::decode((const U16Char**)&i),(U8Char*)buf);
		buf[s] = 0;
		path+=buf;
	}
	return path;
}

struct VSLinker {
	std::string file;
	std::string executionDirectory;
	std::string libExeFile;
	std::string libDirectory;
	std::string sdkDirectory;
};

bool searchForVSInstallation(std::string& path,VSLinker& linker){
	std::string match = path + "\\Microsoft Visual Studio*";
	UTF16::StringBuffer u16path(match.c_str());

	WIN32_FIND_DATAW data;
	HANDLE h = FindFirstFileW(u16path, &data );
	do {
		if( data.dwFileAttributes | FILE_ATTRIBUTE_DIRECTORY ){
			std::string fdir = path + "\\" + UTF16zToUTF8(data.cFileName);
			if(System::fileExists((fdir + "\\VC\\bin\\link.exe").c_str()) &&
				System::directoryExists((fdir + "\\Common7\\IDE").c_str())){
					linker.file = fdir + "\\VC\\bin\\link.exe";
					linker.libExeFile = fdir + "\\VC\\bin\\lib.exe";
					linker.executionDirectory = fdir + "\\Common7\\IDE";//IS there aby better way for this???
					linker.libDirectory = fdir + "\\VC\\lib";
					linker.sdkDirectory = path + "\\Microsoft SDKs\\Windows\\v7.0A\\Lib";//TODO
					return true;
			}
		}
	} while (FindNextFileW(h, &data) != 0);
	return false;
}

static void findVSLinker(VSLinker& linker){
	//Search in program files
	WCHAR pf[MAX_PATH];
	SHGetFolderPathW(0,CSIDL_PROGRAM_FILES,nullptr,SHGFP_TYPE_CURRENT,pf);
	std::string path = UTF16zToUTF8(pf);
	if(!searchForVSInstallation(path,linker)){
		System::print(std::string("Couldn't find the Microsoft Visual Studio installation in ")+path);
	}
}

static void visualStudioLinkerDriver(Target* target,data::gen::Options* options,const char** files,size_t count,const char* output,PackageLinkingFormat outputFormat,const char* linkerOptions){
	VSLinker linker;
	findVSLinker(linker);
	if(linker.file.empty()){
		return;
	} else {
		System::print(std::string("Found the Microsoft Visual Studio linker - ")+linker.file);
	}

	std::stringstream cmd;
	if(linkerOptions) cmd<<linkerOptions<<" ";
	cmd<<" /OUT:\""<<output<<"\" /NOLOGO /ERRORREPORT:QUEUE ";
	for(auto endFiles = files+count;files!=endFiles;files++){
		cmd<<'"';
		cmd<<*files;
		cmd<<"\" ";
	}
	//target
	using namespace data::gen::native;
	const char* machine = "QUANTUM_COMPUTER";
	if(target->cpuArchitecture == Target::X86){
		if(target->cpuMode == Target::M32) machine = "X86";
		else machine = "X64";
	} else if(target->cpuArchitecture == Target::ARM){
		machine = "ARM";
	}
	cmd<<" /MACHINE:"<<machine;
	//
	if(outputFormat == LIBRARY){
		cmd<<" /SUBSYSTEM:CONSOLE";//LIST for debugging
	} 
	else if(!linkerOptions) {
		cmd<<" /NXCOMPAT /INCREMENTAL ";
		if(outputFormat == EXECUTABLE){
			cmd<<" /STACK:\"10000000\" /SUBSYSTEM:CONSOLE ";
		}
		else if(outputFormat == SHARED_LIBRARY){
			cmd<<" /DLL /SUBSYSTEM:WINDOWS ";
		}
	
		//link c lib
		if(!linker.libDirectory.empty()){
			if(target->cpuMode == Target::M64) linker.libDirectory+="\\amd64";
			cmd<<" /LIBPATH:\""<<linker.libDirectory<<"\" /DEFAULTLIB:libcmt";
		}
		else
			cmd<<" /NODEFAULTLIB /ENTRY:\"main\"";
		if(!linker.sdkDirectory.empty())
			cmd<<" /LIBPATH:\""<<linker.sdkDirectory<<"\"";
	}
	
	auto cmdString = cmd.str();
	System::print("\nRunning linker with: ");
	System::print(cmdString);
	System::print("\n\n");
	System::execute(outputFormat != LIBRARY ? linker.file.c_str() : linker.libExeFile.c_str(),cmdString.c_str(),linker.executionDirectory.c_str());
}


#endif


static void gccLinkerDriver(const char** files,size_t count,const char* output,data::gen::native::PackageLinkingFormat outputFormat,const char* linkerOptions){
	std::string gccPath = "gcc";
	//TODO
	std::stringstream cmd;
	cmd<<"-o \""<<output<<"\"";

	if(outputFormat == SHARED_LIBRARY){
		cmd<<" -shared";
	}

	//files
	for(auto endFiles = files+count;files!=endFiles;files++){
		cmd<<'"';
		cmd<<*files;
		cmd<<"\" ";
	}

	auto cmdString = cmd.str();
	System::print("\nRunning gcc linker with: ");
	System::print(cmdString);
	System::print("\n\n");
	System::execute(gccPath.c_str(),cmdString.c_str());
}

using namespace gen;

Linker::Linker(data::gen::native::Target* target,data::gen::Options* options){
	this->target  = target;
	this->options = options;
}
std::string Linker::link(const char** files,size_t fileCount,const char* outputFile,data::gen::native::PackageLinkingFormat outputFormat,const char* linkerOptions){
#ifdef _WIN32
	const char* extensions[] = { ".exe" , this->target->platform != data::gen::native::Target::WINDOWS_MINGW ? ".lib" : ".a" , ".dll" };
#else
	const char* extensions[] = { "" , ".a" , ".so" };
#endif
	auto ext = extensions[outputFormat];
	std::string fullOutputName  = outputFile;
	if(strlen(ext)){
		fullOutputName = fullOutputName + ext;
	}

#ifdef _WIN32
	if(this->target->platform != data::gen::native::Target::WINDOWS_MINGW)
		visualStudioLinkerDriver(target,options,files,fileCount,fullOutputName.c_str(),outputFormat,linkerOptions);
	else gccLinkerDriver(files,fileCount,fullOutputName.c_str(),outputFormat,linkerOptions);
#else
	gccLinkerDriver(files,fileCount,fullOutputName.c_str(),outputFormat,linkerOptions);
#endif
	return fullOutputName;
}
