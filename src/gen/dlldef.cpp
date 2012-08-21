#include <set>
#include "../compiler.h"
#include "../ast/node.h"
#include "../ast/declarations.h"
#include "linker.h"
#include "dlldef.h"

namespace gen {

void DllDefGenerator::addStdCall(Function* func,size_t paramSize){
	Declaration decl = { func,(uint32)paramSize };
	declarations.push_back(decl);
}
void DllDefGenerator::add       (Function* func){
	Declaration decl = { func,0 };
	declarations.push_back(decl);
}

namespace coff {
	struct Header {
	enum Machine {
		IMAGE_FILE_MACHINE_UNKNOWN = 0x0,
		IMAGE_FILE_MACHINE_AMD64 = 0x8664,
		IMAGE_FILE_MACHINE_ARM =0x1c0,
		IMAGE_FILE_MACHINE_ARMV7 = 0x1c4,
		IMAGE_FILE_MACHINE_I386 = 0x14c,
		IMAGE_FILE_MACHINE_THUMB = 0x1c2,
	};
	enum Characteristics {
		IMAGE_FILE_RELOCS_STRIPPED = 0x1,
		IMAGE_FILE_EXECUTABLE_IMAGE = 0x2,
		IMAGE_FILE_LARGE_ADDRESS_AWARE = 0x20,
		IMAGE_FILE_32BIT_MACHINE = 0x100,
		IMAGE_FILE_DLL = 0x2000
	};
		uint16 machine;
		uint16 numberOfSections;
		uint32 timestamp;
		uint32 ptrSymbolTable;
		uint32 numberOfSymbols;
		uint16 optHeaderSize;
		uint16 characteristics;
	};

	struct Section {
		char name[8];
		uint32 virtualSize;
		uint32 virtualAddress;
		uint32 sizeOfRawData;
		uint32 pointerToRawData;
		uint32 pointerToRelocations;
		uint32 pointerToLinenumbers;
		uint16 numberOfRelocations;
		uint16 numberOfLinenumbers;
		uint32 characteristics;

	enum Characteristrics{
		IMAGE_SCN_CNT_CODE = 0x20,
		IMAGE_SCN_CNT_INITIALIZED_DATA = 0x40,
		IMAGE_SCN_CNT_UNINITIALIZED_DATA = 0x80,
		IMAGE_SCN_LNK_REMOVE = 0x800,
		IMAGE_SCN_GPREL = 0x8000,
		IMAGE_SCN_MEM_16BIT = 0x20000,
		IMAGE_SCN_MEM_SHARED = 0x10000000,
		IMAGE_SCN_MEM_EXECUTE = 0x20000000,
		IMAGE_SCN_MEM_READ = 0x40000000,
		IMAGE_SCN_MEM_WRITE = 0x80000000,
	};
	};

	struct Symbol {
		char name[8];
		uint32 value;
		uint16 sectionNumber;
		uint16 type;
		uint8  storageClass;
		uint8  numberOfAuxSymbols;

		enum {
			IMAGE_SYM_UNDEFINED = 0,
			IMAGE_SYM_ABSOLUTE = -1,
		};
		enum {
			IMAGE_SYM_CLASS_NULL = 0,
			IMAGE_SYM_CLASS_AUTOMATIC = 1,
			IMAGE_SYM_CLASS_EXTERNAL = 2,
			IMAGE_SYM_CLASS_STATIC = 3,
			IMAGE_SYM_CLASS_EXTERNAL_DEF = 5,
			IMAGE_SYM_CLASS_LABEL = 6,
			IMAGE_SYM_CLASS_UNDEFINED_LABEL = 8,

		};
	};
}

void dummyHeader(FILE* file,uint32 declarations,data::gen::native::Target* target){
	using namespace coff;

	Header header;
	memset(&header,0,sizeof(header));
	switch(target->cpuArchitecture){
	case data::gen::native::Target::X86:
		if(target->cpuMode == data::gen::native::Target::M32){
			header.machine = Header::IMAGE_FILE_MACHINE_I386;
			header.characteristics = Header::IMAGE_FILE_32BIT_MACHINE;
		}
		else header.machine = Header::IMAGE_FILE_MACHINE_AMD64;
		break;
	case data::gen::native::Target::ARM:
		header.machine = Header::IMAGE_FILE_MACHINE_ARM;
		header.characteristics = Header::IMAGE_FILE_32BIT_MACHINE;
		break;
	default:
		assert(false);
	}
	header.numberOfSections = 1;
	header.timestamp = 968958695;
	header.ptrSymbolTable = 0x3C;
	header.numberOfSymbols = 1 + declarations;
	Section section;
	memset(&section,0,sizeof(section));
	strcpy(section.name,".text");
	section.pointerToRawData = 0x3C;
	section.characteristics = Section::IMAGE_SCN_MEM_EXECUTE | Section::IMAGE_SCN_MEM_READ | Section::IMAGE_SCN_CNT_CODE | 0x300000;
	fwrite(&header,20,1,file);
	fwrite(&section,40,1,file);		
	Symbol symbol;
	memset(&symbol,0,sizeof(symbol));
	strcpy(symbol.name,".text");
	symbol.sectionNumber = 1;
	symbol.storageClass = Symbol::IMAGE_SYM_CLASS_STATIC;
	fwrite(&symbol,18,1,file);
}
void dummyDecl(FILE* file,DllDefGenerator::Declaration& decl,std::vector<char>& stringTable){
	using namespace coff;
	Symbol symbol;
	memset(&symbol,0,sizeof(symbol));

	std::stringstream mangler;
	mangler<<"_"<<decl.function->label().ptr()<<'@'<<decl.stdCallParamSize;
	auto str = mangler.str();
	if(str.size() <= 8){
		memcpy(symbol.name,str.c_str(),str.size());
	} else {
		*(uint32*)(symbol.name + 4) = (uint32)(4 + stringTable.size());
		for(auto c = str.begin();c!=str.end();++c) stringTable.push_back(*c);
		stringTable.push_back('\0');
	}

	symbol.sectionNumber = 1;
	symbol.storageClass  = Symbol::IMAGE_SYM_CLASS_EXTERNAL;
	fwrite(&symbol,18,1,file);
}
void dummyStringTable(FILE* file,std::vector<char> stringTable){
	uint32 length = sizeof(length) + stringTable.size();
	fwrite(&length,sizeof(length),1,file);
	if(stringTable.size()) fwrite(&stringTable[0],stringTable.size(),1,file);
}
void genDecl(FILE* file,DllDefGenerator::Declaration& decl){
	auto func = decl.function;
	fprintf(file,"%s\n",decl.function->label().ptr());
}
void genDeclM64(FILE* file,DllDefGenerator::Declaration& decl){
	auto func = decl.function;
	fprintf(file,"%s= _%s@%d\n",decl.function->label().ptr(),decl.function->label().ptr(),decl.stdCallParamSize);
}

void DllDefGenerator::gen(const char* outputDirectory,data::gen::native::Target* target,gen::Linker* linker,std::vector<std::string>& objectFiles){
	if(!declarations.size()) return;
	std::set<std::string> libs;
	uint32 libID = 0;
	for(auto decl = declarations.begin();decl != declarations.end();decl++){
		if(auto ext = System::path::extension(decl->function->externalLib)){
			assert(!strcmp(ext,"dll"));
			auto len = strlen(decl->function->externalLib) - strlen(ext) - 1;
			assert(len);
			const_cast<char*>(decl->function->externalLib)[len] = '\0';
		}
		libs.insert(decl->function->externalLib);
	}
	bool m64= target->cpuMode == data::gen::native::Target::M64;

	FILE* file;

	uint32 symbols;
	std::vector<char> stringTable;
	for(auto i = libs.begin();i!=libs.end();i++,libID++){

		symbols = 0;

		auto libname  = (*i).c_str();
		std::string defFile = std::string(outputDirectory)+libname+".def";
		file = System::open(defFile.c_str(),true);
		if(!file){
			continue;
		}
		fprintf(file,"LIBRARY %s\nEXPORTS\n",libname);
		for(auto decl = declarations.begin();decl != declarations.end();decl++){
			if(!strcmp(libname,decl->function->externalLib)){
				if(m64) genDeclM64(file,*decl);
				else genDecl(file,*decl);
				symbols++;
			}
		}
		fclose(file);

		//Dummy obj
		std::string objFile = std::string(outputDirectory)+libname+".obj";
		file = System::open(objFile.c_str(),true,true);
		if(!file){
			continue;
		}
		dummyHeader(file,symbols,target);

		for(auto decl = declarations.begin();decl != declarations.end();decl++){
			if(!strcmp(libname,decl->function->externalLib)){
				dummyDecl(file,*decl,stringTable);
			}
		}
		dummyStringTable(file,stringTable);
		stringTable.clear();
		fclose(file);

		const char* files[] = { objFile.c_str() };
		std::string output = std::string(outputDirectory)+libname;
		std::string opts = std::string(" /DLL /NOENTRY /DEF:\"") + defFile + "\" /SUBSYSTEM:WINDOWS ";
		linker->link(files,1,output.c_str(),data::gen::native::SHARED_LIBRARY,opts.c_str());
		objectFiles.push_back(std::string(outputDirectory)+libname+".lib");
	}
	declarations.clear();
}

}
