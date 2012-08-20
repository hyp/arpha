/**
* This package implements A LLVM backend.
*/
#ifndef ARPHA_GEN_LLVM_H
#define ARPHA_GEN_LLVM_H

#include "../../data/data.h"
#include "../gen.h"
#include "../dlldef.h"

namespace gen {
	struct LLVMBackend: AbstractBackend {
		enum {
			OUTPUT_BC = 0x4,
		};
		LLVMBackend(data::gen::native::Target* target,data::gen::Options* options,DllDefGenerator* dllGen);

		std::string generateModule(Node* root,const char* outputDirectory,const char* moduleName,int outputFormat = data::gen::native::OBJECT);
		std::string generateModule(Node** roots,size_t rootCount,const char* outputDirectory,const char* moduleName,int outputFormat = data::gen::native::OBJECT);
	private:
		data::gen::Options* options;
		data::gen::native::Target* target;
		DllDefGenerator* dllDefGenerator;
	};
};

#endif

