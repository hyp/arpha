/**
* This package implements A LLVM backend.
*/
#ifndef ARPHA_GEN_LLVM_H
#define ARPHA_GEN_LLVM_H

#include "../../data/data.h"
#include "../gen.h"

namespace gen {
	struct LLVMBackend: AbstractBackend {
		enum {
			OUTPUT_BC = data::gen::native::ModuleOutputFormat::ASSEMBLY + 1,
		};
		LLVMBackend(data::gen::native::Target* target,data::gen::Options* options);
		std::string generateModule(Node* root,const char* outputDirectory,const char* moduleName,int outputFormat = data::gen::native::ModuleOutputFormat::OBJECT);
	private:
		data::gen::Options* options;
		data::gen::native::Target* target;	
	};
};

#endif
