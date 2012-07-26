/**
* This package implements A Driver to the system's linker.
*/
#ifndef ARPHA_GEN_LINK_H
#define ARPHA_GEN_LINK_H

#include "../data/data.h"

namespace gen {


	struct Linker {
		Linker(data::gen::native::Target* target,data::gen::Options* options);

		std::string link(const char** files,size_t fileCount,const char* outputFile,data::gen::native::PackageLinkingFormat outputFormat);
	private:
		data::gen::native::Target* target;
		data::gen::Options* options;
	};

};

#endif
