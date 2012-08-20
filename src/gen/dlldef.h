/**
* This module generates '.def' files for dll imports/exports.
*/
#ifndef ARPHA_GEN_DLLDEF_H
#define ARPHA_GEN_DLLDEF_H

namespace gen {

struct DllDefGenerator {
	struct Declaration {
		Function* function;
		uint32 libID;
		uint32 stdCallParamSize;
	};

	void addStdCall(Function* func,size_t paramSize);
	void add       (Function* func);

	void gen(const char* outputDirectory);
private:
	std::vector<Declaration> declarations;
	std::vector<const char*> libs;
public:
	std::vector<std::string> generatedDefFiles;
};

}

#endif