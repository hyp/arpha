#include "../compiler.h"
#include "../ast/node.h"
#include "../ast/declarations.h"
#include "dlldef.h"

namespace gen {

void DllDefGenerator::addStdCall(Function* func,size_t paramSize){
	if(!libs.size()) libs.push_back("user32.dll");
	Declaration decl = { func,0,(uint32)paramSize };
	declarations.push_back(decl);
}
void DllDefGenerator::add       (Function* func){
}
void genDecl(FILE* file,DllDefGenerator::Declaration& decl){
	fprintf(file,"_%s@%d\n",decl.function->label().ptr(),decl.stdCallParamSize);
}
void DllDefGenerator::gen(const char* outputDirectory){
	if(!declarations.size()) return;

	FILE* file;
	uint32 libID = 0;
	for(auto i = libs.begin();i!=libs.end();i++,libID++){
		auto libname  = (*i);
		generatedDefFiles.push_back(std::string(outputDirectory)+libname+".def");
		file = System::open(generatedDefFiles.back().c_str(),true);
		if(!file){
			generatedDefFiles.pop_back();
			continue;
		}
		fprintf(file,"LIBRARY %s\nEXPORTS\n",libname);
		for(auto decl = declarations.begin();decl != declarations.end();decl++){
			if(libID == decl->libID) genDecl(file,*decl);
		}
		fclose(file);
	}
	libs.clear();
	declarations.clear();
}

}
