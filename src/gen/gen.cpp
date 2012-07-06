#include "../base/base.h"
#include "stdio.h"
#include "gen.h"

using namespace gen;

void AbstractBackend::onError     (std::string& str){
	printf(str.c_str());//TODO fix
}
void AbstractBackend::onFatalError(std::string& str){
	onFatalError(str.c_str());
}
void AbstractBackend::onFatalError(const char*  str){
	printf(str);
}
