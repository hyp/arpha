#include "../base/base.h"
#include "../base/system.h"
#include "gen.h"

using namespace gen;

void AbstractBackend::onError     (std::string& str){
	System::print(str);
}
void AbstractBackend::onFatalError(std::string& str){
	System::print(str);
}
void AbstractBackend::onFatalError(const char*  str){
	System::print(str);
}
