#include "base.h"

void _assert(const char* file, int line, const char* what) {
	std::cout<<"Assertion failed: file: "<<file<<" line: "<<line<<'('<<what<<")!"<<std::endl;
}

namespace testing {
	Unittest::Unittest(const char* name, void (*func)()) {
		std::cout<<"Running unittest "<<name<<"... \n";
		func();
		std::cout<<"   success!\n";
	}
}
