#include "format.h"

std::string format(const char *s){
	std::ostringstream stm;
    while (*s) {
        if (*s == '%' && *(++s) != '%')
            throw std::runtime_error("invalid format string: missing arguments");
		stm << *s++;
    }
	return stm.str();
}

unittest(format) {
	assert(format("Hello world") == "Hello world");
	assert(format("A %d %s C %c B",12,"KIAI",'_') == "A 12 KIAI C _ B");
	assert(format("Asc%s","ii") == "Ascii");
}
