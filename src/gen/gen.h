/**
* This module defines some common definitions for specific generator implementation.
*/
#ifndef ARPHA_GEN_H
#define ARPHA_GEN_H

namespace gen {

	struct AbstractBackend {
		void onError     (std::string& str);
		void onFatalError(std::string& str);
		void onFatalError(const char*  str);
	};

	struct Linker;

};

#endif
