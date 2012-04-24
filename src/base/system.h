/**
*  Provides abstraction over various system operations.
*/
#ifndef ARPHA_SYSTEM_H
#define ARPHA_SYSTEM_H

#include "base.h"

namespace System {
    bool fileExists(const char* filename);
	const char* fileToString(const char* filename);
}

#endif