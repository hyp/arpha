#ifndef COMON_H
#define COMON_H


#include "base/base.h"
#include "base/symbol.h"
#include "base/format.h"
#include "base/system.h"
#include "base/location.h"


#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))
#define printf(...) System::print(format(__VA_ARGS__))
#define debug(...) System::debugPrint(format(__VA_ARGS__))

//to implement



#endif