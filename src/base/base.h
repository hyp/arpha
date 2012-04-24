/**
* Base module contains the necessary macroes and types.
*/
#ifndef ARPHA_BASE_H
#define ARPHA_BASE_H

#include <vector>
#include <new>
#include <string>
#include <map>
#include <exception>
#include <sstream>
#include <iostream>

//Common types
typedef __int64 int64;
typedef unsigned __int64 uint64;
typedef unsigned int uint32;
typedef unsigned char uint8;

//Assertion macro
void _assert(const char* file, int line, const char* what);
#define assert(what) ((what) ? (void)0 : _assert( __FILE__, __LINE__, (#what)))

//Unit testing macro
namespace testing {

	struct Unittest {
		Unittest(const char* name,void (*func)());
	};	
}
#define unittest(name) void __unittest__##name(); testing::Unittest __Utest__##name(#name,& __unittest__##name); void __unittest__##name()

#endif