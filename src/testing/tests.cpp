/**
* This module contains all the non base unittests.
*/
#include "../base/base.h"
#include "../base/format.h"
#include "../base/memory.h"
#include "../base/system.h"
#include "../base/symbol.h"
#include "../base/bigint.h"

#include "../syntax/location.h"

#include "../ast/scope.h"
#include "../ast/node.h"
#include "../ast/declarations.h"

#include "../intrinsics/types.h"

#undef unittest
#define unittest(name) \
	if(running){  \
		System::print(format("  Unittest %s was successfull!\n",running)); \
	} \
	running = #name; \
	if(!(running[0]=='_' && running[1]=='t')) System::print(format("Running unittest %s..\n",running));

void runTests(){
	//TODO
	const char* running = nullptr;

	unittest(Scope){
		auto scope = new Scope(0);

		assert(!scope->lookupPrefix("foo"));
		assert(!scope->lookupPrefix("bar"));
		assert(!scope->lookupInfix("bar"));
		assert(!scope->lookupInfix("foo"));
		assert(!scope->containsPrefix("foo"));
		assert(!scope->containsPrefix("bar"));
		assert(!scope->containsInfix("bar"));
		assert(!scope->containsInfix("foo"));

		delete scope;
	}

	unittest(_theEndDummy);
}
