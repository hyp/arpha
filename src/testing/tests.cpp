/**
* This module contains all the non base unittests.
*/
#include "../base/base.h"
#include "../base/format.h"
#include "../base/memory.h"
#include "../base/system.h"
#include "../base/symbol.h"

#include "../scope.h"
#include "../ast/declarations.h"
#include "../ast/node.h"

#include "../intrinsics/types.h"
#include "../arpha.h"

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

	unittest(typeSystem){
		auto type = new TypeExpression();
		assert(!type->resolved());
		assert(type->_returnType() == intrinsics::types::Unresolved);
		auto integer = new IntegerType("uint32",Location());
		//TODO max/min/signed/unsigned/valid tests
		auto type2 = new TypeExpression(integer);
		assert(type2->resolved());
		assert(type2->_returnType() == intrinsics::types::Type);
		auto type3 = new TypeExpression(TypeExpression::CONSTANT,type2);//const uint32
		assert(type3->resolved());
		assert(type3->_returnType() == intrinsics::types::Type);
		auto type4 = new TypeExpression(TypeExpression::POINTER,type2);//uint32*
		assert(type4->resolved());
		assert(type4->_returnType() == intrinsics::types::Type);
		assert(type4->size() == 4);
	}
	unittest(_theEndDummy);
}
