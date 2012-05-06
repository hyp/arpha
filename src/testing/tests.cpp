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

		//normal scope lookup and contains
	/*	auto pdata = (PrefixDefinition*)0xDEADBEEF;
		scope->define(Location(),"foo",pdata);
		auto pdef = scope->lookupPrefix("foo");
		assert(pdef);
		assert(pdef == pdata);
		pdef = scope->containsPrefix("foo");
		assert(pdef);
		assert(pdef == pdata);

		auto idata = (InfixDefinition*)0xDEADBEEF;
		scope->define(Location(),"foo",idata);
		auto idef = scope->lookupInfix("foo");
		assert(idef);
		assert(idef == idata);
		idef = scope->containsInfix("foo");
		assert(idef);
		assert(idef == idata);

		//lookup from parent
		auto scope2 = new Scope(scope);
		scope->define(Location(),"bar",pdata);
		scope->define(Location(),"bar",idata);
		assert(scope->containsPrefix("bar") && scope->containsInfix("bar"));
		assert((!scope2->containsPrefix("bar")) && (!scope2->containsInfix("bar")));
		assert(scope2->lookupPrefix("bar") == scope->containsPrefix("bar"));
		assert(scope2->lookupInfix("bar") == scope->containsInfix("bar"));*/

		//


		//delete scope2;
		delete scope;
	}
	unittest(typeSystem){
		/*auto type = new TypeExpression();
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
		assert(type4->size() == 4);*/
	}
	unittest(_theEndDummy);
}
