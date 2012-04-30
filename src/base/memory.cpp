#include <stdlib.h>
#include "memory.h"
#include "system.h"
#include "format.h"

std::ostream& operator<< (std::ostream& stream,const memory::Block& block){
	assert(block.ptr());
	if(!block.hasOwnership()) stream<<"->";
	for(size_t i = 0;i<block.length();i++) stream<<block[i];
	return stream;
}

namespace memory {

	Block Block::duplicate() const{
		assert(ptr());
		if(!length()) assert(false);
		void* mem = System::malloc(length()+1);
		memcpy(mem,ptr(),length());
		(static_cast<char*>(mem))[length()] = '\0';
		Block _;
		_._ptr=static_cast<const char*>(mem);
		_._length=length();
		return _;
	}

	Block Block::construct(const char* begin,size_t length){
		assert(begin);
		assert((length&NotOwnerMask) == 0);
		Block _;
		_._ptr=begin;
		_._length=length|NotOwnerMask;
		return _;
	}

	void memory::Block::aquire(Block& other){
		assert(other._ptr);
		_ptr = other._ptr;_length = other._length;
		other._length|=NotOwnerMask;
	}

	void memory::Block::release(){
		assert(_ptr);
		if(hasOwnership()){
			System::free(const_cast<char*>(_ptr));
			_ptr = nullptr;_length = NotOwnerMask;
		}
	}

	unittest(memoryBlock){
		const char* data = "data";
		Block a = Block::construct(data,4);
		Block b;
		assert(a.ptr() == data && a.length() == 4);
		assert(!a.hasOwnership());
		b.aquire(a);
		assert(b.ptr() == data && b.length() == 4);
		assert(!b.hasOwnership());
		assert(a.ptr() == data && a.length() == 4);
		assert(!a.hasOwnership());
	}

	ManagedDefinition* definitionsRoot = nullptr;
	size_t managedDefinitionsAllocations = 0;

#define SYNCHRONIZED(x) x

	void* ManagedDefinition::operator new(size_t size){
		auto p = System::malloc(size);
		ManagedDefinition* next;
		SYNCHRONIZED(next = definitionsRoot;definitionsRoot = static_cast<ManagedDefinition*>(p);managedDefinitionsAllocations++);
		static_cast<ManagedDefinition*>(p)->next = next;
		return p;
	}

	//Mark and sweep collector for ManagedDefinitions
	/*static void collectDefinitions(){
		//Mark
		for(auto i = definitionsRoot;i!=nullptr;i=i->__next()){
			i->reach();
		}
		//Sweep
		ManagedDefinition* next = nullptr;
		ManagedDefinition* prev = nullptr;
		for(auto i = definitionsRoot;i!=nullptr;i=next){
			next = i->__next();
			if(!i->mark){
				System::free(i);
				if(prev) prev->next = next;
				else definitionsRoot = next;
			}else {
				i->mark = 0;
				prev = i;
			}	
		}
	}*/

	void reach(ManagedDefinition* definition){
		//Definitions aren't collected by GC for now, so don't do anything here
	}
	void init(){
	}
	void shutdown(){
		System::debugPrint(format("Shutting down memory - defs:%s.",managedDefinitionsAllocations));
		//Delete all the definitions
		ManagedDefinition* next;
		for(auto i = definitionsRoot;i!=nullptr;i=next){
			next = i->__next();
			System::free(i);
			managedDefinitionsAllocations--;
		}
		assert(managedDefinitionsAllocations == 0); //ensure no memory leaks
	}


	unittest(managedDefinition){
		struct Def: ManagedDefinition {
			int i;
		};
		assert(definitionsRoot == nullptr);
		auto x = new Def();
		assert(x);
		assert(definitionsRoot == x);
		assert(x->__next() == nullptr);
		auto y = new Def();
		assert(y);
		assert(definitionsRoot == y);
		assert(y->__next() == x);

		assert(managedDefinitionsAllocations == 2);
	}
}

