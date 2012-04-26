#include <stdlib.h>
#include "memory.h"
#include "system.h"

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
		void* mem = System::malloc(length());
		memcpy(mem,ptr(),length());
		Block _;
		_._ptr=(const char*)mem;
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
			System::free((void*)_ptr);
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
}

