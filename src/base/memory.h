/**
*  Provides various abstractions for memory management.
*/
#ifndef ARPHA_MEMORY_H
#define ARPHA_MEMORY_H

#include "base.h"

namespace memory {

	/**
		A continuos, fixed-length array of bytes.
		Can be the owner of the array, and if it is, it will free up memory when release is called. 
	*/
	struct Block {
	private:
		const char* _ptr;
		size_t _length;

		enum {
			NotOwnerMask = 0x80000000
		};
	public:
		inline const char* ptr() const { return _ptr; }
		inline size_t length() const { return (_length&(~NotOwnerMask)); }
		inline char operator [](size_t i) const{ return _ptr[i]; }
		inline bool hasOwnership() const { return (_length&NotOwnerMask) == 0; }

		// Aquires data and ownership from another block, which looses the data and ownership.
		void aquire(Block& other);

		// Releases data if the block has the ownership of the block.
		void release();

		//Returns a portion of this block without block ownership.
		inline Block slice(size_t i,size_t j) const { return construct(ptr()+i,j-i); }

		// Allocates a new block of the same size and copies data into it.
		Block duplicate() const;

		// Creates a block object on an existing memory block without the block ownership.
		static Block construct(const char* begin,size_t length);
	};

	
}

std::ostream& operator<< (std::ostream& stream,const memory::Block& block);

#endif