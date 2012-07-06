/**
* All unique lexed symbols are stored in a global symbol table and a referenced by a unique symbolID.
*/
#ifndef ARPHA_SYMBOL_H
#define ARPHA_SYMBOL_H

#include "base.h"

//A symbol is a unique string.
struct Symbol {
   Symbol* next;
   size_t length;
   char ptr[0];
};

//Abstracts a pointer to a symbol stored in a hash table
struct SymbolID {

	inline SymbolID() : symbol(nullptr) {}
	SymbolID(const char* begin,const char* end);
	SymbolID(const char* str,size_t length);
	SymbolID(const char* str);

	inline bool operator ==(const SymbolID& other) const {
		return symbol == other.symbol;
	}
	inline bool operator !=(const SymbolID& other) const {
		return symbol != other.symbol;
	}
	inline const char* ptr() const {
		return symbol->ptr;
	}
	inline const size_t length() const {
		return symbol->length;
	}
	inline bool isNull() const{
		return symbol == nullptr;
	}
	inline bool operator <(const SymbolID& other) const{
		return symbol < other.symbol;
	}
private:
	Symbol* symbol;
};

//Outputs the symbol
std::ostream& operator<< (std::ostream& stream,const SymbolID symbol);

#endif