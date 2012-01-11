#ifndef COMON_H
#define COMON_H

#include<stdio.h>
#include<string.h>
#include<math.h>
#include <time.h>
#include <vector>
#include <new>
#include <string>
#include <map>
#include <exception>
#include <sstream>
#include <iostream>

typedef __int64 int64;
typedef unsigned __int64 uint64;
typedef unsigned int uint32;

void _assert(const char* file, int line, const char* what);
#define assert(what) ((what) ? (void)0 : _assert( __FILE__, __LINE__, (#what)))

namespace testing {

	struct Unittest {
		Unittest(const char* name,void (*func)());
	};	
}
#define unittest(name) void __unittest__##name(); testing::Unittest __Utest__##name(#name,& __unittest__##name); void __unittest__##name()

//string formatting
std::string format(const char *s);

//NoVariadicTemplatesInVC2010
#define FmtBody(...) { std::ostringstream stm; while (*s) {  \
        if (*s == '%' && *(++s) != '%') { \
            stm << value; \
            ++s; \
            stm << format(s,__VA_ARGS__); \
            return stm.str(); \
        } \
        stm << (*s++); \
    } \
    throw std::logic_error("extra arguments provided to format!"); \
} 

template<typename T>
std::string format(const char *s, T value) FmtBody()
template<typename T,typename T2>
std::string format(const char *s, T value,T2 a) FmtBody(a)
template<typename T,typename T2,typename T3>
std::string format(const char *s, T value,T2 a,T3 b) FmtBody(a,b)
template<typename T,typename T2,typename T3,typename T4>
std::string format(const char *s, T value,T2 a,T3 b,T4 c) FmtBody(a,b,c)
template<typename T,typename T2,typename T3,typename T4,typename T5>
std::string format(const char *s, T value,T2 a,T3 b,T4 c,T5 d) FmtBody(a,b,d)

#undef FmtBody

void printFunc(std::string message);
void debugPrint(std::string message);

#define error(loc,...) compiler->onError(loc,format(__VA_ARGS__))
#define printf(...) printFunc(format(__VA_ARGS__))
#define debug(...) debugPrint(format(__VA_ARGS__))

//a table of unique symbols for fast symbol comparison
struct SymbolTable {

public:
    struct Symbol {
        Symbol* next;
        size_t length;
        char ptr[0];
    };

	SymbolTable();
    ~SymbolTable();
    Symbol* create(const char* src, size_t length);
private:
    enum {
        hashTableLength = 64
    };
    Symbol* hashTable[hashTableLength];
};
extern SymbolTable symbols;

struct SymbolID {

	inline SymbolID() : symbol(0) {}
	inline SymbolID(const char* begin,const char* end) { symbol=symbols.create(begin,size_t(end-begin)); }
	inline SymbolID(const char* str,size_t length) { symbol=symbols.create(str,length); }
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
	inline bool isNull() const{
		return symbol == 0;
	}
	inline bool operator <(const SymbolID& other) const{
		return symbol < other.symbol;
	}
private:
	SymbolTable::Symbol* symbol;
};

std::ostream& operator<< (std::ostream& stream,const SymbolID symbol);

const char* readFile(const char* filename);

#endif