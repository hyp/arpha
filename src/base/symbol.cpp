#include <stdlib.h>
#include <time.h>
#include "symbol.h"

//A global table of unique symbols for fast symbol comparison
struct SymbolTable {
public:
	SymbolTable();
    ~SymbolTable();
    Symbol* create(const char* src, size_t length);
private:
    enum {
        hashTableLength = 64
    };
    Symbol* hashTable[hashTableLength];
};
SymbolTable symbols;

SymbolID::SymbolID(const char* begin,const char* end) { 
	assert(begin);
	assert(end);
	symbol=symbols.create(begin,size_t(end-begin)); 
}
SymbolID::SymbolID(const char* str,size_t length) { 
	assert(str);
	symbol=symbols.create(str,length); 
}
SymbolID::SymbolID(const char* str){
	assert(str);
	symbol = symbols.create(str,strlen(str));
}
std::ostream& operator<< (std::ostream& stream,const SymbolID symbol){
	return stream<<symbol.ptr();
}

//A hash function for a string
//TODO replace with a proper hash
inline size_t hashString(const char* src, size_t length) {
	return length + (size_t) src[0];
}

Symbol* SymbolTable::create(const char* src, size_t length) {
	if(length == 0) return nullptr;
	size_t hsh = hashString(src, length) % hashTableLength;
	//compare strings in current chain
	Symbol** current = &hashTable[hsh];
	while ((*current) != nullptr) {
		if ((*current)->length == length) {
			if (memcmp((*current)->ptr, src, length) == 0) {
				return *current;
			}
		}
		current = &((*current)->next);
	}
	//new entry
	(*current) = (Symbol*) malloc(sizeof (Symbol) + sizeof (char) *(length + 1));
	(*current)->next = nullptr;
	(*current)->length = length;
	memcpy((*current)->ptr, src, length);
	(*current)->ptr[length] = 0; //null terminate
	return *current;
}

SymbolTable::SymbolTable() {
	for (size_t i = 0; i < hashTableLength; i++) hashTable[i] = nullptr;
}

SymbolTable::~SymbolTable() {
	for (size_t i = 0; i < hashTableLength; i++) {
		Symbol* current = hashTable[i], *next;
		for (; current != nullptr; current = next) {
			next = current->next;
			free(current);
		}
		hashTable[i] = nullptr;
	}
}

//Testing the table
unittest(symbolTable){
	SymbolTable symbols;
	char test[5];
	int length,i;

	srand(time(0));

	const int iterations = 100;
	for(int j=0;j<iterations;j++){
		length = rand()%4+1;
		for(i=0;i<length;i++) test[i]=rand()%255+1;
		test[i] = '\0';

		auto a = symbols.create(test,length) , b = symbols.create(test,length);
		test[0]=~test[0];
		auto c  = symbols.create(test,length);
		assert(a == b);
		assert(a != c);
		assert(b != c);
		assert(strcmp(test,c->ptr)==0);
		SymbolID x = *((SymbolID*)&a),y = *((SymbolID*)&b),z = *((SymbolID*)&c);
		assert(x == y);
		assert(x != z);
		assert(y != z);
	}
}
