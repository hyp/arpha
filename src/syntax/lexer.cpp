#include "lexer.h"

Lexer::Lexer(const char* source) : location(0,0) {
	ptr= source;
}

bool isDigit(char c){
	return c>='0' && c<='9';
}

bool isLetter(char c){
	return (c>='a' && c<='z') || (c>='A' && c<='Z') || c == '_';
}

Token Lexer::consume(){
	Token token;	
	while((*ptr) <= ' ' && (*ptr)!='\0' && (*ptr)!='\n') ptr++; //skip spaces
	
	if( *ptr == '\n' || *ptr ==';'){
		if(*ptr == '\n') location = Location(location.line()+1,0);
		else location.column++;
		token.type = Token::EndExpression;
		ptr++;			
	}
	else if(*ptr == '(' || *ptr==')' || *ptr == ',' || *ptr == '{' || *ptr == '}' || *ptr == ':'){
		token.symbol = SymbolID(ptr,1);
		ptr++;
		token.type = Token::Symbol;
	}
	else if( isDigit(*ptr)){
		token.uinteger=0;
		for(;isDigit(*ptr);ptr++)
			token.uinteger = token.uinteger*10 + int((*ptr) -	'0');
		token.type = Token::Uinteger;	
	}
	else if(*ptr=='\0') {
		token.type = Token::Eof;	
	}		
	else if( isLetter(*ptr) ){
		const char* start = ptr;
		for(;(*ptr) > ' ' && (isLetter(*ptr) || isDigit(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type = Token::Symbol;
	}else{
		const char* start = ptr;
		for(;(*ptr) > ' ' && (!isDigit(*ptr)) && (!isLetter(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type = Token::Symbol;	
	}			
	return token;		
}

Token Lexer::peek(){
	auto ptr2 = ptr;
	auto t = consume();
	ptr = ptr2;
	return t;
}

unittest(lexer){
	Token token;
#define expectSymbol(which) token = lexer.consume();assert(token.isSymbol() && token.symbol==SymbolID(which))
#define expectUinteger(n) token = lexer.consume();assert(token.isUinteger() && token.uinteger == n)
#define expectEof() token = lexer.consume();assert(token.isEOF())
	
	auto lexer = Lexer("foo 2 =");
	assert(lexer.currentLocation().line() == 0);
	expectSymbol("foo");
	expectUinteger(2);
	expectSymbol("=");
	assert(lexer.currentLocation().line() == 0);
	expectEof();
	

	lexer = Lexer("a_b bar + 5 - 7");
	assert(lexer.currentLocation().line() == 0);
	expectSymbol("a_b");
	expectSymbol("bar");
	expectSymbol("+");
	expectUinteger(5);
	expectSymbol("-");
	expectUinteger(7);
	assert(lexer.currentLocation().line() == 0);
	expectEof();

	//clean up
	//symbols.~SymbolTable();
#undef expectSymbol
#undef expectUinteger
#undef expectEof
}