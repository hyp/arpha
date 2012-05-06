#include "lexer.h"
#include "../base/utf.h"
#include "../compiler.h"


Lexer::Lexer(const char* source) : location(0,0) {
	ptr= source;
	//check for UTF8 BOM
	if(ptr[0]=='\xEF' && ptr[1]=='\xBB' && ptr[2]=='\xBF') ptr+=3;
}

bool isDigit(char c){
	return c>='0' && c<='9';
}

bool isLetter(char c){
	return (c>='a' && c<='z') || (c>='A' && c<='Z') || c == '_';
}

bool isSpace(char c){
	return c==' ' || c=='\t';
}

//TODO make so that on when we peek, we don't duplicate the string
Token Lexer::consume(){
	Token token;	

	//Skip spaces and tabs.
	while((*ptr) <= ' ' && (*ptr)!='\0' && (*ptr)!='\n'){
		ptr++;location.column++; //skip spaces
	}
	//
	if( *ptr == '\n'){
		token.type = Token::Line;
		location.lineNumber++;
		ptr++;	
	}
	else if(*ptr ==';' || *ptr == '(' || *ptr==')' || *ptr == ',' || *ptr == '{' || *ptr == '}' || *ptr == ':'){
		token.symbol = SymbolID(ptr,1);
		ptr++;
		token.type = Token::Symbol;
	}
	//Number literal
	else if( isDigit(*ptr) ){
		token.type = Token::Uinteger;
		//the integer part
		token.uinteger = int((*ptr) - '0');
		for(ptr++;isDigit(*ptr) || *ptr == '_';ptr++){
			if(*ptr != '_') token.uinteger = token.uinteger*10 + int((*ptr) -	'0');
		}
		if(*ptr == '.'){ //the fractional part
			for(ptr++;isDigit(*ptr);ptr++);
		}
	}
	//Comment TODO - fix crashes when comment isnt closed till eof
	else if(*ptr=='#') {
		ptr++;
		if((*ptr) == '\n' || (*ptr) == '\r'){
			for(;!( (*ptr)=='#' && ((*(ptr+1))=='\r' || (*(ptr+1))=='\n') ) ;ptr++); //skip till '#' followed by newline
			ptr++;
		}
		else for(;(*ptr)!='\n';ptr++); //skip till the end of newline
		return consume();
	}
	else if(*ptr == '"'){
		ptr++;
		auto begin = ptr;
		for(;(*ptr)!='"';ptr++);
		token.type = Token::String;
		token.string = memory::Block::construct(begin,size_t(ptr - begin)).duplicate();
		ptr++;
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
	auto loc = location;
	auto ptr2 = ptr;
	auto t = consume();
	ptr = ptr2;
	location = loc;
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