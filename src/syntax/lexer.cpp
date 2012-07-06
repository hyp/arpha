#include "lexer.h"
#include "../base/utf.h"
#include "../compiler.h"

static inline bool isDigit(char c){
	return c>='0' && c<='9';
}
static inline bool isLetter(char c){
	return (c>='a' && c<='z') || (c>='A' && c<='Z') || c == '_';
}
static inline bool isSpace(char c){
	return c==' ' || c=='\t';
}
static inline bool isHexDigit(char c){
	return (c>='0' && c<='9') || (c>='A' && c<='F') || (c>='a' && c<='f');
}
static inline bool isSingleCharSymbol(char c){
	return c == '(' || c == ')' || c == ',' || c == '{' || c == '}'  || c == ':'  || c == ';';
}

char hexToInt(char c){
	if(c<'A') return c - '0';
	else return c - (c>='a' ? 'a' - 10 : 'A' - 10);
}

unittest(chars){
	assert(isSpace(' ') && isSpace('\t'));

	const char* hex = "0123456789ABCDEF";
	for(auto i =0;i<16;i++) assert(isHexDigit(hex[i]));
	for(auto i =0;i<16;i++) assert(hexToInt(hex[i]) == i);
}

Lexer::Lexer(const char* source) : location(0,0) {
	ptr= source;
	//check for UTF8 BOM
	if(ptr[0]=='\xEF' && ptr[1]=='\xBB' && ptr[2]=='\xBF') ptr+=3;
	original= ptr;
	peeked = false;
}

void Lexer::saveState(State *state){
	state->src= ptr;
	state->original = original;
	state->location = location;
	state->peeked = peeked;
	state->peekedToken = peekedToken;
}
void Lexer::restoreState(State *state){
	ptr = state->src;
	original = state->original;
	location = state->location;
	peeked = state->peeked;
	peekedToken = state->peekedToken;
}

UnicodeChar Lexer::escapeChar(){
	UnicodeChar c;
	switch(*ptr){
	case 'n': c  = '\n'; break;
	case 'r': c  = '\r'; break;
	case '\\': c = '\\'; break;
	case '"':  c  = '"'; break;
	case '\'': c = '\''; break;
	case '?':  c = '?';  break;
	case 't':  c = '\t'; break;
	case 'a':  c = '\a'; break;
    case 'b':  c = '\b'; break;
	case 'v':  c = '\v'; break;
	case 'f':  c = '\f'; break;
	case 'x': //\xHexHex
	case 'U': //\UHexHexHexHexHexHex
	case 'u': //\uHexHexHexHex
		{
		char seq = *ptr;
		int escapeLength = seq == 'u' ? 4 : (seq == 'x' ? 2 : 6);
		c = 0;
		for(auto i = 0;i<escapeLength;i++){
			ptr++;
			c*=0x10;
			if(!isHexDigit(*ptr)){
				onExpectedError("a valid hexadecimal digit in an escape sequence '\\u'");
				break;
			}
			c+=hexToInt(*ptr);
		}
		if(!isValidUnicodeCharacter(c)){
			syntaxError(format("Expected a valid unicode code point instead of U+%d!",c));
		}
		break;
		}
	default:
		onExpectedError("a valid escape sequence after '\\'");
		c = '?';
	}
	ptr++;
	return c;
}
UnicodeChar Lexer::lexChar(){
	if(*ptr >= 128) return UTF8::decode((const U8Char**)ptr);
	else {
		auto c = *ptr; ptr++;
		return c != '\\' ? c : escapeChar();
	}
}

bool Lexer::matchNewline(){
	//Source: https://secure.wikimedia.org/wikipedia/en/wiki/Newline#Unicode
	//NOTE: NEL,LS & PS aren't supported!
	auto c = *ptr;
	if(c >= 0xA && c<=0xD){ //LF,VT,FF,CR
		ptr++;
		if(c == 0xD && *ptr == 0xA) ptr++; //CR+LF
		location.lineNumber++;
		original = ptr;
		return true;
	}
	return false;
}

void Lexer::lexComment(){
	ptr++;
	if(!matchNewline()){
		//single line comment
		while(!matchNewline() && *ptr != '\0') ptr++;
	} else {
		//multiline comment
		do {
			auto prev = *ptr;ptr++;
			if(matchNewline()){
				if(prev == '#') break;
			}
			if(*ptr == '\0'){
				onUnexpectedEOF("'#' followed by a newline");
				break;
			}
		} while(true);
	}
}
void Lexer::lexString(Token& token){
	ptr++;
	auto begin = ptr;
	memory::StringLiteralConstructor string;
	for(;(*ptr)!='"';){
		if(matchNewline()) string.append('\n');
		else if(*ptr == '\0'){
			onUnexpectedEOF("a closing '\"'");
			ptr--;
			break;
		}
		else string.append(lexChar());
	}
	token.type   = Token::String;
	token.string = string.toString();
	ptr++;
}
void Lexer::lexChar(Token& token){
	ptr++;
	token.type = Token::Uinteger;
	if(!matchNewline()){
		token.uinteger = lexChar();
		if(*ptr != '\'') onExpectedError("a closing '");
		else ptr++;
	}
	else onExpectedError("a valid character");
}
void Lexer::lexHex(Token& token){
	uint64 uint = 0;
	for(ptr++;isHexDigit(*ptr) || *ptr == '_';ptr++){
		if(*ptr != '_') uint = uint*0x10 + hexToInt(*ptr);
	}
	token.type = Token::Uinteger;
	token.uinteger = uint;
}
void Lexer::lexFloat(Token& token){
	double real = double(token.uinteger);
	double iteration = 0.1;
	for(ptr++;isDigit(*ptr) || *ptr == '_';ptr++){
		if(*ptr != '_'){
			real+=iteration*((*ptr) - '0');
			iteration*=0.1;
		}
	}
	token.type = Token::Real;
	token.real = real;
}
void Lexer::lexDecimal(Token& token){
	//the integer part
	uint64 uint = uint64((*ptr) - '0');
	for(ptr++;isDigit(*ptr) || *ptr == '_';ptr++){
		if(*ptr != '_') uint = uint*10 + ((*ptr) -	'0');
	}

	token.uinteger = uint;
	if(*ptr == '.' && isDigit(*(ptr+1))) lexFloat(token);
	else token.type = Token::Uinteger;
}

Token Lexer::consume(){
	if(peeked){
		peeked = false;
		return peekedToken;
	}
	Token token;	
	//Skip spaces and tabs.
	while(isSpace(*ptr)) ptr++;
	location.column = int(ptr - original);
	
	//lex
	if(isSingleCharSymbol(*ptr)){
		token.symbol = SymbolID(ptr,1);ptr++;
		token.type   = Token::Symbol;
	}
	else if( isLetter(*ptr) ){
		//alphabetical symbol
		const char* start = ptr;
		for(;(*ptr) > ' ' && (isLetter(*ptr) || isDigit(*ptr) || (*ptr >= 128));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type   = Token::Symbol;
	}
	else if(matchNewline()){
		token.type = Token::Line;
	}
	//Number literal
	else if( isDigit(*ptr) ) lexDecimal(token);
	else if( *ptr == '0' ){
		ptr++;
		if(*ptr == 'x' && isHexDigit(*(ptr+1))) lexHex(token);
		else if(*ptr == '.' && isDigit(*(ptr+1))) {
			token.uinteger = 0;
			lexFloat(token);
		}
		else {
			token.type = Token::Uinteger;
			token.uinteger = 0;
		}
	}
	else if(*ptr == '"')  lexString(token);
	else if(*ptr == '.' && isDigit(*(ptr+1)) ){
		token.uinteger = 0;
		lexFloat(token);
	}
	else if(*ptr == '#') {
		lexComment();
		token.type = Token::Line;
	}
	else if(*ptr == '\'') lexChar(token);
	else if(*ptr=='\0') {
		token.type = Token::Eof;	
	}		
	else{
		//other symbol
		const char* start = ptr;
		for(;(*ptr) > ' ' && (!isDigit(*ptr)) && (!isLetter(*ptr)) && !isSingleCharSymbol(*ptr);ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type = Token::Symbol;	
	}			
	return token;		
}

Token Lexer::peek(){
	if(!peeked) {
		peekedToken = consume();
		peeked = true;
	}
	return peekedToken;
}

void  Lexer::onUnexpectedEOF(const char* str){
	compiler::onError(location,format("Unexpected end of file reached - Expected %s!",str));
}
void  Lexer::onExpectedError(const char* str){
	compiler::onError(location,format("Expected %s instead of '%c'!",str,*ptr));
}
void  Lexer::syntaxError(std::string& msg){
	compiler::onError(previousLocation(),msg);
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