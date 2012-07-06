/**
* This module contains a uniform lexer which provides interface for token access to the parser.
*/
#ifndef ARPHA_LEXER_H
#define ARPHA_LEXER_H

#include "../base/base.h"
#include "../base/symbol.h"
#include "token.h"
#include "location.h"

struct Lexer {
	Lexer(const char* source);

	struct State {
		const char* src;
		const char* original;
		Location location;
		bool peeked;
		Token peekedToken;
	};	
	void saveState(State *state);
	void restoreState(State *state);

	Token consume();
	Token peek();

	inline Location currentLocation() { return location; }
	inline Location previousLocation(){ return location; }

	void syntaxError(std::string& msg);
private:
	const char* ptr;
	const char* original;
	Location location;
	bool  peeked;
	Token peekedToken;

	bool matchNewline();
	UnicodeChar lexChar();
	UnicodeChar escapeChar();
	void lexComment();
	void lexString(Token& token);
	void lexChar(Token& token);
	void lexDecimal(Token& token);
	void lexHex(Token& token);
	void lexFloat(Token& token);
	
	void onUnexpectedEOF(const char* str);
	void onExpectedError(const char* str);
	
};

#endif