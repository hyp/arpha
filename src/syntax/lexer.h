/**
* This module contains a uniform lexer which provides interface for token access to the parser.
*/
#ifndef ARPHA_LEXER_H
#define ARPHA_LEXER_H

#include "../common.h"
#include "../base/base.h"
#include "../base/symbol.h"
#include "token.h"

struct Lexer {
	Lexer(const char* source);

	Token consume();
	Token peek();

	inline Location currentLocation(){ return location; }
	inline Location previousLocation(){ return location; }

protected:
	const char* ptr;
	Location location;
};

#endif