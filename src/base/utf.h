/**
* This module is responsible for converting strings between various UTF formats.
*/
#ifndef ARPHA_UTF_H
#define ARPHA_UTF_H

#include "base.h"

bool isValidUnicodeCharacter(UnicodeChar c);

namespace UTF8 {
    UnicodeChar decode(const U8Char** ptr);
    size_t encode(UnicodeChar c, U8Char buf[4]);
}
namespace UTF16 {
	//Converts normal utf8 string to wide utf16 string.
	struct StringBuffer {
		StringBuffer(const char* str);
		operator const U16Char*();
	private:
		enum {
			BufferSize = 512
		};
		U16Char buffer[BufferSize];
		bool outOfBuffer;
		std::vector<U16Char> dynBuffer;
	};

    UnicodeChar decode(const U16Char** ptr);
    size_t encode(UnicodeChar c, U16Char buf[2]);
}

#endif