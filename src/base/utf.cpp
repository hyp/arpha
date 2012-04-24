#include "utf.h"

namespace UTF8 {

	UnicodeChar decode(U8Char** ptr) {
		if (!( (*(*ptr)) & 0x80)) {
			*ptr = (*ptr) + 1;
			return (UnicodeChar) *((*ptr)-1);
		}

		U8Char* str = *ptr;
		U8Char* end;
		U8Char first = *str;
		UnicodeChar result;

		/* The following encodings are valid, except for the 5 and 6 byte
		* combinations:
		*  0xxxxxxx
		*  110xxxxx 10xxxxxx
		*  1110xxxx 10xxxxxx 10xxxxxx
		*  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
		*  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		*  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		*/
		uint32 n = 1;
		for (;; n++) {
			if (((first << n) & 0x80) == 0) {
				break;
			}
		}
		if (n == 1 || n > 4) goto Lerr;

		// Pick off (7 - n) significant bits of B from first byte of octet
		result = (UnicodeChar) (first & ((1 << (7 - n)) - 1));

		/* The following combinations are overlong, and illegal:
		*  1100000x (10xxxxxx)
		*  11100000 100xxxxx (10xxxxxx)
		*  11110000 1000xxxx (10xxxxxx 10xxxxxx)
		*  11111000 10000xxx (10xxxxxx 10xxxxxx 10xxxxxx)
		*  11111100 100000xx (10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx)
		*/
		if ((first & 0xFE) == 0xC0 ||
			(first == 0xE0 && ((*(str + 1)) & 0xE0) == 0x80) ||
			(first == 0xF0 && ((*(str + 1)) & 0xF0) == 0x80) ||
			(first == 0xF8 && ((*(str + 1)) & 0xF8) == 0x80) ||
			(first == 0xFC && ((*(str + 1)) & 0xFC) == 0x80))
			goto Lerr; // overlong combination

		end = str + (ptrdiff_t) n;
		for (str++; str < end; str++) {
			if ((*str & 0xC0) != 0x80)
				goto Lerr; // trailing bytes are 10xxxxxx
			result = (result << 6) | (((UnicodeChar) (*str)) & 0x3F);
		}
		*ptr = (U8Char*) *ptr + n;
		return result;
Lerr:
		throw std::runtime_error("invalid UTF-8 sequence!");
		return 0;
	}

	size_t encode(UnicodeChar c, U8Char buf[4]) {
		if (c <= 0x7F) {
			buf[0] = (U8Char) c;
			return 1;
		}
		if (c <= 0x7FF) {
			buf[0] = (U8Char) (0xC0 | (c >> 6));
			buf[1] = (U8Char) (0x80 | (c & 0x3F));
			return 2;
		}
		if (c <= 0xFFFF) {
			if (0xD800 <= c && c <= 0xDFFF)
				throw std::runtime_error("Encoding a surrogate codepoint in UTF8!");

			buf[0] = (U8Char) (0xE0 | (c >> 12));
			buf[1] = (U8Char) (0x80 | ((c >> 6) & 0x3F));
			buf[2] = (U8Char) (0x80 | (c & 0x3F));
			return 3;
		}
		if (c <= 0x10FFFF) {
			buf[0] = (U8Char) (0xF0 | (c >> 18));
			buf[1] = (U8Char) (0x80 | ((c >> 12) & 0x3F));
			buf[2] = (U8Char) (0x80 | ((c >> 6) & 0x3F));
			buf[3] = (U8Char) (0x80 | (c & 0x3F));
			return 4;
		}

		throw std::runtime_error("Encoding an invalid codepoint in UTF8!");
	}
}

namespace UTF16 {

	//Converts normal utf8 string to wide utf16 string.
	StringBuffer::StringBuffer(const char* str){
		size_t i =0;
		const size_t limit = BufferSize - 3;
		auto u8str = (U8Char*) str;
		while(*u8str){
			i+=UTF16::encode(UTF8::decode(&u8str),buffer + i);
			if(i>=limit) break;
		}
		buffer[i] = 0;
	}
	StringBuffer::operator const U16Char*(){
		return buffer;
	}


	UnicodeChar decode(U16Char** ptr) {
		//TODO
		*ptr = *ptr + 1;
		return (UnicodeChar)*ptr;
	}

	size_t encode(UnicodeChar c, U16Char buf[2]) {
		if (c <= 0xFFFF) {
			if (0xD800 <= c && c <= 0xDFFF)
				throw std::runtime_error("Encoding a surrogate codepoint in UTF8!");
			buf[0] = (U16Char)c;
			return 1;
		}
		if (c <= 0x10FFFF) {
			buf[0] = (U16Char)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
			buf[1] = (U16Char)(((c - 0x10000) & 0x3FF) + 0xDC00);
			return 2;
		}
		throw std::runtime_error("Encoding an invalid codepoint in UTF8!");
	}
}

unittest(UTF){
#define expect(what) assert(what == UTF8::decode(&src));
	auto src = (U8Char*) "A\xC2\xA2\xC2\xA9\xC5\x8C\xC5\xBE\xD0\xA0\xE0\xA6\x95\xE2\x88\x9A\xE3\x81\x86\xF0\x9D\x90\x80";

	expect('A');
	expect(0x00A2); //cent sign
	expect(0x00A9); //copy right sign
	expect(0x014C); //Latin capital letter O with macron
	expect(0x017E); //Latin small letter z with caron
	expect(0x0420); //CYRILLIC CAPITAL LETTER ER
	expect(0x0995); //BENGALI LETTER KA
	expect(0x221A); //SQUARE ROOT
	expect(0x3046); //HIRAGANA LETTER U
	expect(0x1D400); //MATHEMATICAL BOLD CAPITAL A
#undef expect

	U8Char buf[4];
	size_t len;
#define expect(what) len=UTF8::encode(what,buf);assert(memcmp(src,buf,len)==0);src+=len
	src = (U8Char*) "A\xC2\xA2\xC2\xA9\xC5\x8C\xC5\xBE\xD0\xA0\xE0\xA6\x95\xE2\x88\x9A\xE3\x81\x86\xF0\x9D\x90\x80";

	expect('A');
	expect(0x00A2); //cent sign
	expect(0x00A9); //copy right sign
	expect(0x014C); //Latin capital letter O with macron
	expect(0x017E); //Latin small letter z with caron
	expect(0x0420); //CYRILLIC CAPITAL LETTER ER
	expect(0x0995); //BENGALI LETTER KA
	expect(0x221A); //SQUARE ROOT
	expect(0x3046); //HIRAGANA LETTER U
	expect(0x1D400); //MATHEMATICAL BOLD CAPITAL A
#undef expect
}