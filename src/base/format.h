/**
* This module contains string formating functions.
* Usage: format("Hello %s!","world") => "Hello world!"
*/
#ifndef ARPHA_FORMAT_H
#define ARPHA_FORMAT_H

#include "base.h"

std::string format(const char *s);

//No variadic templates in vc 2010
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
std::string format(const char *s, T value,T2 a,T3 b,T4 c,T5 d) FmtBody(a,b,c,d)
template<typename T,typename T2,typename T3,typename T4,typename T5,typename T6>
std::string format(const char *s, T value,T2 a,T3 b,T4 c,T5 d,T6 e) FmtBody(a,b,c,d,e)

#undef FmtBody

#endif