#ifndef PARSER_H
#define PARSER_H

//to implement
struct Location {
private:
	int lineNumber;
public:

	inline int line(){ return lineNumber; }
	Location(int line);
};

struct Token {
	union {
		uint64 uinteger;
		double real;
	};

	
	
	SymbolID symbol;	
	bool isNumber;
	bool isString;
	bool isName;
	bool isEof;
	bool isEndExpression;	
	//future proofing
	int location;
	
	

	Token();
	inline bool isSymbol(){ return isName; }
	inline bool isUinteger(){ return isNumber; }
	inline bool isEOF(){ return isEof; }
	//inline bool isEndExpression(){ return isEndExpression; }
};
std::ostream& operator<< (std::ostream& stream,const Token& token);

struct Lexer {
	Lexer(const char* source);

	Token consume();
	Token peek();

	Location currentLocation(){ return location; }

private:
	const char* ptr;
	Location location;
};


/*
struct Scope;
struct Expression;

struct Parser : public Lexer {
	Parser(const char* source);
	Expression* parse(int stickiness = 0);

	Scope* currentScope;
};*/

#endif