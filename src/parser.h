#ifndef PARSER_H
#define PARSER_H


//lexing
struct Token {
	enum {
		Symbol = 0,
		Uinteger,
		EndExpression,
		Eof,
	};
	
	union {
		uint64 uinteger;
		double real;
	};
	SymbolID symbol;	
	int type;
	
	Token();
	inline bool isSymbol() const { return type == Symbol; }
	inline bool isUinteger() const { return type == Uinteger; }
	inline bool isEOF() const { return type == Eof; }
	inline bool isEndExpression() const { return type == EndExpression; }
};
std::ostream& operator<< (std::ostream& stream,const Token& token);

struct Lexer {
	Lexer(const char* source);

	Token consume();
	Token peek();

	Location currentLocation(){ return location; }
	Location previousLocation(){ return location; }

protected:
	const char* ptr;
	Location location;
};

//parsing
struct Node; //ast node(expression)
struct Expression;
struct Scope;
struct Definition;
struct OverloadSet;
struct ExpressionFactory;

struct Parser : Lexer {

	Parser(const char* src,Scope* scope);

	void expect(SymbolID token);
	SymbolID expectName();
	int expectInteger();

	bool match(SymbolID token);
	bool match(int tokenType);

	bool isEndExpressionNext();

	Node* _parse(int stickiness =  0);
	Node* _parseModule();

	Type* parseOptionalType();

	/// Resolve symbols, find the matching function call overloads, constant fold
	Node* evaluate(Node* node);

	//Current parsing state
	Token  lookedUpToken;
	Scope* _currentScope;
	ExpressionFactory* expressionFactory;
	size_t unresolvedExpressions,solvedExpressions;

	inline Scope* currentScope() const { return _currentScope; }
};


#endif