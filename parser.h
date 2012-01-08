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

void errorFunc(Location& location,std::string message);

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
struct Expression;
struct Scope;
struct Definition;
struct OverloadSet;

struct Parser : Lexer {
	struct State {
		const char* ptr;
		bool dontLookupNextSymbol;
	};

	Parser(const char* src,Scope* scope);

	State getState();
	void restoreState(State& state);

	void expect(SymbolID token);
	SymbolID expectName();

	bool match(SymbolID token);
	bool match(int tokenType);

	bool isEndExpressionNext();
	
	/// Returns a raw expression, which may contain unresolved symbols
	Expression* parse(int stickiness = 0);

	Expression* parseBlock();

	/// Resolve symbols, find the matching function call overloads, constant fold
	Expression* evaluate(Expression*);

	//Current parsing state
	Scope* currentScope;
	bool dontLookupNextSymbol;	
	size_t unresolvedExpressions,solvedExpressions;
};

//Defines how to parse a name
struct Definition {
	

	virtual Expression* prefixParse(Parser*,Token);
	virtual Expression* infixParse(Parser*,Token,Expression*);

	//Tries to resolve an unresolved expression
	//Returns the given expression if resolving failed
	virtual Expression* resolve(Expression* expr) ;

	virtual bool isOverloadSet();
	
	Definition(Scope* scp,SymbolID name);
	Definition(Scope* scp,SymbolID name,int sticky);

	Location location() const;

	SymbolID id;
	int stickiness;
	Scope* scope;
	int lineNumber; //location

protected:
	OverloadSet* getSet();
};

struct Scope {

	Scope(Scope* parent);
	void define(Definition* definition);
	Definition* lookup(SymbolID name);
	Definition* contains(SymbolID name);

	Scope* parent;
	Definition* owner;
private:
	std::map<SymbolID,Definition*>  definitions;
};

//simple expression substitution - def pi = 3.14 - status: expressionwise 100% , def parser 40% (need multiples)
struct Substitute: Definition {
	Substitute(Scope* scope,SymbolID name,Location location,Expression* expr);
	Expression* prefixParse(Parser* parser,Token);
private:
	Expression* substitute;
};

#endif