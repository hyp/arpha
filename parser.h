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

	/// Returns a block - a list of raw expressions, surrounded by { } if required
	Expression* parseBlock(bool surroundedByBrackets = true);

	Expression* parseModule();

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
	virtual Expression* resolve(Expression* expr);

	virtual bool isOverloadSet();
	
	Definition(Scope* scp,SymbolID name);
	Definition(Scope* scp,SymbolID name,Location location,int sticky = -1);
	Definition(Scope* scp,SymbolID name,int sticky);

	Location location() const;

	SymbolID id;
	int stickiness;
	Scope* scope;
	
protected:
	int lineNumber; //location
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

//type. tuple is an unnamed type
struct Type: public Definition {

	struct Field {
		Type* type;
		SymbolID name;

		Field(Type* type,SymbolID name);
		bool isUnnamed();
	};

	Type(Scope* scope,SymbolID name,size_t sz);

	Expression* prefixParse(Parser*,Token);

	//map like interface for field queries
	Type::Field* operator[](const SymbolID fieldName);
	void add(Field field);
	
	//implicit type cast
	bool canAssignFrom(Type* other);

	//global tuple constructs
	static std::vector<Type*> tuples;
	static Type* tuple(Type* a,Type* b);
	static Type* flattenedTuple(Type* a,Type* b);
	static Type* tuple(std::vector<Field>& fields);

	uint32 size;
	uint32 alignment;
	bool isTuple;
	std::vector<Field> fields;

private:
	Type(SymbolID name,size_t sz);
};

//variable
struct Variable: public Definition {
	Variable(Scope* scope,SymbolID name,Location location,Type* type);
	
	Expression* prefixParse(Parser*,Token);

	void bindToConstant(Expression* expr);
	Expression* bindedConstant();

	Type* type;
private:
	Expression* substitute;
};

//Parses operators as function calls ex. 1 + 2 => add(1,2)
struct Operator : public Definition {
	
	Operator(Scope* scope,SymbolID name,Location location,SymbolID func,int sticky = -1);


	Expression* prefixParse(Parser*,Token);
	Expression* infixParse(Parser*,Token,Expression*);

	struct Parselet : public Definition {
		Parselet(Scope* scope,SymbolID name,Location location);

		Expression* prefixParse(Parser*,Token);
	};

private:
	SymbolID functionName;

};

#endif