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
	struct State {
		const char* ptr;
	};

	Parser(const char* src,Scope* scope);

	State getState();
	void restoreState(State& state);

	void expect(SymbolID token);
	SymbolID expectName();

	bool match(SymbolID token);
	bool match(int tokenType);

	bool isEndExpressionNext();

	Node* _parse(int stickiness =  0);
	Node* _parseModule();
	
	/// Returns a raw expression, which may contain unresolved symbols
	Expression* parse(int stickiness = 0);

	/// Returns a block - a list of raw expressions, surrounded by { } if required
	Expression* parseBlock(bool surroundedByBrackets = true);

	Expression* parseModule();

	/// Resolve symbols, find the matching function call overloads, constant fold
	Node* evaluate(Node* node);
	Expression* evaluate(Expression*);

	//Current parsing state
	Token  lookedUpToken;
	Scope* _currentScope;
	ExpressionFactory* expressionFactory;
	size_t unresolvedExpressions,solvedExpressions;

	inline Scope* currentScope() const { return _currentScope; }
};

//::= substitute => Expression
//def->data must be a valid Node*
//Node* parseSubstitute(PrefixDefinition* def,Parser* parser);

//::= type   => TypeExpression
//def->data must be a valid Type*
Node* parseTypeExpression(PrefixDefinition* def,Parser* parser);

//::= overloadSet => OSExpression
//def->data must be a valid OverloadSet*
Node* parseOverloadSetExpression(PrefixDefinition* def,Parser* parser);

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

//variable
struct Function: public Definition {
	OverloadSet* set;
	Type* argument;
	Type* returnType;
	Scope* bodyScope;
	Expression* body;
	Expression* constraint; //constraint for inferred functions

	struct Argument {
		Variable variable;			 // so that code inside the functions has access to arguments
		Function* typeConstraint;  // Arithmetic
		Definition* valueConstraint; // x <- int32 //can be type or function!
		
		Argument(const Variable& var,Function* typeConstraint,Definition* valueConstraint);
	};

	std::vector<Argument> arguments;

	Function(Scope* scope,SymbolID name,Type* argumentType,Type* retType,Scope* bodyScope,Expression* body);

	Function* infer(Type* type);
};

struct OverloadSet: public Definition {
	OverloadSet* parent;
	std::vector<Function*> functions;

	OverloadSet(Scope* scope,SymbolID name);

	Expression* prefixParse(Parser*,Token);
	bool isOverloadSet(){ return true; }

	//Tries to resolve an unresolved expression using the set symbol hierarchy
	//Returns Unresolved expression if resolving fails
	void findMatches(std::vector<Function*>& results,const Node* argument);
	Expression* resolve(Expression*);

	Function* add(Function* func);
	Function* find(Expression* expr);
};


//Parses operators as function calls ex. 1 + 2 => add(1,2)




#endif