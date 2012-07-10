/**
* This module contains the heart of arpha - THE Parser.
*/
#ifndef ARPHA_PARSER_H
#define ARPHA_PARSER_H

#include "token.h"
#include "lexer.h"

struct Node;
struct BlockExpression;
struct Variable;
struct Function;
struct Record;
struct Scope;
struct CompilationUnit;

struct Parser : Lexer {
private:


	CompilationUnit* _compilationUnit;
public:

	Parser(const char* src,CompilationUnit* compilationUnit);

	CompilationUnit* compilationUnit() const;

	//This is a one func army!
	Node* parse(int stickiness =  0);

	void expect(SymbolID token);
	SymbolID expectName();

	bool match(SymbolID token);
	bool match(int tokenType);

	//Usage: parser.expect("foo",ignoreNewlines:true) => parser->ignoreNewlines();parser->expect("foo");
	void ignoreNewlines();
	//Usage: parser.match("foo",ignoreNewlines:true) => NewlineIgnorer(true,parser); if(!parser->match("foo")) rollback();
	// Use if you want to undo the ignoring of newlines
	struct NewlineIgnorer {

		NewlineIgnorer(bool doIgnore,Parser* parser);
		void rollback();
	private:
		bool ignore;
		Parser::State state;
		Parser* parser;
	};

	//introducing definitions
	void introduceDefinition(Variable* variableDefinition);
	void introduceDefinition(Function* functionDefinition);
	void introduceDefinition(TypeDeclaration* typeDeclaration);
	void introduceDefinition(PrefixMacro* macroDefinition);
	void introduceDefinition(InfixMacro* macroDefinition);

	//definition properties
	//TODO remove this hacks
	void useProperty(SymbolID name,Node* value);
	void useProperty(SymbolID name); // implies value = true
	void clearProperties();
	void useTypedArgument(SymbolID name,Node* type); //use argument parser as *Parser
	
	void applyProperties(Node* node);

	//Current parsing state
	Token  lookedUpToken;
	Scope* _outerMacroOuterScope;//the [> <] inside a macro will be parsed using this scope
	std::vector<Node*> mixinedExpressions;
private:
	Scope* _currentScope;
	
public:
	inline Scope* currentScope() const { return _currentScope; }
	void currentScope(Scope* scope);

	void enterBlock(BlockExpression* block);
	void leaveBlock();

	Node* mixinMacroResult(CTFEinvocation* invocation);
};


#endif
