#include "common.h"
#include "parser.h"
#include "ast.h"
#include "arpha.h"

void Parser::expect(SymbolID token){
	Token tok = consume();
	if(tok.isSymbol()==false || tok.symbol!=token) error(previousLocation(),"'%s' expected!",token);
}

bool Parser::match(SymbolID token){
	Token tok = peek();
	if(tok.isSymbol()==false || tok.symbol!=token) return false;
	consume();
	return true;
}

bool Parser::match(int tokenType){
	assert(tokenType >= Token::Symbol && tokenType <= Token::Eof); 
	Token tok = peek();
	if(tok.type != tokenType) return false;
	consume();
	return true;
}

bool Parser::isEndExpressionNext(){
	Token tok = peek();
	if(!tok.isEndExpression()) return false;
	return true;
}

SymbolID Parser::expectName(){
	Token tok = consume();
	if(tok.isSymbol()==false) error(previousLocation(),"A valid name is expected!");
	return tok.symbol;
}

Expression* Definition::prefixParse(Parser* parser,Token token){
	error(parser->previousLocation(),"Can't prefix parse %s!",token);
	return 0;
}

Expression* Definition::infixParse(Parser* parser,Token token,Expression*){
	error(parser->previousLocation(),"Can't prefix parse %s!",token);
	return 0;
}

//core
/*SymbolID parenthesisCloser;

Node* parseParenthesis(Definition*,Parser* parser){
	if( parser->match(parenthesisCloser) ){
		parser->consume();
		return 
	}
	auto e = parser->parse();
	parser->expect(")");
	return (Node*)e;
}

Node* parseCall(Definition*,Parser*,Node*){
}

Node* parseTuple(Definition*,Parser*,Node*){
}
Node* parseAccess(Definition*,Parser*,Node*){
}
Node* parseAssignment(Definition*,Parser*,Node*){
	}*/