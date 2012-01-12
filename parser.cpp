#include "common.h"
#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

Parser::Parser(const char* src,Scope* scope) : Lexer(src) { _currentScope=scope; }

Parser::State Parser::getState(){
		State state;
		state.ptr = ptr;
		return state;
}

void Parser::restoreState(Parser::State& state){
		ptr = state.ptr;
}

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

Node* Parser::_parse(int stickiness){
	
	Node* expression;

	//prefix	
	Location location = currentLocation();
	lookedUpToken = consume();
	if(lookedUpToken.isUinteger()){
		//
		expression = expressionFactory->makeConstant();
		((ConstantExpression*)expression)->type       = arpha::uint64;
		((ConstantExpression*)expression)->_isLiteral = true;
		((ConstantExpression*)expression)->_uint64    = lookedUpToken.uinteger;
	}
	else if(lookedUpToken.isSymbol()){
		Definition* parselet = _currentScope->lookup(lookedUpToken.symbol);
		if(!parselet){ 
			error(location,"Can't prefix parse %s!",lookedUpToken); 
			expression = expressionFactory->makeError();
		}else{
			expression = parselet->prefixParse(this);
		}
	}
	else {
		error(location,"Can't prefix parse %s!",lookedUpToken);
		expression = expressionFactory->makeError();
	}
	expression->location = location;
	expression = evaluate(expression);

	//infix parsing
	//Token token;
	while(1){
		lookedUpToken = peek();
		if(lookedUpToken.isSymbol()){
			Definition* parselet = _currentScope->lookup(lookedUpToken.symbol);
			if(parselet && stickiness < parselet->stickiness){
				location = currentLocation();
				consume();
				expression = parselet->infixParse(this,expression);
				expression->location = location;
				expression = evaluate(expression);
			}
			else break;
		}else break;	
	}	
	return expression;	
}

Node* Definition::prefixParse(Parser* parser){
	error(parser->previousLocation(),"Can't prefix parse %s!",parser->lookedUpToken);
	return parser->expressionFactory->makeError();
}

Node* Definition::infixParse(Parser* parser,Node*){
	assert(false); //TODO throw
	return nullptr;
}

Node* OverloadSet::prefixParse(Parser* parser){
	return parser->expressionFactory->makeOverloadSet(parser->currentScope(),parser->lookedUpToken.symbol);
}

//arpha concepts
ParenthesisParser::ParenthesisParser(SymbolID open,SymbolID close,int sticky) : Definition(nullptr,open,sticky) {
	closer= close;
}
Node* ParenthesisParser::prefixParse(Parser* parser){
	if( parser->match(closer) )
		return parser->expressionFactory->makeUnit();
	auto e = parser->_parse();
	parser->expect(closer);
	return e;
}
Node* ParenthesisParser::infixParse(Parser* parser,Node* node){
	return parser->expressionFactory->makeCall(node,prefixParse(parser));
}

TupleParser::TupleParser(SymbolID op,int sticky) : Definition(nullptr,op,sticky) {}
Node* TupleParser::infixParse(Parser* parser,Node* node){
	return parser->expressionFactory->makeTuple(node,parser->_parse(stickiness));
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