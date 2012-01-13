#include "common.h"
#include "scope.h"
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

Node* Parser::_parseModule(){
	//top level
	Node* block = expressionFactory->makeBlock();
	Token token;
	
	while(1){
		token = peek();
		if(token.isEOF()) break;
		else if(token.isEndExpression()){
			consume();
			continue;
		}

		((BlockExpression*)block)->children.push_back(_parse());

		token = consume();
		if(token.isEOF()) break;
		else if(!token.isEndExpression()) error(previousLocation(),"';' expected!");
	}

	if(((BlockExpression*)block)->children.size() == 0) error(previousLocation(),"the source file is empty!");

	return block;
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
		((ConstantExpression*)expression)->u64    = lookedUpToken.uinteger;
	}
	else if(lookedUpToken.isSymbol()){
		auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
		if(!prefixDefinition){ 
			error(location,"Can't prefix parse %s!",lookedUpToken); 
			expression = expressionFactory->makeError();
		}else{
			expression = prefixDefinition->parse(prefixDefinition,this);
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
			auto infixDefinition = _currentScope->lookupInfix(lookedUpToken.symbol);
			if(infixDefinition && stickiness < infixDefinition->stickiness){
				location = currentLocation();
				consume();
				expression = infixDefinition->parse(infixDefinition,this,expression);
				expression->location = location;
				expression = evaluate(expression);
			}
			else break;
		}else break;	
	}	
	return expression;	
}

Node* parseTypeExpression(PrefixDefinition* def,Parser* parser){
	return parser->expressionFactory->makeType((Type*)def->data);
}

Node* Definition::prefixParse(Parser* parser){
	error(parser->previousLocation(),"Can't prefix parse %s!",parser->lookedUpToken);
	return parser->expressionFactory->makeError();
}

Node* Definition::infixParse(Parser* parser,Node*){
	assert(false); //TODO throw
	return nullptr;
}

Node* Type::prefixParse(Parser* parser){
	assert(false); //TODO throw
	return nullptr;
}

Node* OverloadSet::prefixParse(Parser* parser){
	return parser->expressionFactory->makeOverloadSet(parser->currentScope(),parser->lookedUpToken.symbol);
}

