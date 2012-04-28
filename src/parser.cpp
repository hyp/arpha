#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "ast/node.h"
#include "compiler.h"
#include "arpha.h"

Parser::Parser(const char* src,Scope* scope) : Lexer(src) { _currentScope=scope; }

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
	if(tok.isSymbol()==false){
		error(previousLocation(),"A valid name is expected!");
		return SymbolID("error-name");
	}
	return tok.symbol;
}

int Parser::expectInteger(){
	auto node = _parse(arpha::Precedence::Tuple);
	if(auto c= node->asConstantExpression()){
		if(arpha::isInteger(c->type)){
			return int(c->u64);
		}
	}
	error(node->location,"Expected an integer constant instead of %s!",node);
	return -1;
}

Node* Parser::_parseModule(){
	//top level
	auto block = BlockExpression::create();
	Token token;
	
	while(1){
		token = peek();
		if(token.isEOF()) break;
		else if(token.isEndExpression()){
			consume();
			continue;
		}

		block->children.push_back(_parse());

		token = consume();
		if(token.isEOF()) break;
		else if(!token.isEndExpression()) error(previousLocation(),"';' expected!");
	}

	if(block->children.size() == 0) error(previousLocation(),"the source file is empty!");

	return block;
}

Node* Parser::_parse(int stickiness){
	
	Node* expression;

	//prefix	
	Location location = currentLocation();
	lookedUpToken = consume();
	if(lookedUpToken.isUinteger()){
		auto cnst = ConstantExpression::create(arpha::uint64);
		expression = static_cast<Node*>(cnst);
		cnst->_isLiteral = true;
		cnst->u64    = lookedUpToken.uinteger;
	}
	else if(lookedUpToken.isString()){
		auto cnst = ConstantExpression::create(arpha::constantString);
		expression = static_cast<Node*>(cnst);
		cnst->_isLiteral = true;
		cnst->string.aquire(lookedUpToken.string);
	}
	else if(lookedUpToken.isSymbol()){
		auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
		if(!prefixDefinition){ 
			error(location,"Can't prefix parse %s!",lookedUpToken); 
			expression = ConstantExpression::create(compiler::Error);
		}else{
			expression = prefixDefinition->parse(this);
		}
	}
	else {
		error(location,"Can't prefix parse %s!",lookedUpToken);
		expression = ConstantExpression::create(compiler::Error);
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
				expression = infixDefinition->parse(this,expression);
				expression->location = location;
				expression = evaluate(expression);
			}
			else break;
		}else break;	
	}	
	return expression;	
}

Type* Parser::parseOptionalType(){
	auto next = peek();
	if(next.isEOF() || next.isEndExpression()) return nullptr;
	const char* prevptr = ptr;
	auto node = _parse(arpha::Precedence::Assignment);
	const ConstantExpression* val;
	if( (val = node->asConstantExpression()) && val->type == compiler::type) return val->refType;
	ptr = prevptr;
	return nullptr;
}


//parsing declarations

Node* ImportedScope::parse(Parser* parser) {
	return ConstantExpression::createScopeReference(scope);
}
Node* Substitute::parse(Parser* parser){
	return expression; //TODO duplicate
}

Node* Variable::parse(Parser* parser){
	return VariableExpression::create(this);
}

Node* Type::parse(Parser* parser){
	return ConstantExpression::createTypeReference(this);
}

Node* Function::parse(Parser* parser){
	return ConstantExpression::createFunctionReference(this);
}

Node* Overloadset::parse(Parser* parser){
	return OverloadSetExpression::create(parser->lookedUpToken.symbol,parser->currentScope());
}

Node* PrefixOperator::parse(Parser* parser){
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),parser->_parse());
}

Node* InfixOperator::parse(Parser* parser,Node* node){
	auto tuple = TupleExpression::create();
	tuple->children.push_back(node);
	tuple->children.push_back(parser->_parse(stickiness));
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),tuple);
}
