#include "../common.h"
#include "../scope.h"
#include "../ast/declarations.h"
#include "parser.h"

#include "../ast/node.h"
#include "../compiler.h"
#include "../arpha.h"

Parser::Parser(const char* src) : Lexer(src) {  
}

void Parser::currentScope(Scope* scope){
	_currentScope=scope;
	firstRoundEvaluator.currentScope(scope);
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

SymbolID Parser::expectName(){
	Token tok = consume();
	if(tok.isSymbol()==false){
		error(previousLocation(),"A valid name is expected!");
		return SymbolID("error-name");
	}
	return tok.symbol;
}

int Parser::expectInteger(){
	auto node = parse(arpha::Precedence::Tuple);
	if(auto c= node->asIntegerLiteral()){
		return int(c->integer.u64); //TODO this is potentially unsafe
	}
	error(node->location,"Expected an integer constant instead of %s!",node);
	return -1;
}

Node* Parser::evaluate(Node* node){
	return firstRoundEvaluator.eval(node);
}


/**
* Pratt parser is fucking awsome.
*/
Node* Parser::parse(int stickiness){
	
	Node* expression;

	//prefix	
	Location location = currentLocation();
	lookedUpToken = consume();
	if(lookedUpToken.isUinteger()){
		expression = new IntegerLiteral(BigInt(lookedUpToken.uinteger));
	}
	else if(lookedUpToken.isSymbol()){
		auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
		if(!prefixDefinition){ 
			error(location,"Can't prefix parse %s!",lookedUpToken); 
			expression = ErrorExpression::getInstance();
		}else{
			expression = prefixDefinition->parse(this);
		}
	}
	else {
		error(location,"Can't prefix parse %s!",lookedUpToken);
		expression = ErrorExpression::getInstance();
	}
	expression->location = location;
	expression = evaluate(expression);

	//infix parsing
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


//parsing declarations

Node* ImportedScope::parse(Parser* parser) {
	return nullptr;//TODo ConstantExpression::createScopeReference(scope);
}

Node* Variable::parse(Parser* parser){
	return new VariableReference(this);
}

Node* Record::parse(Parser* parser){
	return new TypeExpression(this);
}

Node* IntegerType::parse(Parser* parser){
	return new TypeExpression(this);
}

Node* Function::parse(Parser* parser){
	return nullptr;//FunctionReference::create(this);//TODO remove?
}

Node* Overloadset::parse(Parser* parser){
	return OverloadSetExpression::create(parser->lookedUpToken.symbol,parser->currentScope());
}

Node* PrefixOperator::parse(Parser* parser){
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),parser->parse());
}

Node* InfixOperator::parse(Parser* parser,Node* node){
	auto tuple = new TupleExpression;
	tuple->children.push_back(node);
	tuple->children.push_back(parser->parse(stickiness));
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),tuple);
}
