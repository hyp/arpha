#include "../common.h"
#include "../scope.h"
#include "../declarations.h"
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
	if(auto c= node->asConstantExpression()){
		if(arpha::isInteger(c->type)){
			return int(c->u64);
		}
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

Type* Parser::expectType(int stickiness){
	auto loc = currentLocation();
	auto node = parse(stickiness);
	const ConstantExpression* val;
	if( (val = node->asConstantExpression()) && val->type == compiler::type) return val->refType;
	error(loc,"Expected a valid type instead of %s!",node);
	return compiler::Error;
}
Type* Parser::matchType(int stickiness){
	//TODO disbale error reporting upon matching
	auto next = peek();
	if(next.isEOF() || next.isLine()) return nullptr;
	const char* prevptr = ptr;
	auto node = parse(stickiness);
	const ConstantExpression* val;
	if( (val = node->asConstantExpression()) && val->type == compiler::type) return val->refType;
	ptr = prevptr;
	return nullptr;
}
std::pair<Type*,Node*> Parser::expectTypeOrUnresolved(int stickiness){
	auto loc = currentLocation();
	auto node = parse(stickiness);
	auto t = node->returnType();
	const ConstantExpression* val;
	if( (val = node->asConstantExpression()) && t == compiler::type) return std::make_pair(val->refType,nullptr);
	else {
		if(t == compiler::Unresolved || t == compiler::type) return std::make_pair(nullptr,node);
	}
	error(loc,"Expected a valid type instead of %s!",node);
	return std::make_pair(compiler::Error,nullptr);
}


//parsing declarations

Node* ImportedScope::parse(Parser* parser) {
	return ConstantExpression::createScopeReference(scope);
}

Node* Variable::parse(Parser* parser){
	return VariableExpression::create(this);
}

Node* Type::parse(Parser* parser){
	return resolved? static_cast<Node*>(ConstantExpression::createTypeReference(this)) : static_cast<Node*>(UnresolvedDeclaration::create(this));
}

Node* Function::parse(Parser* parser){
	return ConstantExpression::createFunctionReference(this);//TODO remove?
}

Node* Overloadset::parse(Parser* parser){
	return OverloadSetExpression::create(parser->lookedUpToken.symbol,parser->currentScope());
}

Node* PrefixOperator::parse(Parser* parser){
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),parser->parse());
}

Node* InfixOperator::parse(Parser* parser,Node* node){
	auto tuple = TupleExpression::create();
	tuple->children.push_back(node);
	tuple->children.push_back(parser->parse(stickiness));
	return CallExpression::create(OverloadSetExpression::create(function,parser->currentScope()),tuple);
}
