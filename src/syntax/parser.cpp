#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "parser.h"
#include "../compiler.h"
#include "../intrinsics/types.h"

Parser::Parser(const char* src) : Lexer(src) {  
}

void Parser::currentScope(Scope* scope){
	_currentScope=scope;
	firstRoundEvaluator.currentScope(scope);
}

void Parser::expect(SymbolID token){
	Token tok = consume();
	if(tok.isSymbol()==false || tok.symbol!=token){
		error(previousLocation(),"'%s' expected!",token);
	}
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
		error(previousLocation(),"A valid symbol is expected instead of %s!",tok);
		return SymbolID("error-name");
	}
	return tok.symbol;
}

Node* Parser::evaluate(Node* node){
	return firstRoundEvaluator.eval(node);
}


static Node* parseNotSymbol(Parser* parser){
	Token& token = parser->lookedUpToken;
	if(token.isUinteger()){
		return new IntegerLiteral(BigInt(token.uinteger));
	}else{
		error(parser->previousLocation(),"Unexpected token %s!",token);
		return ErrorExpression::getInstance();
	}
}

/**
* Pratt parser is fucking awsome.
*/
Node* Parser::parse(int stickiness){
	Node* expression;

	//prefix	
	auto location = currentLocation();
	lookedUpToken = consume();
	if(lookedUpToken.isSymbol()){
		auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
		if(!prefixDefinition){ 
			debug("line %s: Can't prefix parse %s at first round!",location.line(),lookedUpToken); //TODO unresolved name
			expression = new UnresolvedSymbol(location,lookedUpToken.symbol);
		}else{
			expression = prefixDefinition->parse(this);
		}
	}
	else expression = parseNotSymbol(this);
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
	return reference();
}

void InferredUnresolvedTypeExpression::parse(Parser* parser,int stickiness){
	auto oldSetting = parser->evaluator()->expectedTypeForEvaluatedExpression;
	parser->evaluator()->expectedTypeForEvaluatedExpression = intrinsics::types::Type;
	auto node = parser->parse(stickiness);
	parser->evaluator()->expectedTypeForEvaluatedExpression = oldSetting;

	auto isTypeExpr = node->asTypeExpression();
	if(isTypeExpr && isTypeExpr->isResolved()){
		kind = Type;
		_type = isTypeExpr;
		return;
	}
	kind = Unresolved;
	unresolvedExpression = node;
}

Node* Variable::parse(Parser* parser){
	return reference();
}

Node* Record::parse(Parser* parser){
	return reference();
}

Node* IntegerType::parse(Parser* parser){
	return reference();
}

Node* IntrinsicType::parse(Parser* parser){
	return reference();
}
#include "../intrinsics/types.h"//hacks
Node* PointerType::parse(Parser* parser){
	return new TypeExpression(this,intrinsics::types::int32);//TODO
}

Node* Function::parse(Parser* parser){
	return nullptr;//FunctionReference::create(this);//TODO remove?
}

Node* Overloadset::parse(Parser* parser){
	return new UnresolvedSymbol(parser->previousLocation(),parser->lookedUpToken.symbol);
}

Node* PrefixOperator::parse(Parser* parser){
	return CallExpression::create(new UnresolvedSymbol(parser->previousLocation(),function),parser->parse());
}

Node* InfixOperator::parse(Parser* parser,Node* node){
	auto tuple = new TupleExpression;
	tuple->children.push_back(node);
	tuple->children.push_back(parser->parse(stickiness));
	return CallExpression::create(new UnresolvedSymbol(parser->previousLocation(),function),tuple);
}
