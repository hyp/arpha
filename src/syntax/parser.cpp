#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "parser.h"
#include "../compiler.h"
#include "../intrinsics/types.h"

Parser::Parser(const char* src,Evaluator* evaluator) : Lexer(src),_evaluator(evaluator) {  
}

void Parser::saveState(State *state){
	state->src= ptr;
	state->location = location;
}
void Parser::restoreState(State *state){
	ptr = state->src;
	location = state->location;
}

void Parser::currentScope(Scope* scope){
	_currentScope=scope;
	_evaluator->currentScope(scope);
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
	return _evaluator->eval(node);
}


static Node* parseNotSymbol(Parser* parser){
	Token& token = parser->lookedUpToken;
	if(token.isUinteger()){
		return new IntegerLiteral(BigInt(token.uinteger));
	}
	else if(token.isString()){
		return new StringLiteral(token.string);
	}
	else{
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
		auto next = peek();
		if(next.isSymbol() && next.symbol == ":"){//TODo macroes featuring ':'
			labelForNextNode = lookedUpToken.symbol;
			consume();
			return parse(stickiness);
		}
		else{
			auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
			if(!prefixDefinition){ 
				debug("line %s: Can't prefix parse %s at first round!",location.line(),lookedUpToken); //TODO unresolved name
				expression = new UnresolvedSymbol(location,lookedUpToken.symbol);
			}else{
				expression = prefixDefinition->parse(this);
			}
		}
	}
	else expression = parseNotSymbol(this);
	expression->location = location;
	expression->_label = labelForNextNode;
	labelForNextNode = SymbolID();
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

void TypePatternUnresolvedExpression::parse(Parser* parser,int stickiness){
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
	return new VariableReference(this);
}

Node* Record::parse(Parser* parser){
	return new TypeExpression(this);
}

Node* IntegerType::parse(Parser* parser){
	return new TypeExpression(this);
}

Node* IntrinsicType::parse(Parser* parser){
	return reference();
}

Node* Function::parse(Parser* parser){
	return new FunctionReference(this);
}


Node* Overloadset::parse(Parser* parser){
	return new UnresolvedSymbol(parser->previousLocation(),parser->lookedUpToken.symbol);
}
