#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/resolve.h"
#include "../ast/interpret.h"
#include "parser.h"
#include "../compiler.h"
#include "../intrinsics/types.h"

Parser::Parser(const char* src,CompilationUnit* compilationUnit) : Lexer(src),_compilationUnit(compilationUnit) {  
	_currentScope = nullptr;
	_outerMacroOuterScope = nullptr;
}
CompilationUnit* Parser::compilationUnit() const {
	return _compilationUnit;
}

void Parser::ignoreNewlines(){
	for(auto next = peek();next.isLine();next = peek()) consume();
}

Parser::NewlineIgnorer::NewlineIgnorer(bool doIgnore,Parser* parser) {	
	ignore = doIgnore;
	if(ignore){
		parser->saveState(&state);
		for(auto next = parser->peek();next.isLine();next = parser->peek()) parser->consume();
		this->parser = parser;
	}
}
void Parser::NewlineIgnorer::rollback(){
	if(ignore) parser->restoreState(&state);
}

void Parser::currentScope(Scope* scope){
	_currentScope = scope;
}
void Parser::enterBlock(BlockExpression* block){
	block->scope->setParent(_currentScope);
	_currentScope = block->scope;
}
void Parser::leaveBlock(){
	_currentScope = _currentScope->parent;
}

void Parser::expect(SymbolID token){
	Token tok = consume();
	if(!tok.isSymbol() || tok.symbol!=token){
		syntaxError(format("Unexpected token %s - '%s' is expected!",tok,token));
	}
}

bool Parser::match(SymbolID token){
	Token tok = peek();
	if(!tok.isSymbol() || tok.symbol!=token) return false;
	consume();
	return true;
}

bool Parser::match(int tokenType){
	//assert(tokenType >= Token::Symbol && tokenType <= Token::Eof); 
	Token tok = peek();
	if(tok.type != tokenType) return false;
	consume();
	return true;
}

SymbolID Parser::expectName(){
	Token tok = consume();
	if(!tok.isSymbol()){
		syntaxError(format("Unexpected token %s - a valid symbol is expected!",tok));
		return SymbolID("error-name");
	}
	return tok.symbol;
}

static Node* parseNotSymbol(Parser* parser){
	Token& token = parser->lookedUpToken;
	if(token.isUinteger()){
		return new IntegerLiteral(BigInt(token.uinteger));
	}
	else if(token.isReal()){
		return new FloatingPointLiteral(token.real);
	}
	else if(token.isString()){
		return new StringLiteral(token.string);
	}
	else{
		parser->syntaxError(format("Can't parse token %s!",token));
		return ErrorExpression::getInstance();
	}
}

/**
* Pratt parser is fucking awesome.
* TODO  definition caching for improved symbol lookup.
* TODO  investigate stack limitations.
*/
Node* Parser::parse(int stickiness){
	Node* expression;

	//prefix	
	lookedUpToken = consume();
	auto location = previousLocation();
	if(lookedUpToken.isSymbol()){
		auto next = peek();
		if(next.isSymbol() && next.symbol == ":"){
			auto label = lookedUpToken.symbol;
			consume();
			expression = parse(20);//TODO this is hacky..
			location = expression->location();
			expression->_label = label;
		}
		else{
			auto prefixDefinition = _currentScope->lookupPrefix(lookedUpToken.symbol);
			if(!prefixDefinition){ 
				expression = new UnresolvedSymbol(location,lookedUpToken.symbol);
			}else{
				expression = prefixDefinition->parse(this);
			}
		}
	}
	else expression = parseNotSymbol(this);
	expression->_location = location;

	//infix parsing
	while(1){
		lookedUpToken = peek();
		if(lookedUpToken.isSymbol()){
			auto infixDefinition = _currentScope->lookupInfix(lookedUpToken.symbol);
			if(infixDefinition && stickiness < infixDefinition->stickiness){
				consume();
				location = previousLocation();
				expression = infixDefinition->parse(this,expression);
				expression->_location = location;
			}
			else break;
		}else break;	
	}	
	return expression;	
}

//Definition properties application
// TODO remove this hacks!!
void Parser::useProperty(SymbolID name,Node* value){
	if(name == "precedence"){
		currentScope()->precedenceProperty = value;
	}
}
void Parser::useProperty(SymbolID name){
	if(name == "intrinsic"){
		//hacks
		currentScope()->externalFunction = 1;
	}
}
void Parser::clearProperties(){
}
void Parser::useTypedArgument(SymbolID name,Node* type){
}
void Parser::applyProperties(Node* node){
	if(currentScope()->externalFunction) node->applyProperty("intrinsic",nullptr);
	if(currentScope()->precedenceProperty && node->asInfixMacro()) node->applyProperty("precedence",currentScope()->precedenceProperty);
}

//introducing definitions
void Parser::introduceDefinition(Variable* variableDefinition){
	currentScope()->define(variableDefinition);
}
void Parser::introduceDefinition(Function* functionDefinition){
	if(functionDefinition->isFlagSet(Function::CONSTRAINT_FUNCTION)){
		currentScope()->define(functionDefinition);
	}
	else currentScope()->defineFunction(functionDefinition);
}
void Parser::introduceDefinition(Record* recordDefinition){
	currentScope()->define(recordDefinition);
}
void Parser::introduceDefinition(TypeDeclaration* typeDeclaration){
	currentScope()->define(typeDeclaration);
}
void Parser::introduceDefinition(PrefixMacro* macroDefinition){
	currentScope()->define(macroDefinition);
}
void Parser::introduceDefinition(InfixMacro* macroDefinition){
	currentScope()->define(macroDefinition);
}

//parsing declarations

Node* Parser::mixinMacroResult(CTFEinvocation* invocation){
	return mixinMacro(invocation,currentScope());
}

Node* ImportedScope::parse(Parser* parser) {
	return reference();
}

void TypePatternUnresolvedExpression::parse(Parser* parser,int stickiness){
	kind = UNRESOLVED;
	unresolvedExpression = parser->parse(stickiness);
}

Node* Variable::parse(Parser* parser){
	return new VariableReference(this);
}

Node* AggregateType::parse(Parser* parser){
	return new TypeReference(&_type);
}
Node* TypeDeclaration::parse(Parser* parser){
	return new TypeReference(_type);
}

Node* Function::parse(Parser* parser){
	return new FunctionReference(this);
}

Node* Overloadset::parse(Parser* parser){
	return new UnresolvedSymbol(parser->previousLocation(),parser->lookedUpToken.symbol);
}

Node* PrefixMacro::parse(Parser* parser){
	if(isResolved() || function->isFlagSet(Function::CANT_CTFE)){
		CTFEinvocation i(parser->compilationUnit(),function);
		if(i.invoke(nullptr)) return parser->mixinMacroResult(&i);
		else error(parser->previousLocation(),"Failed to interpret a macro '%s' at compile time:\n\tCan't interpret an expression %s!",label(),i.result());
	}
	else {
		if(isResolved()) error(parser->previousLocation(),"Can't parse macro '%s' - the macro can't be interpreted!",label());
		else error(parser->previousLocation(),"Can't parse macro '%s' - the macro isn't resolved at usage time!",label());
	}
	return ErrorExpression::getInstance();
}

Node* InfixMacro::parse(Parser* parser,Node* node){
	if(isResolved() || function->isFlagSet(Function::CANT_CTFE)){
		CTFEinvocation i(parser->compilationUnit(),function);
		if(i.invoke(new NodeReference(node))) return parser->mixinMacroResult(&i);
		else error(parser->previousLocation(),"Failed to interpret a macro '%s' at compile time:\n\tCan't interpret an expression %s!",label(),i.result());
	}
	else {
		if(isResolved()) error(parser->previousLocation(),"Can't parse macro '%s' - the macro can't be interpreted!",label());
		else error(parser->previousLocation(),"Can't parse macro '%s' - the macro isn't resolved at usage time!",label());
	}
	return ErrorExpression::getInstance();
}

