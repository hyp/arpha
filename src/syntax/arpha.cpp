/**
* This module implements arpha's core syntax macroes
*/

#include "../compiler.h"
#include "../base/symbol.h"
#include "../ast/scope.h"
#include "../ast/node.h"
#include "../ast/declarations.h"
#include "../ast/resolve.h"
#include "../syntax/parser.h"
#include "../intrinsics/types.h"
#include "../data/data.h"

//allow newlines before '{' e.g. def foo() \n { .. } 
//NB: It's important to keep consistency!
#define SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE 1 

namespace arpha {

	namespace Precedence {
		enum {
			Assignment = 10, // =
			Tuple = 20, // ,
			Call = 110, //()
			Access = 120, //.
		};
	}

	void defineCoreSyntax(Scope* scope);
	BlockExpression* parseModule(Parser* parser,BlockExpression* block);
};


// parses blocks and whatnot
// body ::= {';'|newline}* expression {';'|newline}+ expressions...
// ::= {body|'{' body '}'}
struct BlockParser: IntrinsicPrefixMacro {
	SymbolID lineAlternative; //AKA the ';' symbol
	SymbolID closingBrace;    //'}'

	BlockParser(): IntrinsicPrefixMacro("{") {
		lineAlternative = ";";
		closingBrace = "}";
	}

	struct BlockChildParser {
		BlockExpression* _block;
		BlockChildParser(BlockExpression* block) : _block(block) {}
		bool operator ()(Parser* parser){
			_block->addChild(parser->parse());
			if(parser->mixinedExpressions.size()){
				for(auto i = parser->mixinedExpressions.begin();i!=parser->mixinedExpressions.end();i++){
					if(auto f = (*i)->asFunction()) parser->introduceDefinition(f);
					_block->addChild(*i);
				}
				parser->mixinedExpressions.clear();
			}
			return true;
		}
	};

	void skipExpression(Parser* parser,bool matchClosingBrace){	
		for(Token token;;parser->consume()){
			token = parser->peek();
			//TODO correctly skip potential inner {} blocks
			if(token.isLine() || (token.isSymbol() && token.symbol == lineAlternative) 
				|| (matchClosingBrace && token.isSymbol() && token.symbol == closingBrace) || token.isEOF()) return;	
		}
	}
	template<class F>
	void body(Parser* parser,F functor,bool matchClosingBrace = true,bool acceptEOF = false){
		Token token;
		bool isSym;
		while(1){
			token = parser->peek();
			isSym = token.isSymbol();
			//account for useless ';'|newlines - i.e. { ';' ';' expr ';' ';' expr ';' }
			if(token.isLine() || (isSym && token.symbol == lineAlternative)){
				parser->consume();
				continue;
			}
			//account for standart '}' - i.e. { expr ; expr ; }. Also accounts for the '{' '}' case.
			else if(matchClosingBrace && isSym && token.symbol == closingBrace){
				parser->consume();
				break;
			}
			else if(acceptEOF && token.isEOF()) break; //expr ';' EOF case
			//If the functor returned an error, skip till ';'|newline
			if(!functor(parser)) skipExpression(parser,matchClosingBrace);
			//Expect a ';'|newline|'}'
			token = parser->consume();
			isSym = token.isSymbol();		
			if(matchClosingBrace && isSym && token.symbol == closingBrace) break; //account for no closing ';' on last field - i.e. { expr ; expr }
			else if(!(token.isLine() || (isSym && token.symbol == lineAlternative))){
				if(token.isEOF()){
					if(!acceptEOF) parser->syntaxError(format("Expected '%s' before the end of the file!",closingBrace));
					break;
				}
				if(matchClosingBrace) parser->syntaxError(format("Expected a newline,'%s' or a '%s' before %s!",lineAlternative,closingBrace,token));
				else parser->syntaxError(format("Expected a newline or a '%s' before %s!",lineAlternative,token));
			}
		}
	}

	//On '{'
	Node* parse(Parser* parser){
		BlockExpression* block = new BlockExpression();
		parser->enterBlock(block);
		body(parser,BlockChildParser(block));
		parser->leaveBlock();
		return block;
	}
};

namespace {

BlockParser* blockParser;

}

// parses an arpha module
// ::= {EOF|block.body EOF}
BlockExpression* arpha::parseModule(Parser* parser,BlockExpression* block){
	parser->enterBlock(block);
	blockParser->body(parser,BlockParser::BlockChildParser(block),false,true); //Ignore '}' and end on EOF
	parser->leaveBlock();
	return block;
}

/// ::= '(' expression ')'
struct ParenParser: IntrinsicPrefixMacro {
	SymbolID closingParenthesis;
	ParenParser(): IntrinsicPrefixMacro("(") {
		closingParenthesis = ")";
	}
	Node* parse(Parser* parser){
		if( parser->match(closingParenthesis) )
			return new UnitExpression;
		auto e = parser->parse();
		parser->expect(closingParenthesis);
		return e;
	}
};

/// ::= expression '(' expression ')'
struct CallParser: IntrinsicInfixMacro {
	SymbolID closingParenthesis;
	CallParser(): IntrinsicInfixMacro("(",arpha::Precedence::Call) {
		closingParenthesis = ")";
	}
	Node* parse(Parser* parser,Node* node){
		Node* arg;
		if( parser->match(closingParenthesis) ) arg = new UnitExpression;
		else{
			arg = parser->parse();
			parser->expect(closingParenthesis);
		}
		return new CallExpression(node,arg);
	}
};

/// ::= expression ',' expression
struct TupleParser: IntrinsicInfixMacro {
	TupleParser(): IntrinsicInfixMacro(",",arpha::Precedence::Tuple) { }
	Node* parse(Parser* parser,Node* node){
		auto tuple = new TupleExpression;
		tuple->children.push_back(node);
		do tuple->addChild(parser->parse(arpha::Precedence::Tuple));
		while(parser->match(","));
		return tuple;
	}
};

/// ::= expression '.' expression
struct AccessParser: IntrinsicInfixMacro {
	AccessParser(): IntrinsicInfixMacro(".",arpha::Precedence::Access) { }
	Node* parse(Parser* parser,Node* node){
		parser->lookedUpToken.type = Token::Symbol;
		parser->lookedUpToken.symbol = parser->expectName();
		//scope.something
		if(auto val = node->asImportedScopeReference()){
			//next in import tree?
			auto var = val->scope->importTree.find(parser->lookedUpToken.symbol);
			if (var != val->scope->importTree.end()){
				return var->second->reference();
			}
			else if(val->scope->scope){
				auto def = val->scope->scope->lookupImportedPrefix(parser->lookedUpToken.symbol);
				if(!def){
					error(node,"Symbol '%s' isn't defined in module '%s'!",parser->lookedUpToken.symbol,val->scope->label());
					return ErrorExpression::getInstance();
				}
				auto expression = def->parse(parser);
				//apply the correct overload lookup scope
				if(auto overloadSet = expression->asUnresolvedSymbol()) overloadSet->explicitLookupScope = val->scope->scope;
				return expression;
			}else{
				error(node,"A module '%s' isn't imported from package '%s'!",parser->lookedUpToken.symbol,val->scope->label());
				return ErrorExpression::getInstance();
			}
		}
		return new AccessExpression(node,parser->lookedUpToken.symbol);
	}
};

/// ::= expression '=' [Newlines] expression
struct AssignmentParser: IntrinsicInfixMacro {
	AssignmentParser(): IntrinsicInfixMacro("=",arpha::Precedence::Assignment) { }
	Node* parse(Parser* parser,Node* node){
		parser->ignoreNewlines();//NB: for consistency with other usages of '='
		return new AssignmentExpression(node,parser->parse(arpha::Precedence::Assignment-1)); //left associative(NOT NEEDED,SINCE '=' is now a statement!)
	}
};

bool isEndExpression(const Token& token){
	return token.isEOF() || token.isLine() || (token.isSymbol() && token.symbol == blockParser->lineAlternative );
}
bool isEndExpressionEquals(const Token& token){
	return isEndExpression(token) || (token.isSymbol() && token.symbol == "=");
}

/// names     ::= <name> ',' names | <name>
/// variable  ::= 'var' names [type|unresolvedExpression (hopefully) resolving to type|Nothing] [ '=' [Newlines] initializers ]
struct VarParser: IntrinsicPrefixMacro {
	VarParser(): IntrinsicPrefixMacro("var") {}
	static Node* parseVar(Parser* parser,SymbolID first,bool isMutable){
		std::vector<Variable*> vars;

		auto var = new Variable(first,parser->previousLocation());
		var->isMutable = isMutable;
		parser->applyProperties(var);
		if(!isMutable && parser->compilationUnit()->moduleBody->scope->importsArphaIntrinsic) var->applyProperty("intrinsic",nullptr);
		parser->introduceDefinition(var);
		vars.push_back(var);
		while(parser->match(",")){
			auto name = parser->expectName();
			var = new Variable(name,parser->previousLocation());
			var->isMutable = isMutable;
			parser->applyProperties(var);
			parser->introduceDefinition(var);
			vars.push_back(var);
		}

		//parse optional type
		TypePatternUnresolvedExpression type;
		if(!isEndExpressionEquals(parser->peek())) type.parse(parser,arpha::Precedence::Assignment);
		for(auto i=vars.begin();i!=vars.end();i++){
			(*i)->type = type;
		}
		
		Node* result;
		if(vars.size() == 1)result = vars[0];
		else{
			auto tuple = new TupleExpression;
			for(auto i=vars.begin();i!=vars.end();i++){
				tuple->addChild(*i);
			}
			result = tuple;
		}
		//Initial assignment
		if(parser->match("=")){
			parser->ignoreNewlines();//NB: for consistency with other usages of '='
			auto assign = new AssignmentExpression(result,parser->parse(arpha::Precedence::Assignment-1)); 
			assign->_location = parser->currentLocation();
			assign->isInitializingAssignment = true;
			return assign;
		}
		else return result;
	}
	Node* parse(Parser* parser){
		return parseVar(parser,parser->expectName(),true);
	}
};

/// param     ::= <name> [Type] [ '=' defaultValue ]
/// paramList ::= param ',' paramList | param
/// params    ::= '(' paramList ')' | '(' ')'
void parseFunctionParameters(Parser* parser,Function* func,Type* defaultType = nullptr){
	if(!parser->match(")")){
		while(1){
			auto location = parser->currentLocation();
			auto argName  = parser->expectName();
			auto param = new Argument(argName,location,func);
		
			auto next = parser->peek();
			if(next.isSymbol() && ( next.symbol == "," || next.symbol == ")" || next.symbol == "=")){
				if(defaultType) param->type.specify(defaultType);
			}else{
				param->type.parse(parser,arpha::Precedence::Tuple);
				next = parser->peek();
			}

			//parameter's default value
			if(next.isSymbol() && next.symbol == "="){
				parser->consume();
				param->defaultValue(parser->parse(arpha::Precedence::Tuple));
			}
			func->addArgument(param);

			if(parser->match(")")) break;
			parser->expect(",");
		}
	}
}

/// functionBody   ::= nothing | '=' expression | '{' block '}'
//TODO: maybe make this more eficcient by moving up the '=' test
void parseFunctionBody(Parser* parser,Function* func,bool allowNoBody = false){
	//NB: need to take \n '{' into account!
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
	Parser::NewlineIgnorer i(true,parser);
	if(parser->match("{")){
		blockParser->body(parser,BlockParser::BlockChildParser(&func->body));
		return;
	}
	else i.rollback();
#endif
	if(isEndExpression(parser->peek())){
		if(!allowNoBody) error(parser->previousLocation(),"The function %s needs to have a body( You can use either '=' expression or '{' body '}')!",func->label());
		return;
	}

	if(parser->match("=")){
		parser->ignoreNewlines();//NB: for consistency with other usages of '='
		auto ret = new ReturnExpression(parser->parse());
		ret->_location = ret->expression->location();
		func->body.addChild(ret);
	}
	else {
		parser->expect("{");
		blockParser->body(parser,BlockParser::BlockChildParser(&func->body));
	}
}

// TODO contract requirements?
static Function* parseFunction(SymbolID name,Parser* parser){
	//Function
	auto func = new Function(name,parser->previousLocation());
	//
	if(parser->compilationUnit()->moduleBody->scope->importsArphaIntrinsic) func->applyProperty("intrinsic",nullptr);
	parser->applyProperties(func);
	parser->introduceDefinition(func);

	parser->enterBlock(&func->body);
	//parse arguments
	parseFunctionParameters(parser,func);
	//return type & body
	auto token = parser->peek();
	if(!isEndExpression(token) && !(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
		func->_returnType.parse(parser,arpha::Precedence::Assignment);
	}
	parseFunctionBody(parser,func,func->isIntrinsic());
	parser->leaveBlock();

	return func;
}

enum MethodContext {
	MethodContextInterface,//allows only declarations, no implementation, this arg is virtual
	MethodContextTrait,    //allows declarations as requirements, and implementation
	MethodContextType
};

// TODO contract requirements
// TODO interface no body check
static Function* parseMethod(Parser* parser,Type* thisType,SymbolID name,MethodContext context){
	auto func = new Function(name,parser->previousLocation());

	parser->enterBlock(&func->body);
	//parse arguments
	if(context == MethodContextType){
		auto arg = new Argument("self",parser->previousLocation(),func);
		arg->type.unresolvedExpression = new TypeReference(new Type(Type::POINTER,thisType));
		arg->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
		func->addArgument(arg);
	}
	else parser->expect("(");
	parseFunctionParameters(parser,func);
	//return type & body
	auto token = parser->peek();
	if(!isEndExpression(token) && !(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
		func->_returnType.parse(parser,arpha::Precedence::Assignment);
	}
	parseFunctionBody(parser,func,context != MethodContextType);
	parser->leaveBlock();

	return func;	
}

// TODO
static void parseTypeInvariant(Parser* parser,TypeDeclaration* typeDecl){
	parser->syntaxError(format("Invariants aren't supported yet"));
}

void parseProperties(Parser* parser){
	do {
		auto prop = parser->expectName();
		Node* value = nullptr;
		if(parser->match(":")) value = parser->parse(arpha::Precedence::Tuple);
		//if(value) parser->useProperty(prop,value);
		//else parser->useProperty(prop);
	} while(parser->match(","));
	parser->expect(")");
}

/// constant  ::= 'def' names  '=' [Newlines] initializers
/// function  ::= 'def' <name> params [returnType] functionBody
struct DefParser: IntrinsicPrefixMacro {
	DefParser(): IntrinsicPrefixMacro("def") {  }

	Node* parse(Parser* parser){
		if(parser->match("(")) parseProperties(parser);
		auto name = parser->expectName();
		Node* result;
		if(parser->match("(")) result = parseFunction(name,parser);
		else result = VarParser::parseVar(parser,name,false);
		return result;
	}
};

/// macro     ::= 'macro' [ '(' <infixParam> ')' ] <name> functionBody
struct MacroParser: IntrinsicPrefixMacro {
	MacroParser(): IntrinsicPrefixMacro("macro") {  }
	Node* parse(Parser* parser){
		auto location  = parser->previousLocation();
		Argument* infix = nullptr;
		//create a macro processing function
		bool setOuterScope = false;
		if(!parser->_outerMacroOuterScope){
			parser->_outerMacroOuterScope = parser->currentScope();
			setOuterScope = true;
		}
		Function* func;

		//infix?
		if(parser->match("(")){
			auto argName = parser->expectName();
			parser->expect(")");
			func  = new Function(parser->expectName(),location);
			infix = new Argument(argName,parser->previousLocation(),func); 
			infix->specifyType(intrinsics::types::NodePointer);
			func->addArgument(infix);
		}
		else func = new Function(parser->expectName(),location);
		func->setFlag(Function::MACRO_FUNCTION);
		func->_returnType.specify(intrinsics::types::NodePointer);
		parser->enterBlock(&func->body);

		//Function like?
		bool functionLike = false;
		if(!infix && parser->match("(")){
			functionLike = true;//Function on the outside, but macro lays within!
			parseFunctionParameters(parser,func);
			for(auto i = func->arguments.begin();i!=func->arguments.end();i++){
				(*i)->hideType(intrinsics::types::NodePointer);
			}
		}
		
		//import defaults.
		func->body.scope->import(compiler::findModule("arpha/ast"),"ast",true,false);
		if(!functionLike) func->body.scope->import(compiler::findModule("arpha/syntax/parser"),"parser",true,false);
		parseFunctionBody(parser,func,false);
		
		parser->leaveBlock();
		if(setOuterScope) parser->_outerMacroOuterScope = nullptr;
		//Create the actual macro
		if(!infix){
			if(!functionLike){
				auto macro = new PrefixMacro(func);
				parser->applyProperties(macro);
				parser->introduceDefinition(macro);
				return parser->compilationUnit()->resolver->resolveMacroAtParseStage(macro);
			} 
			else {
				parser->introduceDefinition(func);
				return func;
			}
		} else {
			auto macro = new InfixMacro(func,new IntegerLiteral((uint64)0));
			parser->applyProperties(macro);
			parser->introduceDefinition(macro);
			return parser->compilationUnit()->resolver->resolveMacroAtParseStage(macro);
		}
	}
};

//TODO remove?
struct ConstraintParser: IntrinsicPrefixMacro {
	ConstraintParser(): IntrinsicPrefixMacro("constraint") {}
	Node* parse(Parser* parser){
		
		auto name       = parser->expectName();
		auto location   = parser->previousLocation();
		auto constraint = new Function(name,location);
		constraint->setFlag(Function::CONSTRAINT_FUNCTION);

		parser->enterBlock(&constraint->body);
		parser->expect("(");
		auto arg = new Argument(parser->expectName(),parser->previousLocation(),constraint);
		parser->expect(")");
		arg->specifyType(intrinsics::types::Type);
		constraint->addArgument(arg);
		constraint->specifyReturnType(intrinsics::types::boolean);
		
		parseFunctionBody(parser,constraint);
		parser->leaveBlock();

		parser->introduceDefinition(constraint);
		return constraint;
	}
};

// ::= '(' params ')'
Function* parseTypeTemplateDeclaration(SymbolID name,Parser* parser){
	auto func = new Function(name,parser->previousLocation());
	parser->enterBlock(&func->body);
	parseFunctionParameters(parser,func,intrinsics::types::Type);
	func->setFlag(Function::TYPE_GENERATOR_FUNCTION);
	func->makeAllArgumentsExpendable();
	return func;
}

/// requirement    ::= 'def' <name> params [returnType]
/// implementation ::= 'def' <name> params [returnType] functionBody
/// declaration    ::= requirement | implementation | invariant
/// declarations   ::= declaration (';'|Newlines) declarations | declaration
/// trait          ::= 'trait' <name> ( '{' declarations '}' ) | ('{' '}')
/// TODO proper method handling
/// TODO trait inheritance
struct ConceptParser: IntrinsicPrefixMacro {
	ConceptParser(): IntrinsicPrefixMacro("trait") {}

	struct BodyParser {
		Trait* trait;
		BodyParser(Trait* _trait) : trait(_trait) {}
		bool operator()(Parser* parser){
			auto  cmd = parser->expectName();
			if(cmd == "def"){
				//method requirement/implementation
				auto func = parseMethod(parser,trait,parser->expectName(),MethodContextTrait);
				
			}
			else if(cmd == "invariant") 
				parseTypeInvariant(parser,trait->declaration);
			else{
				parser->syntaxError(format("Can't parse a command '%s' inside trait declaration",cmd));
				return false;
			}
			return true;
		}
	};

	Node* parse(Parser* parser){
		auto trait = new Trait();
		auto decl  = new TypeDeclaration(trait,parser->expectName());
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		blockParser->body(parser,BodyParser(trait));
		return decl;
	}
};

/// interface ::= 'interface' <name> 
//  TODO
struct InterfaceParser: IntrinsicPrefixMacro {
	InterfaceParser(): IntrinsicPrefixMacro("interface") {}
	Node* parse(Parser* parser){
		auto name       = parser->expectName();

		parser->syntaxError(format("Interfaces aren't implemented yet!"));
		return new UnitExpression();
	}
};

void createFieldGettersSetters(Parser* parser,Type* thisType,SymbolID field,int fieldID,bool isPrivate,bool isReadonly){
	auto location = parser->previousLocation();
	//getter
	auto getter = new Function(field,location);
	auto gthis = new Argument("self",location,getter);
	gthis->type.unresolvedExpression = new TypeReference(new Type(Type::POINTER,thisType));
	gthis->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
	getter->addArgument(gthis);
	getter->makeFieldAccess(fieldID);
	
	//setter
	auto setter = new Function(field,location);
	auto sthis  = new Argument("self",location,setter);
	sthis->type.unresolvedExpression = new TypeReference(new Type(Type::POINTER,thisType));
	sthis->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
	setter->addArgument(sthis);
	auto value = new Argument("value",location,setter);
	setter->addArgument(value);
	setter->makeFieldAccess(fieldID);

	parser->mixinedExpressions.push_back(getter);
	parser->mixinedExpressions.push_back(setter);
}

/// fields       ::= ['private'] 'var'|'def' names (type | type '=' [Newlines] initializers | '=' [Newlines] initializers)
/// method       ::= 'def' <name> params [returnType] functionBody
/// declaration  ::= fields | method
/// declarations ::= declaration (';'|Newlines) declarations | declaration
/// type         ::= 'type' <name> '{' declarations '}'

/// TODO field initializers, private fields
/// TODO stuff
struct TypeParser: IntrinsicPrefixMacro {
	TypeParser(): IntrinsicPrefixMacro("type") {  }

	/// fields ::= ['private'] ['extends'] 'var' | 'def' names [ type ] '=' [Newlines] initializers
	enum { FIELDS_EXT = 0x1,FIELDS_DEF = 0x2,FIELDS_PRIVATE = 0x4 };
	static void fields(SymbolID first,Parser* parser,Record* record,int flags = 0){
		size_t i = record->fields.size();
		bool isExtending = (flags & FIELDS_EXT)!=0;

		auto field = Record::Field(first,intrinsics::types::Void);
		field.isExtending = isExtending;
		//TODO private
		record->add(field);
		createFieldGettersSetters(parser,record->asType(),field.name,record->fields.size()-1,false,false);
		while(parser->match(",")) {
			auto field = Record::Field(parser->expectName(),intrinsics::types::Void);
			field.isExtending = isExtending;
			//TODO private
			record->add(field);
			createFieldGettersSetters(parser,record->asType(),field.name,record->fields.size()-1,false,false);
		}
		
		TypePatternUnresolvedExpression type;
		type.parse(parser,arpha::Precedence::Assignment);
		for(;i<record->fields.size();i++) record->fields[i].type = type;
		if(parser->match("=")){
			parser->ignoreNewlines();//NB: for consistency with other usages of '='
			auto initializers = parser->parse();
			//TODO integrate initializers
		}
	}

	// body ::= '{' fields ';' fields ... '}'
	struct BodyParser {
		Record* record;
		BodyParser(Record* _record) : record(_record) {}
		bool operator()(Parser* parser){
			auto  cmd = parser->expectName();
			int flags = 0;
			if(cmd == "private"){
				flags |= FIELDS_PRIVATE;
				cmd = parser->expectName();
			}
			if(cmd == "type"){
				//parse type..
				return true;
			}
			if(cmd == "extends"){
				flags |= FIELDS_EXT;
				cmd = parser->expectName();
			}
			bool isDef = cmd == "def";
			if(cmd == "var" || isDef){
				auto name = parser->expectName(); //first field .. Need to check for '(' cause of def!
				if(isDef){
					if(parser->match("(")){
						if(flags & FIELDS_EXT) parser->syntaxError(format("Can't use property 'extends' on a function declaration!"));
						else {
							auto func = parseMethod(parser,record->asType(),name,MethodContextType);
							parser->mixinedExpressions.push_back(func);
							return true;
						}
					}
					flags |= FIELDS_DEF;
				}
				fields(name,parser,record,flags );
			}
			else{
				parser->syntaxError(format("Can't parse a command '%s' inside type declaration",cmd));
				return false;
			}
			return true;
		}
	};

	Node* parse(Parser* parser){
		auto name      = parser->expectName();
		auto location  = parser->previousLocation();

		Function* typeGenerationFunction = nullptr;

		if(parser->match("(")) typeGenerationFunction = parseTypeTemplateDeclaration(name,parser);

		Node* result;
		
		/*if(parser->match("=")){
			parser->syntaxError(format("NOT ALLOWED >_<!"));
		}*/
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		//record
		auto record = new Record(name,location);
		parser->introduceDefinition(record);
		result = record;
		blockParser->body(parser,BodyParser(record));

		/*if(typeGenerationFunction){
			record->setFlag(Record::GENERATED);
			record->_owner = typeGenerationFunction->body.scope;
			typeGenerationFunction->body.addChild(record);
			parser->leaveBlock();
			parser->introduceDefinition(typeGenerationFunction);
			return typeGenerationFunction;
		}
		else*/ return result;
	}
};

/// option    ::= <name> [ [Newlines] '{' type '}' ]
/// options   ::= option [Newlines] '|' options | option
/// variant   ::= variant <name> [Newlines] '{' [Newlines]  '|' options '}'
// TODO inner option type
struct VariantParser: IntrinsicPrefixMacro {
	VariantParser(): IntrinsicPrefixMacro("variant") {  }

	Node* parse(Parser* parser){
		auto name      = parser->expectName();

		auto variant = new Variant();
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		parser->ignoreNewlines();
		parser->expect("|");
		do {
			Variant::Field field = { parser->expectName(),-1 };
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
			parser->ignoreNewlines();
#endif
			if(parser->match("{")){
				parser->expect("}");
				parser->ignoreNewlines();
			}
			variant->add(field);
#ifndef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
			else parser->ignoreNewlines();
#endif
		} while(parser->match("|"));
		parser->expect("}");

		return new TypeDeclaration(variant,name);
	}
};

/// return    ::= 'return' [ expression ]
/// TODO what about { return }?!
struct ReturnParser: IntrinsicPrefixMacro {
	ReturnParser(): IntrinsicPrefixMacro("return") {}
	Node* parse(Parser* parser){
		if(isEndExpression(parser->peek())) return new ReturnExpression(new UnitExpression());
		else return new ReturnExpression(parser->parse());
	}
};

//TODO more advanced
Node* parsePattern(Parser* parser){
	//TODO substitute
	return parser->parse();
}

/// match     ::= 'match' '(' object ')' ... Bollocks! ...
struct MatchParser : IntrinsicPrefixMacro {
	MatchParser(): IntrinsicPrefixMacro("match") {}
	Node* parse(Parser* parser){

		parser->expect("(");
		auto matchResolver = new MatchResolver(parser->parse());
		parser->expect(")");
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		parser->ignoreNewlines();
		parser->expect("|");
		bool continueOn;
		do {
			continueOn = false;
			matchResolver->addChild(parsePattern(parser));//TODO deal with '|' operator
			if(!parser->match("=>")){
				parser->ignoreNewlines();
				parser->expect("|");
				matchResolver->addChild(new ControlFlowExpression(ControlFlowExpression::FALLTHROUGH));//fallthrough
				continueOn = true;
				continue;
			}
			parser->ignoreNewlines();

			auto body = new BlockExpression();
			body->_location = parser->previousLocation();
			parser->enterBlock(body);
			while(true){
				//TODO more testing and block integration
				body->addChild(parser->parse());
				parser->match(";");//optional ';' to mark the end of the statement
				if(parser->peek().isEOF()){
					parser->syntaxError(format("Unexpected EOF reached!"));//TODO 
					break;
				}
				parser->ignoreNewlines();
				if(parser->match("|")){ continueOn = true; break; }
				else if(parser->match("}")) break;
			}
			parser->leaveBlock();
			matchResolver->addChild(body);
		} while(continueOn);
		return matchResolver;
	}
};

/// catch     ::= 'catch'
// TODO
/// Last expression in block
/// Proposed syntax(match style)
/// {
///		var x = 1/0
/// catch
///		| e is DivisionByZero => ...
///		| _ => print("Error!")
/// }
struct CatchParser: IntrinsicPrefixMacro {
	CatchParser(): IntrinsicPrefixMacro("catch") {}
	Node* parse(Parser* parser){
		//TODO decide on syntax
		return new UnitExpression();
	}
};

/// throw    ::= 'throw' exception
//TODO
struct ThrowParser: IntrinsicPrefixMacro {
	ThrowParser(): IntrinsicPrefixMacro("throw") {}
	Node* parse(Parser* parser){
		auto expr = parser->parse();
		return new UnitExpression();
	}
};

/// sections  ::= <name> '.' sections | <name>
/// module    ::= sections
/// modules   ::= module ',' module | module
/// import    ::= 'import' ['export'] ['qualified'] modules
struct ImportParser: IntrinsicPrefixMacro {
	ImportParser(): IntrinsicPrefixMacro("import") {}
	Node* parse(Parser* parser){
		Location location;
		SymbolID moduleName;
		bool qualified = false,exported = false;
		if(parser->match("export")) exported = true;
		if(parser->match("qualified")) qualified = true;
		do {
			auto initial = parser->expectName();
			location = parser->previousLocation();
			std::string modulePath = initial.ptr();
			while(parser->match(".")){
				modulePath += '/';
				modulePath += parser->expectName().ptr();
			}
			if(auto moduleScope = compiler::findModule(modulePath.c_str())){
				debug("Importing %s.",modulePath);
				/**
				* This is a hack for intrinsic definitions so that there won't be symbol conflict
				* TODO might require checks so that we can only import it in certain modules
				*/
				if(modulePath == "arpha/intrinsic"){
					parser->currentScope()->importsArphaIntrinsic = true;
				}
				parser->currentScope()->import(moduleScope,modulePath.c_str(),qualified,exported);
			}else{
				error(location,"Module '%s' wasn't found!",modulePath);
			}
		}while(parser->match(","));
		return new UnitExpression;
	}
};

/// TODO remove
struct UseParser : IntrinsicPrefixMacro {
	UseParser(): IntrinsicPrefixMacro("use") {}
	Node* parse(Parser* parser){

		if(parser->match("argument")){
			auto argName = parser->expectName();
			parser->expect("as");
			auto typeExpr = parser->parse();
			parser->useTypedArgument(argName,typeExpr);
		}
		else if(parser->match("_")){
			//clear properties
			parser->clearProperties();
		} else {
			do {
				auto prop = parser->expectName();
				Node* value = nullptr;
				if(parser->match(":")) value = parser->parse(arpha::Precedence::Tuple);
				if(value) parser->useProperty(prop,value);
				else parser->useProperty(prop);
			} while(parser->match(","));
			parser->match("functions");
		}
		return new UnitExpression();
	}
};

/// capture    ::= '[>' expressions '<]'
/// TODO might need to move this to macro related module
struct CaptureParser : IntrinsicPrefixMacro {
	BlockParser* blockParser;

	struct QuasiParser : IntrinsicPrefixMacro {
		Scope* parentScope;
		QuasiParser(Scope* scope) : IntrinsicPrefixMacro("$"),parentScope(scope) {}
		Node* parse(Parser* parser){
			//Give access to macroes variables
			auto symbol = parser->expectName();
			auto expr = parser->compilationUnit()->resolver->multipassResolve(new UnresolvedSymbol(parser->previousLocation(),symbol,parentScope));
			if(auto v = expr->asVariableReference()){
				//TODO this safety check is required!
				//if(v->variable->functionOwner() != parentScope->functionOwner()) error(v,"Can't $ a variable that is outside the current function!");
			}else error(expr,"Expected a variable reference after '$'!");
			return expr;
			/*
			auto oldScope = parser->currentScope();
			parser->currentScope(parentScope);
			auto res = parser->parse(1000);
			if(auto v = res->asVariableReference()){
				//if(v->variable->functionOwner() != parentScope->functionOwner()) error(v,"Can't $ a variable that is outside the current function!");
			}else error(res,"Expected a variable reference after $!");
			parser->currentScope(oldScope);
			return res;*/
		}
	};

	CaptureParser(): IntrinsicPrefixMacro("[>") {
		blockParser = new BlockParser;
		blockParser->closingBrace = "<]"; 
	}

	Node* parse(Parser* parser){
		auto oldScope   = parser->currentScope(); 
		//parse block
		auto block = new BlockExpression();
		block->scope->parent = parser->_outerMacroOuterScope;
		QuasiParser  quasi(oldScope);
		parser->currentScope(block->scope);
		block->scope->define(&quasi);
		blockParser->body(parser,BlockParser::BlockChildParser(block));
		block->scope->remove(&quasi);
		parser->currentScope(oldScope);
		if(block->size() == 1 && block->scope->numberOfDefinitions() == 0) return new NodeReference(block->childrenPtr()[0]);
		return new NodeReference(block);
	}
};



namespace arpha {

void defineCoreSyntax(Scope* scope){

	blockParser = new BlockParser;
	scope->define(new ImportParser);
	scope->define(blockParser);
	scope->define(new ParenParser);
	scope->define(new CallParser);
	scope->define(new TupleParser);
	scope->define(new AccessParser);
	scope->define(new AssignmentParser);

	scope->define(new DefParser);
	scope->define(new MacroParser);
	scope->define(new ConstraintParser);
	scope->define(new ConceptParser);
	scope->define(new InterfaceParser);
	scope->define(new TypeParser);
	scope->define(new VariantParser);
	scope->define(new VarParser);
	scope->define(new MatchParser);
	scope->define(new CatchParser);
	scope->define(new ThrowParser);

	scope->define(new ReturnParser);

	scope->define(new UseParser);

	scope->define(new CaptureParser);
}

void defineIntrinsicSyntax(Scope* scope){

}

}
