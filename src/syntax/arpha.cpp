/**
* This module implements arpha's core syntax macroes
*/


#include "../base/symbol.h"
#include "../compiler.h"
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
			Unary = 90,
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
					if(auto f = (*i)->asFunction()){
						parser->introduceDefinition(f);
						f->body.scope->setParent(parser->currentScope());
					}
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

/// expressions ::= expression ',' expressions | expression
/// array       ::= '[' expressions ']'
struct ArrayParser: IntrinsicPrefixMacro {
	SymbolID closingParenthesis;
	ArrayParser(): IntrinsicPrefixMacro("[") {
		closingParenthesis = "]";
	}
	Node* parse(Parser* parser){
		if( parser->match(closingParenthesis) )
			return new UnitExpression;
		auto arr = new ArrayExpression();
		do arr->addChild(parser->parse(arpha::Precedence::Tuple));
		while(parser->match(","));
		parser->expect(closingParenthesis);
		return arr;
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
		if(!isMutable) var->setFlag(Variable::IS_IMMUTABLE);
		if(!isMutable && parser->compilationUnit()->moduleBody->scope->importsArphaIntrinsic) var->applyProperty("intrinsic",nullptr);
		parser->introduceDefinition(var);
		vars.push_back(var);
		while(parser->match(",")){
			auto name = parser->expectName();
			var = new Variable(name,parser->previousLocation());
			if(!isMutable) var->setFlag(Variable::IS_IMMUTABLE);
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
			assign->_location = parser->previousLocation();
			return assign;
		}
		else return result;
	}
	Node* parse(Parser* parser){
		return parseVar(parser,parser->expectName(),true);
	}
};

void parseProperties(Parser* parser,Node* applicant){
	Parser::NewlineIgnorer i(true,parser);
	if(!parser->match("uses")){
		i.rollback();
		return;
	}
	do {
		auto prop = parser->expectName();
		Node* value = nullptr;
		if(parser->match(":")) value = parser->parse(arpha::Precedence::Tuple);
		applicant->applyProperty(prop,value);
	} while(parser->match(","));
}

struct ParameterTypeSuggestion {
	SymbolID name;
	Node* expression;
};

/// param     ::= <name> [Type] [ '=' defaultValue ]
/// paramList ::= param ',' paramList | param
/// params    ::= '(' paramList ')' | '(' ')'
void parseFunctionParameters(Parser* parser,Function* func,Type* defaultType = nullptr,ParameterTypeSuggestion* suggestions = nullptr,size_t numberOfSuggestions = 0){
	bool vararg = false;
	if(!parser->match(")")){
		while(1){
			auto location = parser->currentLocation();
			auto argName  = parser->expectName();
			auto param = new Argument(argName,location,func);
		
			auto next = parser->peek();
			if(next.isSymbol() && next.symbol == ".."){
				parser->consume();
				next = parser->peek();
				vararg = true;
				param->setFlag(Argument::IS_VARARG);
			}
			if(next.isSymbol() && ( next.symbol == "," || next.symbol == ")" || next.symbol == "=")){
				if(defaultType) param->type.specify(defaultType);
				else if(suggestions){
					for(ParameterTypeSuggestion* i = suggestions,*end = suggestions+numberOfSuggestions;i!=end;++i){
						if((*i).name == argName){
							param->type.kind = TypePatternUnresolvedExpression::UNRESOLVED;
							param->type.unresolvedExpression = (*i).expression;
						}
					}
				}
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

			if(vararg){
				parser->expect(")");
				break;
			}
			if(parser->match(")")) break;
			parser->expect(",");
		}
	}
}

/// functionBody   ::= nothing | '=' expression | '{' block '}'
//TODO: maybe make this more eficcient by moving up the '=' test
bool parseFunctionBody(Parser* parser,Function* func,bool allowNoBody = false){
	//NB: need to take \n '{' into account!
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
	Parser::NewlineIgnorer i(true,parser);
	if(parser->match("{")){
		blockParser->body(parser,BlockParser::BlockChildParser(&func->body));
		return true;
	}
	else i.rollback();
#endif
	if(isEndExpression(parser->peek())){
		if(!allowNoBody) error(parser->previousLocation(),"The function %s needs to have a body( You can use either '=' expression or '{' body '}')!",func->label());
		return false;
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
	return true;
}

inline bool isIntrinsicImported(Parser* parser){
	return parser->compilationUnit()->moduleBody->scope->importsArphaIntrinsic;
}

// TODO contract requirements?
static Function* parseFunction(SymbolID name,Parser* parser){
	//Function
	auto func = new Function(name,parser->previousLocation());
	//
	if(isIntrinsicImported(parser)) func->applyProperty("intrinsic",nullptr);
	else if(parser->compilationUnit()->moduleBody->scope->importsArphaExternal){
		func->applyProperty("external",nullptr);
		func->cc = data::ast::Function::CCALL;
	}
	parser->introduceDefinition(func);

	parser->enterBlock(&func->body);
	//parse arguments
	parseFunctionParameters(parser,func);
	//return type & body
	auto token = parser->peek();
	if(!isEndExpression(token) && !(token.isSymbol() && (token.symbol == "=" || token.symbol == "{" || token.symbol == "uses"))){
		func->_returnType.parse(parser,arpha::Precedence::Assignment);
	}
	parseProperties(parser,func);
	parseFunctionBody(parser,func,func->isIntrinsic() || func->isExternal());
	parser->leaveBlock();

	return func;
}

// TODO
static void parseTypeInvariant(Parser* parser,TypeDeclaration* typeDecl){
	parser->syntaxError(format("Invariants aren't supported yet"));
}

/// constant  ::= 'def' names  '=' [Newlines] initializers
/// function  ::= 'def' <name> params [returnType] functionBody
struct DefParser: IntrinsicPrefixMacro {
	DefParser(): IntrinsicPrefixMacro("def") {  }

	Node* parse(Parser* parser){
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
		PrefixMacro* pmacro;InfixMacro* imacro;Node* macro;

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
		if(!infix){
			if(parser->match("(")){
				functionLike = true;//Function on the outside, but macro lays within!
				parseFunctionParameters(parser,func);
				for(auto i = func->arguments.begin();i!=func->arguments.end();i++){
					(*i)->hideType(intrinsics::types::NodePointer);
				}
				macro = func;
			}
			else {
				pmacro = new PrefixMacro(func);
				macro = pmacro;
			}
		}
		else {
			imacro = new InfixMacro(func,new IntegerLiteral((uint64)0));
			macro = imacro;
		}
		
		parseProperties(parser,macro);
		//import defaults.
		func->body.scope->import(compiler::findModule("arpha/ast"),"ast",true,false);
		if(!functionLike) func->body.scope->import(compiler::findModule("arpha/syntax/parser"),"parser",true,false);
		parseFunctionBody(parser,func,false);
		
		parser->leaveBlock();
		if(setOuterScope) parser->_outerMacroOuterScope = nullptr;
		//Create the actual macro
		if(!infix){
			if(!functionLike){
				parser->introduceDefinition(pmacro);
				return parser->compilationUnit()->resolver->resolveMacroAtParseStage(macro);
			} 
			else {
				parser->introduceDefinition(func);
				return func;
			}
		} else {
			parser->introduceDefinition(imacro);
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
	return func;
}



void parseTraitTemplateDeclaration(Parser* parser,Scope* scope){
	struct Substitute: IntrinsicPrefixMacro {
	public:
		Substitute(SymbolID name,size_t index) : IntrinsicPrefixMacro(name) { this->index = index; }
		Node* parse(Parser* parser){ return new TraitParameterReference(index); }
		Node* createReference()    { return new TraitParameterReference(index); }
		size_t index;
	};

	size_t i = 0;
	while(1){
		auto argName  = parser->expectName();

		scope->define(new Substitute(argName,i));
		i++;

		if(parser->match(")")) break;
		parser->expect(",");
	}
}

static Function* parseTraitMethod(Parser* parser,ParameterTypeSuggestion* suggestions,size_t numberOfSuggestions){
	auto name = parser->expectName();

	auto func = new Function(name,parser->previousLocation());

	parser->enterBlock(&func->body);
	//parse arguments
	parser->expect("(");
	parseFunctionParameters(parser,func,nullptr,suggestions,numberOfSuggestions);
	//return type & body
	auto token = parser->peek();
	if(!isEndExpression(token) && !(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
		func->_returnType.parse(parser,arpha::Precedence::Assignment);
	}
	//special handling for body
	auto hasBody = parseFunctionBody(parser,func,true);
	parser->leaveBlock();

	if(hasBody){
		parser->mixinedExpressions.push_back(func);
		return nullptr;
	}
	return func;
}

/// requirement    ::= 'def' <name> params [returnType]
/// implementation ::= 'def' <name> params [returnType] functionBody
/// declaration    ::= requirement | implementation | invariant
/// declarations   ::= declaration (';'|Newlines) declarations | declaration
/// trait          ::= 'trait' <name> ( '{' declarations '}' ) | ('{' '}')
/// TODO trait inheritance
struct ConceptParser: IntrinsicPrefixMacro {
	ConceptParser(): IntrinsicPrefixMacro("concept") {}

	struct BodyParser {
		Trait* trait;
		BodyParser(Trait* _trait) : trait(_trait) {}
		bool operator()(Parser* parser){
			auto  cmd = parser->expectName();
			if(cmd == "def"){
				ParameterTypeSuggestion self = { "self",new TypeReference(trait) };
				if(auto func = parseTraitMethod(parser,&self,1)) trait->methods.push_back(func);
			}
			else{
				parser->syntaxError(format("Can't parse a command '%s' inside trait declaration",cmd));
				return false;
			}
			return true;
		}
	};

	Node* parse(Parser* parser){
		bool implicit = false;
		if(parser->match("(")){
			parser->expect("implicit");
			implicit = true;
			parser->expect(")");
		}
		auto name      = parser->expectName();

		Scope* templateDeclaration = nullptr;
		if(parser->match("(")){
			templateDeclaration = new Scope(parser->currentScope());
			parseTraitTemplateDeclaration(parser,templateDeclaration);
			parser->currentScope(templateDeclaration);
		}
		
		auto trait = new Trait(templateDeclaration);
		if(implicit) trait->makeImplicit();


#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		Parser::NewlineIgnorer i(true,parser);
		parser->ignoreNewlines();
#endif
		if(parser->match("{")){
			blockParser->body(parser,BodyParser(trait));
		} else i.rollback();

		if(templateDeclaration){
			parser->currentScope(templateDeclaration->parent);
		}
		auto decl = new TypeDeclaration(trait,name);
		parser->introduceDefinition(decl);
		if(!trait->isImplicit() && isIntrinsicImported(parser)){
			Trait::mapIntrinsicConcept(trait);
		}


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

static Function* parseTypeMethod(Parser* parser,ParameterTypeSuggestion* suggestions,size_t numberOfSuggestions){
	auto name = parser->expectName();
	auto func = new Function(name,parser->previousLocation());

	parser->enterBlock(&func->body);
	//parse arguments
	parser->expect("(");
	parseFunctionParameters(parser,func,nullptr,suggestions,numberOfSuggestions);
	//return type & body
	auto token = parser->peek();
	if(!isEndExpression(token) && !(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
		func->_returnType.parse(parser,arpha::Precedence::Assignment);
	}
	//special handling for body
	parseFunctionBody(parser,func);
	parser->leaveBlock();
	return func;
}

/// fields       ::= ['private'] 'var' names (type | type '=' [Newlines] initializers)
/// method       ::= 'def' <name> params [returnType] functionBody
/// declaration  ::= fields | method
/// declarations ::= declaration (';'|Newlines) declarations | declaration
/// type         ::= 'type' <name> '{' declarations '}'

/// TODO stuff
struct TypeParser: IntrinsicPrefixMacro {
	TypeParser(): IntrinsicPrefixMacro("type") {  }

	/// fields ::= ['private'] ['extends'] 'var' names [ type ] '=' [Newlines] initializers
	enum { FIELDS_EXT = 0x1,FIELDS_READONLY = 0x2,FIELDS_PRIVATE = 0x4,TEMPLATED_TYPE = 0x8 };
	static void fields(Parser* parser,Record* record,std::vector<Node*>& mixinedExpressions,int flags = 0){
		size_t i = record->fields.size();
		const bool isExtending = (flags & FIELDS_EXT)!=0;
		const bool isPrivate   = (flags & FIELDS_PRIVATE)!=0;
		const bool isReadonly  = (flags & FIELDS_READONLY)!=0;

		do {
			auto field = Record::Field(parser->expectName(),intrinsics::types::Void);
			field.isExtending = isExtending;
			field.isPrivate   = isPrivate;
			field.isReadonly  = isReadonly;
			record->add(field);
			if((flags & TEMPLATED_TYPE) == 0){
				auto funcs = record->createFieldGetterSetter(parser->previousLocation(),record->fields.size()-1);
				mixinedExpressions.push_back(funcs.first);
				mixinedExpressions.push_back(funcs.second);
			}
		} while(parser->match(","));
		
		TypePatternUnresolvedExpression type;
		type.parse(parser,arpha::Precedence::Assignment);
		Node** initializers;
		size_t numberOfInitializers = 0;
		Node* initializerExpr;
		if(parser->match("=")){
			parser->ignoreNewlines();//NB: for consistency with other usages of '='
			initializerExpr = parser->parse();
			auto fieldsDeclared = record->fields.size() - i;

			if(auto tuple = initializerExpr->asTupleExpression()){
				if(fieldsDeclared == 1){
					initializers=&initializerExpr;
					numberOfInitializers = 1;
				} else {
					initializers = tuple->childrenPtr();
					numberOfInitializers = tuple->size();
				}
			} else {
				initializers=&initializerExpr;
				numberOfInitializers = 1;
			}
			
			if(numberOfInitializers != fieldsDeclared){
				parser->syntaxError(format("Initializing assignment tuple size mismatch"));
				numberOfInitializers = 0;
			}
		}
		
		for(auto j =i;j<record->fields.size();j++){
			record->fields[j].type = type;
			record->fields[j].initializer = numberOfInitializers==0? nullptr: initializers[j-i];
		}

	}

	// body ::= '{' fields ';' fields ... '}'
	struct BodyParser {
		Record* record;
		Function* templateDeclaration;
		std::vector<Node*>& mixinedExpressions;

		BodyParser(Record* _record,Function* templateDecl,std::vector<Node*>& mixins) : record(_record),templateDeclaration(templateDecl),mixinedExpressions(mixins) {}
		bool operator()(Parser* parser){
			auto  cmd = parser->expectName();
			int flags = templateDeclaration != nullptr?TEMPLATED_TYPE:0;
			if(cmd == "private"){
				flags |= FIELDS_PRIVATE;
				cmd = parser->expectName();
			}
			if(cmd == "extends"){
				flags |= FIELDS_EXT;
				cmd = parser->expectName();
			}
			if(cmd == "var"){
				fields(parser,record,mixinedExpressions,flags);
			}
			else if(cmd == "def"){
				if(flags & FIELDS_EXT) parser->syntaxError(format("Can't use property 'extends' on a function declaration!"));

				ParameterTypeSuggestion self = { "self",nullptr };
				if(!templateDeclaration){
					self.expression = new TypeReference(new Type(Type::POINTER,record));
				}
				//TODO
				else self.expression = new PointerOperation(new UnresolvedSymbol(parser->previousLocation(),templateDeclaration->label()),PointerOperation::DEREFERENCE_OR_TYPE);
				
				auto func = parseTypeMethod(parser,&self,1);
				if(flags & FIELDS_PRIVATE) func->visibilityMode(data::ast::PRIVATE);
				mixinedExpressions.push_back(func);
				return true;
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

		Function* templateDeclaration = nullptr;
		if(parser->match("(")){
			templateDeclaration = parseTypeTemplateDeclaration(name,parser);
		}

		std::vector<TypePatternUnresolvedExpression> extensions;
		if(parser->match("extends")){
			TypePatternUnresolvedExpression expr;
			expr.parse(parser,1);
			extensions.push_back(expr);
		}
		
		/*if(parser->match("=")){
			parser->syntaxError(format("NOT ALLOWED >_<!"));
		}*/
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		//record
		auto record = new Record();
		std::vector<Node*> mixinedExpressions;
		blockParser->body(parser,BodyParser(record,templateDeclaration,mixinedExpressions));

		auto decl = new TypeDeclaration(record,name,templateDeclaration != nullptr);
		if(record->fields.size() == 0){
			if(templateDeclaration && isIntrinsicImported(parser)){
				templateDeclaration->makeIntrinsic();
				Function::getIntrinsicTypeTemplateBinder(templateDeclaration);
			}
			else error(decl,"The type '%s' must have at least one field!",name);
		}
		parser->introduceDefinition(decl);
		if(extensions.size()){
			decl->extendedConcepts = extensions;
		}

		if(templateDeclaration){
			templateDeclaration->makeTypeTemplate(decl);
			parser->leaveBlock();
			parser->introduceDefinition(templateDeclaration);

			parser->mixinedExpressions = mixinedExpressions;
			return templateDeclaration;
		}
		parser->mixinedExpressions = mixinedExpressions;

		return decl;
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
		int  optionID  = 0;
		auto variant     = new Variant();
		auto declaration = new TypeDeclaration(variant,name);
		bool hasStructs = false;

#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
		parser->ignoreNewlines();
#endif
		parser->expect("{");
		parser->ignoreNewlines();
		parser->expect("|");
		do {
			auto option = parser->expectName();
			
#ifdef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
			parser->ignoreNewlines();
#endif
			DeclaredType* type;
			if(parser->match("{")){
				auto record = new Record(variant,optionID);
				std::vector<Node*> mixinedExpressions;
				blockParser->body(parser,TypeParser::BodyParser(record,nullptr,mixinedExpressions));
				parser->ignoreNewlines();
				type = record;
				hasStructs = true;
			}
			else type = new VariantOption(variant,optionID);
			optionID++;

			auto decl = new TypeDeclaration(type,option);
			if(declaration->optionalStaticBlock){
				parser->introduceDefinition(decl);
				declaration->optionalStaticBlock->addChild(decl);
			}
			else {
				declaration->optionalStaticBlock = new BlockExpression();
				parser->enterBlock(declaration->optionalStaticBlock);
				parser->introduceDefinition(decl);
				declaration->optionalStaticBlock->addChild(decl);
			}

#ifndef SYNTAX_ALLOW_NEWLINES_BEFORE_BRACE
			parser->ignoreNewlines();
#endif
		} while(parser->match("|"));
		parser->expect("}");
		if(declaration->optionalStaticBlock) parser->leaveBlock();
		
		variant->numberOfOptions = optionID;
		if(!hasStructs) variant->setFlag(Variant::NO_INNER_STRUCTURES);

		parser->introduceDefinition(declaration);
		return declaration;
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

/// ::= '*' expression
struct StarParser: IntrinsicPrefixMacro {
	StarParser(): IntrinsicPrefixMacro("*") {}
	Node* parse(Parser* parser){
		return new PointerOperation(parser->parse(arpha::Precedence::Unary),PointerOperation::DEREFERENCE_OR_TYPE);
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
		return new ThrowExpression(parser->parse());
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
				else if(modulePath == "arpha/external"){
					parser->currentScope()->importsArphaExternal = true;
				}
				parser->currentScope()->import(moduleScope,modulePath.c_str(),qualified,exported);
			}else{
				error(location,"Module '%s' wasn't found!",modulePath);
			}
		}while(parser->match(","));
		return new UnitExpression;
	}
};

struct WhereParser : IntrinsicPrefixMacro {
	WhereParser(): IntrinsicPrefixMacro("where") {}
	Node* parse(Parser* parser){
		auto cmd = new ScopedCommand();
		do{
			auto label = parser->expectName();
			parser->expect("is");
			auto expr = parser->parse(arpha::Precedence::Tuple);
			cmd->parameters.push_back(std::make_pair(label,expr));
		} while(parser->match(","));
		parser->expect(":");
		return cmd;
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
			auto expr = parser->compilationUnit()->resolver->multipassResolve(new UnresolvedSymbol(parser->previousLocation(),symbol,parentScope),true);
			if(auto v = expr->asVariableReference()){
				//TODO this safety check is required!
				//if(v->variable->functionOwner() != parentScope->functionOwner()) error(v,"Can't $ a variable that is outside the current function!");
			}else error(expr,"Expected a variable reference after '$'!");
			return expr;
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
	scope->define(new ArrayParser);
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

	scope->define(new StarParser);


	scope->define(new ReturnParser);

	scope->define(new WhereParser);

	scope->define(new CaptureParser);
}

}
