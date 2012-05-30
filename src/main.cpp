#include "base/base.h"
#include "base/symbol.h"
#include "base/system.h"
#include "base/bigint.h"
#include "compiler.h"
#include "ast/scope.h"
#include "ast/node.h"
#include "ast/declarations.h"
#include "ast/evaluate.h"
#include "ast/interpret.h"
#include "syntax/parser.h"
#include "intrinsics/ast.h"
#include "intrinsics/types.h"
#include "intrinsics/compiler.h"

namespace arpha {
	Scope *scope;

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
};


// parses blocks and whatnot
// body ::= {';'|newline}* expression {';'|newline}+ expressions...
// ::= {body|'{' body '}'}
struct BlockParser: PrefixDefinition {
	SymbolID lineAlternative; //AKA the ';' symbol
	SymbolID closingBrace;    //'}'

	BlockParser(): PrefixDefinition("{",Location()) {
		lineAlternative = ";";
		closingBrace = "}";
	}

	struct BlockChildParser {
		BlockExpression* _block;
		BlockChildParser(BlockExpression* block) : _block(block) {}
		bool operator ()(Parser* parser){
			auto e = parser->evaluator()->mixinedExpression;
			auto p = parser->parse();
			if(parser->evaluator()->mixinedExpression != e){
				_block->children.push_back(parser->evaluator()->mixinedExpression);
				parser->evaluator()->mixinedExpression = e;
			}
			_block->children.push_back(p);
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
				if(acceptEOF && token.isEOF()) break;
				error(parser->previousLocation(),"Unexpected %s - A newline or a '%s' is expected!",token,lineAlternative);
			}
		}
	}

	//On '{'
	Node* parse(Parser* parser){
		auto oldScope = parser->currentScope();
		BlockExpression* block = new BlockExpression(new Scope(oldScope));
		parser->currentScope(block->scope);
		body(parser,BlockChildParser(block));
		parser->currentScope(oldScope);
		return block;
	}
};

BlockParser* blockParser;

// parses an arpha module
// ::= {EOF|block.body EOF}
BlockExpression* parseModule(Parser* parser,Scope* scope){
	parser->currentScope(scope);
	BlockExpression* block = new BlockExpression(scope);
	blockParser->body(parser,BlockParser::BlockChildParser(block),false,true); //Ignore '}' and end on EOF
	return block;
}

//TODO NB x:(1,2) proper parsing using parser extensions

/// ::= '(' expression ')'
struct ParenParser: PrefixDefinition {
	SymbolID closingParenthesis;
	ParenParser(): PrefixDefinition("(",Location()) {
		closingParenthesis = ")";
	}
	Node* parse(Parser* parser){
		if( parser->match(closingParenthesis) )
			return new UnitExpression;
		auto l = parser->labelForNextNode;
		parser->labelForNextNode = SymbolID();
		auto e = parser->parse();
		parser->expect(closingParenthesis);
		parser->labelForNextNode = l;
		return e;
	}
};

/// ::= expression '(' expression ')'
struct CallParser: InfixDefinition {
	SymbolID closingParenthesis;
	CallParser(): InfixDefinition("(",arpha::Precedence::Call,Location()) {
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
struct TupleParser: InfixDefinition {
	TupleParser(): InfixDefinition(",",arpha::Precedence::Tuple,Location()) {}
	Node* parse(Parser* parser,Node* node){
		auto tuple = new TupleExpression;
		tuple->children.push_back(node);
		do tuple->children.push_back(parser->parse(arpha::Precedence::Tuple));
		while(parser->match(","));
		return tuple;
	}
};

/// ::= expression '.' expression
struct AccessParser: InfixDefinition {
	AccessParser(): InfixDefinition(".",arpha::Precedence::Access,Location()) {}
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
					error(node->location,"Symbol '%s' isn't defined in module '%s'!",parser->lookedUpToken.symbol,val->scope->id);
					return ErrorExpression::getInstance();
				}
				auto expression = parser->evaluate(def->parse(parser));
				//apply the correct overload lookup scope
				if(auto overloadSet = expression->asUnresolvedSymbol()) overloadSet->explicitLookupScope = val->scope->scope;
				return expression;
			}else{
				error(node->location,"A module '%s' isn't imported from package '%s'!",parser->lookedUpToken.symbol,val->scope->id);
				return ErrorExpression::getInstance();
			}
		}
		return new AccessExpression(node,parser->lookedUpToken.symbol);
	}
};

/// ::= expression '=' expression
struct AssignmentParser: InfixDefinition {
	AssignmentParser(): InfixDefinition("=",arpha::Precedence::Assignment,Location()) {}
	Node* parse(Parser* parser,Node* node){
		return new AssignmentExpression(node,parser->parse(arpha::Precedence::Assignment-1)); //left associative
	}
};

bool isEndExpression(const Token& token){
	return token.isEOF() || token.isLine() || (token.isSymbol() && token.symbol == blockParser->lineAlternative );
}
bool isEndExpressionEquals(const Token& token){
	return isEndExpression(token) || (token.isSymbol() && token.symbol == "=");
}



/// ::= 'var' <names> [type|unresolvedExpression (hopefully) resolving to type|Nothing]
struct VarParser: PrefixDefinition {
	VarParser(): PrefixDefinition("var",Location()) {}
	static Node* parseVar(Parser* parser,SymbolID first,bool isMutable){
		std::vector<Variable*> vars;
		bool isLocal = parser->currentScope()->functionOwner() != nullptr;
		if(!first.isNull()){
			auto var = new Variable(first,parser->previousLocation(),isLocal);
			var->isMutable = isMutable;
			parser->currentScope()->define(var);
			vars.push_back(var);
			while(parser->match(",")){
				var = new Variable(parser->expectName(),parser->previousLocation(),isLocal);
				var->isMutable = isMutable;
				parser->currentScope()->define(var);
				vars.push_back(var);
			}
		}else{
			do {
				auto var = new Variable(parser->expectName(),parser->previousLocation(),isLocal);
				var->isMutable = isMutable;
				parser->currentScope()->define(var);
				vars.push_back(var);
			}
			while(parser->match(","));
		}
		//parse optional type
		InferredUnresolvedTypeExpression type;
		if(!isEndExpressionEquals(parser->peek())) type.parse(parser,arpha::Precedence::Assignment);
		for(auto i=vars.begin();i!=vars.end();i++) (*i)->type = type;
		
		Node* result;
		if(vars.size() == 1) result = new VariableReference(vars[0]);
		else{
			auto tuple = new TupleExpression;
			for(auto i=vars.begin();i!=vars.end();i++) tuple->children.push_back(new VariableReference((*i)));
			result = tuple;
		}

		if(parser->match("=")){
			//Initial assignment
			result = parser->evaluate(result);
			auto assign = new AssignmentExpression(result,parser->parse(arpha::Precedence::Assignment-1)); 
			assign->isInitializingAssignment = true;
			return assign;
		}
		else return result;
	}
	Node* parse(Parser* parser){
		return parseVar(parser,SymbolID(),true);
	}
};

int expectInteger(Parser* parser,int stickiness){
	auto node = parser->parse(stickiness);
	if(auto c= node->asIntegerLiteral()){
		return int(c->integer.u64); //TODO this is potentially unsafe
	}
	error(node->location,"Expected an integer constant instead of %s!",node);
	return -1;
}




Node* PrefixMacro::parse(Parser* parser){
	return syntax->execute(parser);
}
Node* InfixMacro::parse(Parser* parser,Node* node){
	return syntax->execute(parser,node);
}


//TODO macro with a functional form mixin like syntax macroes for efficiency
MacroSyntax::Instruction::Instruction(Function* func,Node* node,int sticky) : stickiness(sticky) {
	if(!node){
		kind = OPTIONAL;
		return;
	}
	if(auto var = node->asVariableReference()){
		argId  = func->findArgument(var->variable);
		if(argId == -1) error(node->location,"%s is not a valid syntax rule!\n Only macro's parameters are accepted!",node);
		kind = EXPR;//TODO multiple argument error
	}
	else if(auto str = node->asStringLiteral()){
		symbol = SymbolID(str->block.ptr(),str->block.length());//TODO "" check
		kind = SYMBOL;
	}
	else error(node->location,"%s is not a valid syntax rule!",node);
}
MacroSyntax::MacroSyntax(Function* func) : function(func),intrinsicEvaluator(nullptr) {
	numArgs = func->arguments.size();
}
void MacroSyntax::compile(Scope* scope){
	//generate definitions
	if(instructions[0].kind == Instruction::SYMBOL){
		debug("Created new macro %s",instructions[0].symbol);
		scope->define(new PrefixMacro(instructions[0].symbol,function->location,this));
		instructions.erase(instructions.begin());
	}
	else if(instructions[0].kind == Instruction::EXPR && instructions.size()>1 && instructions[1].kind == Instruction::SYMBOL){
		debug("Created new infix macro %s,%s",instructions[1].symbol,instructions[1].stickiness);
		scope->define(new InfixMacro(instructions[1].symbol,function->location,instructions[1].stickiness,this));
		instructions.erase(instructions.begin());
		instructions.erase(instructions.begin());
	}
	else{
		error(function->location,"Can't compile macro's syntax!");
	}
}
Node* MacroSyntax::execute(Parser* parser,Node* node){
	debug("parsing macro %s",function->id);
	auto loc = node ? node->location : parser->previousLocation();
	Node* arg = node;
	TupleExpression* tupleArg = nullptr;
	if(numArgs == 0) arg = new UnitExpression();
	else if(numArgs > 1){
		tupleArg = new TupleExpression();
		tupleArg->children.resize(numArgs);
		if(node) tupleArg->children[0] = node;
		arg = tupleArg;
	}
	for(auto i = instructions.begin();i!=instructions.end();i++){
		debug("Executing instruction %s , stickiness:%s, special:%d",(*i).kind,(*i).stickiness,(*i).innerRangeSize);
		if((*i).kind == Instruction::SYMBOL) parser->expect((*i).symbol);
		else if((*i).kind == Instruction::EXPR){
			if(tupleArg)
				tupleArg->children[(*i).argId] = parser->parse((*i).stickiness);
			else arg = parser->parse((*i).stickiness);
		}
		else if((*i).kind == Instruction::OPTIONAL){
			int rangeSize = (*i).innerRangeSize;
			bool skip = false;
			i++;
			if((*i).kind == Instruction::SYMBOL){
				if(!parser->match((*i).symbol) ) skip = true; 				//optional("foo")
			}
			else if((*i).kind == Instruction::EXPR){
				auto next =parser->peek();
				if(next.isSymbol() && next.symbol == (*(i+rangeSize)).symbol ) skip = true; //optional(expr),"foo"
				else i--;
			}
			if(skip){
				//skip optional block, replacing expression with ()	or argument's defualt value	
				auto skipTo = i + rangeSize;
				for(;i!=skipTo;i++){
					if((*i).kind == Instruction::EXPR){
						if(tupleArg)
							tupleArg->children[(*i).argId] = function->arguments[(*i).argId]->defaultValue() ? function->arguments[(*i).argId]->defaultValue()->duplicate() : new UnitExpression();
						else arg = new UnitExpression();
					}
				}
				i--;
			}
		}
	}
	if(intrinsicEvaluator){	
		return intrinsicEvaluator(parser,tupleArg?tupleArg->children.begin()._Ptr:&arg,0);
	}
	else return parser->evaluator()->mixinFunction(loc,function,arg);
}

int MacroSyntax::parse(Parser* parser){
	int result = 0;
	parser->expect("(");
	do {
		if(parser->match("precedence")){
			parser->expect("(");
			auto stickiness = expectInteger(parser,arpha::Precedence::Tuple);		
			parser->expect(")");
			auto instr = parser->parse(arpha::Precedence::Tuple);
			instructions.push_back(Instruction(function,instr,stickiness));
		}
		else if(parser->match("optional")){
			auto i = instructions.size();
			instructions.push_back(Instruction(function));
			auto n = parse(parser);
			instructions[i].innerRangeSize = n;
			result+=n;
		}
		else instructions.push_back(Instruction(function,parser->parse(arpha::Precedence::Tuple)));
		result++;

		if(parser->match(")")) break;
		parser->expect(",");
	}while(true);
	return result;
}

static void parseMacroSyntax(Function* func,Parser* parser){
	auto m = new MacroSyntax(func);
	m->parse(parser);
	m->compile(parser->currentScope()->parent);
}

/// ::= 'def' <name> '=' expression
/// ::= 'def' <name> '(' args ')' [returnType] body
struct DefParser: PrefixDefinition {
	DefParser(): PrefixDefinition("def",Location()) {  }

	/// body ::= [nothing|'=' expression|'{' block '}']
	static void functionBody(Function* func,Parser* parser,bool allowNoBody = true){
		auto token = parser->peek();
		if(token.isLine() || token.isEOF() || (token.isSymbol() && token.symbol == blockParser->lineAlternative)){
			if(!allowNoBody) error(parser->currentLocation(),"The function %s needs to have a body!",func->id);
		}else{
			if(parser->match("="))
				func->body.children.push_back(parser->evaluate(new ReturnExpression(parser->parse())));
			else {
				parser->expect("{");
				blockParser->body(parser,BlockParser::BlockChildParser(&func->body));
			}
		}
	}


	static Node* function(SymbolID name,Location location,Parser* parser,bool macro = false){
		//Function
		auto bodyScope = new Scope(parser->currentScope());
		auto func = new Function(name,location,bodyScope);
		if(macro) func->_mixinOnCall = true;
		bodyScope->_functionOwner = func;

		auto oldScope = parser->currentScope();
		parser->currentScope(bodyScope);
		//parse arguments
		if(!parser->match(")")){
			while(1){
				auto loc = parser->currentLocation();
				auto argName = parser->expectName();
				auto param = new Argument(argName,location);
		
				auto next = parser->peek();
				bool inferOnDefault = false;
				if(next.isSymbol() && ( next.symbol == "," || next.symbol == ")" || next.symbol == "=")){
					param->type.kind = InferredUnresolvedTypeExpression::Wildcard;
					inferOnDefault = true;
				}else{
					param->type.parse(parser,arpha::Precedence::Tuple);
					next = parser->peek();
				}

				//parameter's default value
				if(next.isSymbol() && next.symbol == "="){
					parser->consume();
					param->defaultValue(parser->parse(arpha::Precedence::Tuple),inferOnDefault);
				}

				func->arguments.push_back(param);
				bodyScope->define(param);

				if(parser->match(")")) break;
				parser->expect(",");
			}
		}

		if(macro && parser->match("syntax")){
			parseMacroSyntax(func,parser);
		}else
			oldScope->defineFunction(func);
		//return type & body
		auto token = parser->peek();
		if(token.isLine() || token.isEOF() || (token.isSymbol() && token.symbol == blockParser->lineAlternative)){
			func->_returnType.infer(intrinsics::types::Void);
		}
		else {
			if(!(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
				func->_returnType.parse(parser,arpha::Precedence::Assignment);
			}
			functionBody(func,parser);
		}
		parser->currentScope(oldScope);

		func->resolve(parser->evaluator());
		return new UnitExpression();//new FunctionReference(func);//TODO return reference when func has no generic args
	}

	Node* parse(Parser* parser){
		auto location  = parser->previousLocation();
		auto name = parser->expectName();
		if(parser->match("(")) return function(name,location,parser);
		else return VarParser::parseVar(parser,name,false);
	}
};

struct MacroParser: PrefixDefinition {
	MacroParser(): PrefixDefinition("macro",Location()) {  }
	Node* parse(Parser* parser){
				auto location  = parser->previousLocation();
		auto name = parser->expectName();
		parser->expect("(");
		return DefParser::function(name,location,parser,true);


	}
};

struct PrefixMacro2: PrefixDefinition {
	Function* function;

	PrefixMacro2(Function* f) : PrefixDefinition(f->id,f->location),function(f) {}
	Node* parse(Parser* parser){
		if(!function->isResolved()){
			error(parser->currentLocation(),"Can't parse macro %s - the macro isn't resolved at usage time!",id);
			return ErrorExpression::getInstance();
		}
		InterpreterInvocation i;
		auto res = i.interpret(parser->evaluator()->interpreter(),function,nullptr);
		if(res){
			DuplicationModifiers mods;
			mods.isMacroMixin = true;
			auto v = res->asValueExpression();
			return parser->evaluator()->mixin(&mods,reinterpret_cast<Node*>(v->data));
		}else {
			error(parser->previousLocation(),"Failed to interpret a macro %s at compile time!",id);
			return ErrorExpression::getInstance();
		}
	}

	bool isResolved(){ return function->isResolved(); }
	bool resolve(Evaluator* evaluator){ return function->resolve(evaluator); }
};

struct InfixMacro2: InfixDefinition{
	Function* function;

	InfixMacro2(Function*f,int stickiness) : InfixDefinition(f->id,stickiness,Location()),function(f) {}
	Node* parse(Parser* parser,Node* node){
		if(!function->isResolved()){
			error(parser->currentLocation(),"Can't parse macro %s - the macro isn't resolved at usage time!",id);
			return ErrorExpression::getInstance();
		}
		auto arg = new ValueExpression(node,intrinsics::ast::ExprPtr);
		InterpreterInvocation i;
		auto res = i.interpret(parser->evaluator()->interpreter(),function,arg);
		if(res){
			DuplicationModifiers mods;
			mods.isMacroMixin = true;
			auto v = res->asValueExpression();
			return parser->evaluator()->mixin(&mods,reinterpret_cast<Node*>(v->data));
		}else {
			error(parser->previousLocation(),"Failed to interpret an infix macro %s at compile time!",id);
			return ErrorExpression::getInstance();
		}
		return nullptr;
	}
};

//Todo function macroes
struct Macro2Parser: PrefixDefinition {
	Macro2Parser(): PrefixDefinition("macro",Location()) {  }
	Node* parse(Parser* parser){
		auto location  = parser->previousLocation();
		SymbolID name;
		Argument* infix = nullptr;
		int precedence;
		if(parser->match("(")){
			infix = new Argument(parser->expectName(),parser->currentLocation());
			parser->expect(")");
			name = parser->expectName();
			//sugar
			//
			parser->expect("[");
			parser->expect("precedence");
			parser->expect(":");
			precedence = expectInteger(parser,0);
			parser->expect("]");

		}
		else name = parser->expectName();
		
		auto bodyScope = new Scope(parser->currentScope());
		Function* func = new Function(name,location,bodyScope);
		if(infix){
			infix->type.infer(intrinsics::ast::ExprPtr);
			func->arguments.push_back(infix);
			bodyScope->define(infix);
		}
		bodyScope->_functionOwner = func;
		func->_returnType.infer(intrinsics::ast::ExprPtr);
		if(infix) parser->currentScope()->define(new InfixMacro2(func,precedence));
		else parser->currentScope()->define(new PrefixMacro2(func));

		auto oldScope = parser->currentScope();
		parser->currentScope(bodyScope);
		intrinsics::ast::onMacroScope(bodyScope);
		DefParser::functionBody(func,parser,false);
		parser->currentScope(oldScope);
		func->resolve(parser->evaluator());
		return new UnitExpression();
	}
};


struct ConstraintParser: PrefixDefinition {
	ConstraintParser(): PrefixDefinition("constraint",Location()) {}
	Node* parse(Parser* parser){
		auto location = parser->previousLocation();
		auto name = parser->expectName();
		auto bodyScope = new Scope(parser->currentScope());
		auto constraint = new ConstraintFunction(name,location,bodyScope);
		parser->currentScope()->define(constraint);
		bodyScope->_functionOwner = constraint;

		auto oldScope = parser->currentScope();
		parser->currentScope(bodyScope);

		parser->expect("(");
		auto param = new Argument(parser->expectName(),parser->previousLocation());
		param->type.infer(intrinsics::types::Type->duplicate()->asTypeExpression());
		constraint->arguments.push_back(param);
		bodyScope->define(param);

		if(parser->match(",")){
			auto param = new Argument(parser->expectName(),parser->previousLocation());
			param->type.kind = InferredUnresolvedTypeExpression::Wildcard;
			constraint->arguments.push_back(param);
			bodyScope->define(param);
		}
		parser->expect(")");
		constraint->_returnType = intrinsics::types::boolean->duplicate()->asTypeExpression();

		DefParser::functionBody(constraint,parser,false);
		parser->currentScope(oldScope);
		constraint->resolve(parser->evaluator());
		return new UnitExpression();
	}
};


/// ::= 'type' <name> {body|'=' type}
struct TypeParser: PrefixDefinition {
	TypeParser(): PrefixDefinition("type",Location()) {  }
	// fields ::= ['extends'] {'var'|'val'} <name>,... {type ['=' initialValue]|['=' initialValue]}
	static void fields(Record* record,Parser* parser,bool val = false,bool extender = false){
		size_t i = record->fields.size();
		do {
			auto field = Record::Field(parser->expectName(),intrinsics::types::Void);
			field.isExtending = extender;
			record->add(field);
		}
		while(parser->match(","));
		InferredUnresolvedTypeExpression type;
		type.parse(parser,arpha::Precedence::Assignment);
		for(;i<record->fields.size();i++) record->fields[i].type = type;
	}
	// body ::= '{' fields ';' fields ... '}'
	struct BodyParser {
		Record* record;
		BodyParser(Record* _record) : record(_record) {}
		bool operator()(Parser* parser){
			auto token = parser->consume();
			if(token.isSymbol()){
				bool extender = false;
				if(token.symbol == "extends"){
					extender = true;
					token = parser->consume();
				}
				if(token.symbol == "var"){
					fields(record,parser,false,extender);
					return true;
				}else if(token.symbol == "def"){
					fields(record,parser,true,extender);
					return true;
				}
			}
			error(parser->previousLocation(),"Unexpected %s - a field declaration is expected inside type's %s body!",token,record->id);
			return false;
		}
	};
		

	Node* parse(Parser* parser){
		auto location  = parser->previousLocation();
		auto name = parser->expectName();

		if(parser->match("integer")){
			debug("defined integer type");
			auto type = new IntegerType(name,location);
			parser->currentScope()->define(type);
			return new TypeExpression(type);
		}else if(parser->match("intrinsic")){
			debug("Defined intrinsic type %s",name);
			auto type = new IntrinsicType(name,location);
			parser->currentScope()->define(type);
			return new TypeExpression(type);
		}else if(parser->match("pointer")){
			debug("Defined pointer type %s",name);
			auto type = new PointerType(name,location);
			parser->currentScope()->define(type);
			return new UnitExpression;//TODO
		}
		auto record = new Record(name,location);
		parser->currentScope()->define(record);
		
		//fields
		if(parser->match("{")){
			blockParser->body(parser,BodyParser(record));
		}
		else {
			parser->expect("=");
			auto typeExpre = parser->parse();//TODO aliased type?
		}
		record->resolve(parser->evaluator());
		return new TypeExpression(record);
	}
};

/// ::= 'return' expression
struct ReturnParser: PrefixDefinition {
	ReturnParser(): PrefixDefinition("return",Location()) {}
	Node* parse(Parser* parser){
		if(isEndExpression(parser->peek())) return new ReturnExpression(new UnitExpression());
		else return new ReturnExpression(parser->parse());
	}
};

/// ::= '&' expression
struct AddressParser: PrefixDefinition {
	AddressParser(): PrefixDefinition("&",Location()) {}
	Node* parse(Parser* parser){
		return new PointerOperation(parser->parse(arpha::Precedence::Unary),PointerOperation::ADDRESS);
	}
};

/// ::= '*' expression
struct DereferenceParser: PrefixDefinition {
	DereferenceParser(): PrefixDefinition("*",Location()) {}
	Node* parse(Parser* parser){
		return new PointerOperation(parser->parse(arpha::Precedence::Unary),PointerOperation::DEREFERENCE);
	}
};

/// ::= match expr { to pattern: ... }
struct MatchParser: PrefixDefinition {
	MatchParser(): PrefixDefinition("match",Location()){}
	
	// pattern ::= '_'|expression
	static Node* pattern(Parser* parser){
		if(parser->match("_")) return new WildcardExpression;
		else return parser->parse();
	}
	// body ::= '{' 'to' pattern ':' consequence... '}'
	struct BodyParser {
		MatchExpression* node;
		BodyParser(MatchExpression* expr) : node(expr) {}
		bool operator()(Parser* parser){
			auto token = parser->consume();
			if(token.isSymbol() && token.symbol == "to"){
				parser->expect("(");
				bool fallthrough = true;
				Node* consq = nullptr;
				auto pattern = MatchParser::pattern(parser);
				parser->expect(")");
				//if(parser->match("=>")){
					fallthrough = false;
					consq = parser->parse();
				//}
				node->cases.push_back(MatchExpression::Case(pattern,consq,fallthrough));
				return true;
			}
			error(parser->previousLocation(),"Unexpected %s - to <pattern> : <consequence> is expected inside match's body!",token);
			return false;
		}
	};
	Node* parse(Parser* parser){
		auto expr = new MatchExpression(parser->parse());
		parser->expect("{");
		blockParser->body(parser,BodyParser(expr));
		return expr;
	}
};

/// TODO ::= 'for' values 'in' sequence expression

/// ::= 'import' <module>,...
struct ImportParser: PrefixDefinition {
	ImportParser(): PrefixDefinition("import",Location()) {}
	Node* parse(Parser* parser){
		Location location;
		SymbolID moduleName;
		bool qualified = false,exported = false;
		if(parser->match("export")) exported = true;
		if(parser->match("qualified")) qualified = true;
		do {
			location = parser->currentLocation();
			auto initial = parser->expectName();
			std::string modulePath = initial.ptr();
			while(parser->match(".")){
				modulePath += '/';
				modulePath += parser->expectName().ptr();
			}
			if(auto moduleScope = compiler::findModule(modulePath.c_str())){
				debug("Importing %s.",modulePath);
				parser->currentScope()->import(moduleScope,modulePath.c_str(),qualified,exported);
			}else{
				//Error
				error(location,"module '%s' wasn't found!",modulePath);
			}
		}while(parser->match(","));
		return new UnitExpression;
	}
};

struct CommandParser: PrefixDefinition {
	CommandParser(): PrefixDefinition("@",Location()) {}

	enum {
		None,
		Functions,
		Param,
		
	};

	void functions();
	Node* parse(Parser* parser){
		int state = None;
		SymbolID param;
		InferredUnresolvedTypeExpression type;
		while( !isEndExpression(parser->peek()) ){
			auto tok = parser->expectName();
			if(tok == "functions" && state == None) state = Functions;
			if((tok == "parameter" || tok == "argument") && state == Functions){
				param = parser->expectName();
				state = Param;
			}
			if((tok == "type") && state == Param){
				type.parse(parser,arpha::Precedence::Assignment);
				debug("Command: param %s has type %s",param,type.type());
				state = Functions;
			}
		}
		return new UnitExpression;
	}
};

//TODO refactor
Node* equals(Node* parameters){
	auto t = parameters->asTupleExpression();
	auto e = new IntegerLiteral(BigInt((int64) t->children[0]->asTypeExpression()->isSame(t->children[1]->asTypeExpression()) ));
	e->_type = intrinsics::types::boolean;
	return e;
}
Node* _typeof(CallExpression* node,Evaluator* evaluator){
	return node->arg->_returnType();
}
Node* _sizeof(CallExpression* node,Evaluator* evaluator){
	size_t size;
	if(auto t = node->arg->asTypeExpression()) size = t->size();
	else size = node->arg->_returnType()->size();
	auto e = new IntegerLiteral(BigInt((uint64) size));
	e->_type = intrinsics::types::int32;//TODO natural
	return e;
}



void arphaPostInit(Scope* moduleScope){
	auto x = ensure( ensure(moduleScope->lookupPrefix("equals"))->asOverloadset() )->functions[0];
	x->constInterpreter = equals;
	/*x = ensure( ensure(moduleScope->lookupPrefix("typeof"))->asOverloadset() )->functions[0];
	x->intrinsicEvaluator = _typeof;
	x = ensure( ensure(moduleScope->lookupPrefix("sizeof"))->asOverloadset() )->functions[0];
	x->intrinsicEvaluator = _sizeof;

	auto macro = ensure( dynamic_cast<PrefixMacro*>( ensure(moduleScope->containsPrefix("while")) ) );
	macro->syntax->intrinsicEvaluator = createWhile;*/
	//ensure( dynamic_cast<InfixMacro*>( ensure(moduleScope->containsInfix("(")) ) )->syntax->intrinsicEvaluator = createCall;
}
void coreSyntaxPostInit(Scope* moduleScope){

}
namespace intrinsics {
	namespace operations {
		Node* boolOr(Node* parameters){
			auto t = parameters->asTupleExpression();
			auto result = (!t->children[0]->asIntegerLiteral()->integer.isZero()) || (!t->children[1]->asIntegerLiteral()->integer.isZero());
			auto e = new IntegerLiteral(BigInt((uint64)(result?1:0)));
			e->_type = intrinsics::types::boolean;
			return e;
		}
		Node* boolAnd(Node* parameters){
			auto t = parameters->asTupleExpression();
			auto result = (!t->children[0]->asIntegerLiteral()->integer.isZero()) && (!t->children[1]->asIntegerLiteral()->integer.isZero());
			auto e = new IntegerLiteral(BigInt((uint64)(result?1:0)));
			e->_type = intrinsics::types::boolean;
			return e;
		}
		void init(Scope* moduleScope){
			auto x = ensure( ensure(moduleScope->lookupPrefix("or"))->asOverloadset() )->functions[0];
			x->constInterpreter = boolOr;
			x = ensure( ensure(moduleScope->lookupPrefix("and"))->asOverloadset() )->functions[0];
			x->constInterpreter = boolAnd;
		};
	}
}

struct CaptureParser : PrefixDefinition {
	BlockParser* blockParser;
	CaptureParser(): PrefixDefinition("[>",Location()) {
		blockParser = new BlockParser;
		blockParser->closingBrace = "<]"; 
	}

	Node* parse(Parser* parser){
		auto res = new ValueExpression(blockParser->parse(parser),intrinsics::ast::ExprPtr);
		return res;
	}
};

void astInit(Scope* moduleScope){
	moduleScope->define(new CaptureParser);
	intrinsics::ast::init(moduleScope);
}

void arpha::defineCoreSyntax(Scope* scope){
	Location location(0,0);
	::arpha::scope = scope;

	blockParser = new BlockParser;
	scope->define(new ImportParser);
	scope->define(blockParser);
	scope->define(new ParenParser);
	scope->define(new CallParser);
	scope->define(new TupleParser);
	scope->define(new AccessParser);
	scope->define(new AssignmentParser);

	scope->define(new DefParser);
	scope->define(new Macro2Parser);
	scope->define(new ConstraintParser);
	scope->define(new TypeParser);
	scope->define(new VarParser);
	

	scope->define(new ReturnParser);
	scope->define(new MatchParser);

	scope->define(new AddressParser);
	scope->define(new DereferenceParser);

	scope->define(new CommandParser);

	compiler::registerResolvedIntrinsicModuleCallback("arpha/arpha",arphaPostInit);

	compiler::registerResolvedIntrinsicModuleCallback("arpha/ast/ast",astInit);
	compiler::registerResolvedIntrinsicModuleCallback("arpha/types",intrinsics::types::init);
	compiler::registerResolvedIntrinsicModuleCallback("arpha/compiler/compiler",intrinsics::compiler::init);
	compiler::registerResolvedIntrinsicModuleCallback("arpha/operations",intrinsics::operations::init);
}


namespace compiler {

	struct Module {
		std::string directory;
		Scope* scope;
		Node*  body;
		bool compile;
	};

	typedef std::map<std::string,Module>::iterator ModulePtr;
	std::map<std::string,Module> modules;
	ModulePtr currentModule;

	std::string packageDir;

	std::map<std::string,void (*)(Scope*)> postCallbacks;


	void registerResolvedIntrinsicModuleCallback(const char* name,void (* f)(Scope*)){
		postCallbacks[packageDir+"/"+name+".arp"] = f;
	}

	Interpreter* interpreter;

	Unit _currentUnit;

	Unit *currentUnit(){
		return &_currentUnit;
	}
	Unit::State::State() : interpret(true) {}
	void Unit::updateState(Unit::State& state){
		_state = state;
	}
	const Unit::State& Unit::state() const { return _state; }

	ModulePtr newModule(const char* moduleName,const char* source){
		Module module = {};
		auto insertionResult = modules.insert(std::make_pair(std::string(moduleName),module));

		auto prevModule = currentModule;
		currentModule = insertionResult.first;

		currentModule->second.directory = System::path::directory(moduleName);

		Scope* scope;
		//Special case for 'packages/arpha/arp.arp'
		if((packageDir + "/arpha/arpha.arp") == moduleName){
			scope = new Scope(nullptr);	
			//import 'arpha' by default
			arpha::defineCoreSyntax(scope);
		}
		else {
			scope = new Scope(nullptr);
			//import 'arpha' by default
			scope->import(findModule("arpha"),"arpha");
		}
		currentModule->second.scope = scope;

		Evaluator evaluator(interpreter);
		Parser parser(source,&evaluator);
		_currentUnit.evaluator = &evaluator;
		_currentUnit.interpreter = interpreter;
		_currentUnit.parser = &parser;
		currentModule->second.body = parseModule(&parser,scope);

		auto cb = postCallbacks.find(moduleName);
		if(cb != postCallbacks.end()) (*cb).second(scope);

		debug("------------------- AST: ------------------------------");
		debug("%s\n",currentModule->second.body);

		parser.evaluator()->evaluateModule(currentModule->second.body->asBlockExpression());

		//restore old module ptr
		currentModule = prevModule;
		return insertionResult.first;
	}

	ModulePtr newModuleFromFile(const char* filename){
		auto src = System::fileToString(filename);
		auto module = newModule(filename,(const char*)src);
		System::free((void*)src);
		return module;
	}

	//Module importing is done by searching in the appropriate directories
	Scope* findModuleFromDirectory(std::string& dir,const char* name){
		//Try non package way
		auto filename = dir + "/" + name + ".arp";
		if(!System::fileExists(filename.c_str())){
			//Try package way
			filename = dir + "/" + name + "/" + System::path::filename(name) + ".arp";
			if(!System::fileExists(filename.c_str())) return nullptr;
		}
		//load module
		auto module = modules.find(filename);
		if(module == modules.end()){
			onDebug(format("A new module %s located at '%s' will be loaded.",name,filename));
			module = newModuleFromFile(filename.c_str());
		}
		return module->second.scope;
	}
	//Finds a module and loads it if necessary to match the existing name
	Scope* findModule(const char* name){
		//Search in the current directory
		if(currentModule != modules.end()){
			auto module = findModuleFromDirectory(currentModule->second.directory,name);
			if(module) return module;
		}
		//Search in the packages directory for a package
		return findModuleFromDirectory(packageDir,name);
	}

	int reportLevel;
	//Settings
	size_t wordSize,pointerSize;

	void init(){
		interpreter = constructInterpreter(nullptr);
		currentModule = modules.end();
		packageDir = "D:/alex/projects/parser/packages";
		
		wordSize = 4;
		pointerSize = 4;

		reportLevel = ReportDebug;

		intrinsics::types::startup();
		intrinsics::ast::startup();
		//Load language definitions.
		newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str());
		//auto arphaModule = newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str());

	
	}

	void onDebug(const std::string& message){
		if(reportLevel >= ReportDebug) System::debugPrint(message);
	}
	void onError(Location& location,const std::string& message){
		if(reportLevel >= ReportErrors)
			std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
	}

}


void runTests();

int main(int argc, char * const argv[]){
	
	System::init();
	memory::init();
	compiler::init();
	runTests();
	
	if(argc < 2){

		System::print("\nWelcome to arpha code console. Type in the code and press return twice to compile it!\n");
		std::string source = "";//"import arpha.testing.testing\n";
		char buf[1024];
		while(true){
			std::cout<<"> ";
			std::cin.getline(buf,1024);
			if(buf[0]=='\0'){
				 auto mod = compiler::newModule("source",source.c_str());
				 source = "";
				 continue;
			}
			source+=buf;
			source+="\n";
		}
	}else{
		System::print("\nSorry, can't accept files yet!\n");
	}

	memory::shutdown();
	System::shutdown();
			
	return 0;
}
