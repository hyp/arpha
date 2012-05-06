#include "base/base.h"
#include "base/symbol.h"
#include "base/system.h"
#include "scope.h"
#include "ast/declarations.h"
#include "syntax/parser.h"
#include "ast/node.h"
#include "ast/evaluate.h"
#include "compiler.h"
#include "arpha.h"
#include "intrinsics/ast.h"
#include "intrinsics/types.h"

//TODO functions arguments vector!
namespace arpha {
	Scope *scope;


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
				_block->children.push_back(parser->parse());
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

	/// ::= '(' expression ')'
	struct ParenthesisParser: PrefixDefinition {
		SymbolID closingParenthesis;
		ParenthesisParser(): PrefixDefinition("(",Location()) {
			closingParenthesis = ")";
		}
		Node* parse(Parser* parser){
			if( parser->match(closingParenthesis) )
				return UnitExpression::getInstance();
			auto e = parser->parse();
			parser->expect(closingParenthesis);
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
			if( parser->match(closingParenthesis) ) arg = UnitExpression::getInstance();
			else{
				arg = parser->parse();
				parser->expect(closingParenthesis);
			}
			return CallExpression::create(node,arg);
		}
	};

	/// ::= expression '[' expression ']'
	struct IndexParser: InfixDefinition {
		IndexParser(): InfixDefinition("[",arpha::Precedence::Call,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return nullptr;//TODO
		}
	};

	/// ::= expression ',' expression
	struct TupleParser: InfixDefinition {
		TupleParser(): InfixDefinition(",",arpha::Precedence::Tuple-1,Location()) {}
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
			/*if(auto val = node->asConstantExpression()){
				if(val->type == compiler::scopeRef){
					//auto def = val->refScope->lookupImportedPrefix(parser->lookedUpToken.symbol);
					if(!def){
						error(node->location,"Unresolved symbol - '%s' isn't defined in module!",parser->lookedUpToken.symbol);
						return ErrorExpression::getInstance();
					}
					debug("accessing '%s'",def->id);
					auto expression = def->parse(parser);
					//apply the correct overload lookup scope
					if(auto overloadSet = expression->asOverloadSetExpression()) overloadSet->scope = val->refScope;
					return expression;
				}
			}*/
			return new AccessExpression(node,parser->lookedUpToken.symbol);
		}
	};

	/// ::= expression '=' expression
	struct AssignmentParser: InfixDefinition {
		AssignmentParser(): InfixDefinition("=",arpha::Precedence::Assignment,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return new AssignmentExpression(node,parser->parse(arpha::Precedence::Assignment)); 
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
		Node* parse(Parser* parser){
			std::vector<Variable*> vars;
			do {
				auto var = new Variable(parser->expectName(),parser->previousLocation());
				parser->currentScope()->define(var);
				vars.push_back(var);
			}
			while(parser->match(","));
			//parse optional type
			InferredUnresolvedTypeExpression type;
			if(!isEndExpressionEquals(parser->peek())) type.parse(parser,arpha::Precedence::Assignment);
			for(auto i=vars.begin();i!=vars.end();i++) (*i)->type = type;

			if(vars.size() == 1) return vars[0]->reference();
			auto tuple = new TupleExpression;
			for(auto i=vars.begin();i!=vars.end();i++) tuple->children.push_back((*i)->reference());
			return tuple;
		}
	};

	/// ::= 'def' <name> '=' expression
	/// ::= 'def' <name> '(' args ')' [returnType] body
	struct DefParser: PrefixDefinition {
		DefParser(): PrefixDefinition("def",Location()) {  }

		/// body ::= [nothing|'=' expression|'{' block '}']
		static void functionBody(Function* func,Parser* parser){
			auto token = parser->peek();
			if(token.isLine() || token.isEOF() || (token.isSymbol() && token.symbol == blockParser->lineAlternative)){
				func->body = nullptr;
			}else{
				func->body = new BlockExpression(func->bodyScope);
				auto oldScope = parser->currentScope();
				parser->currentScope(func->bodyScope);
				if(parser->match("="))
					func->body->children.push_back(parser->evaluate(new ReturnExpression(parser->parse())));
				else {
					parser->expect("{");
					blockParser->body(parser,BlockParser::BlockChildParser(func->body));
				}
				parser->currentScope(oldScope);
			}
		}


		static Node* function(SymbolID name,Location location,Parser* parser){
			/*Function
			auto func = new Function(name,location);
			parser->currentScope()->defineFunction(func);
			auto declaration = FunctionDeclaration::create(func);
			declaration->returnTypeExpression = nullptr;
			func->bodyScope = new Scope(parser->currentScope());

			//parse arguments
			if(!parser->match(")")){
				while(1){
					auto loc = parser->currentLocation();
					auto argName = parser->expectName();
					Function::Argument arg(new Variable(argName,location));
					func->bodyScope->define(arg.variable);
					func->arguments.push_back(arg);
					auto next = parser->peek();
					FunctionDeclaration::Parameter param;
					if(next.isSymbol() && ( next.symbol == "," || next.symbol == ")")){
						param.typeExpression = TypeReference::create(compiler::anyType);
					}else{
						param.typeExpression = parser->parse(arpha::Precedence::Tuple);
					}
					declaration->parameters.push_back(param);
					if(parser->match(")")) break;
					parser->expect(",");
				}
			}
			//return type & body
			auto token = parser->peek();
			if(token.isLine() || token.isEOF() || (token.isSymbol() && token.symbol == blockParser->lineAlternative)){
				func->returnType = compiler::Nothing;
				func->body = nullptr;
			}
			else {
				func->returnType = compiler::Unresolved;	
				if(!(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
					declaration->returnTypeExpression = parser->parse(arpha::Precedence::Assignment);
				}
				functionBody(func,parser);
			}
			return declaration;*/
			return nullptr;
		}

		Node* parse(Parser* parser){
			auto location  = parser->previousLocation();
			auto name = parser->expectName();
			if(parser->match("(")) return function(name,location,parser);
			else{
				auto sub = new Variable(name,location);
				sub->isMutable = false;
				parser->currentScope()->define(sub);
				//parse optional type
				InferredUnresolvedTypeExpression type;
				if(!isEndExpressionEquals(parser->peek())) type.parse(parser,arpha::Precedence::Assignment);
				sub->type = type;
				return sub->reference();
			}
		}
	};

	/// ::= 'type' <name> {body|'=' type}
	struct TypeParser: PrefixDefinition {
		TypeParser(): PrefixDefinition("type",Location()) {  }
		// fields ::= ['extends'] {'var'|'val'} <name>,... {type ['=' initialValue]|['=' initialValue]}
		static void fields(Record* record,Parser* parser,bool val = false,bool extender = false){
			size_t i = record->fields.size();
			do {
				auto field = Record::Field(parser->expectName(),intrinsics::types::Unresolved);
				field.isExtending = extender;
				record->add(field);
			}
			while(parser->match(","));
			InferredUnresolvedTypeExpression type;
			type.parse(parser,arpha::Precedence::Assignment);
			record->_resolved = type.resolved();
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
					}
				}
				error(parser->previousLocation(),"Unexpected %s - a var is expected inside type's %s body!",token,record->id);
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
			}
			auto record = new Record(name,location);
			record->_resolved = true;
			parser->currentScope()->define(record);
		
			//fields
			if(parser->match("{")){
				blockParser->body(parser,BodyParser(record));
			}
			else {
				parser->expect("=");
				auto typeExpre = parser->parse();//TODO
			}
			if(record->_resolved) record->updateOnSolving();
			return new TypeExpression(record);
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

	/// ::= 'operator' <name> ['with' 'priority' <number>] = functionName
	struct OperatorParser: PrefixDefinition {
		OperatorParser(): PrefixDefinition("operator",Location()) {}
		Node* parse(Parser* parser){
			auto loc  = parser->previousLocation();
			auto name = parser->expectName();
			if(parser->match("=")){
				auto op = new PrefixOperator(name,loc);
				op->function = parser->expectName();
				parser->currentScope()->define(op);
			}else{
				parser->expect("with");
				parser->expect("priority");
				auto stickiness = expectInteger(parser,arpha::Precedence::Tuple);
				parser->expect("=");
				auto op = new InfixOperator(name,stickiness,loc);
				debug("defined operator %d with stickiness %d",name,op->stickiness);
				op->function = parser->expectName();
				parser->currentScope()->define(op);
			}
			return UnitExpression::getInstance();
		}
	};

	/// ::= 'return' expression
	struct ReturnParser: PrefixDefinition {
		ReturnParser(): PrefixDefinition("return",Location()) {}
		Node* parse(Parser* parser){
			return new ReturnExpression(parser->parse());
		}
	};

	/// ::= 'if' '(' condition ')' consequence [ 'else' alternative ]
	struct IfParser: PrefixDefinition {
		IfParser(): PrefixDefinition("if",Location()) {}
		Node* parse(Parser* parser){
			parser->expect("(");
			auto condition = parser->parse();
			parser->expect(")");
			auto consq = parser->parse();
			Node* alt = parser->match("else") ? parser->parse() : nullptr;
			return nullptr;//TODO//IfExpression::create(condition,consq,alt);
		}
	};

	/// ::= 'while' condition body
	struct WhileParser: PrefixDefinition {
		WhileParser(): PrefixDefinition("while",Location()) {}
		Node* parse(Parser* parser){
			auto condition = parser->parse();
			auto body = parser->parse();
			return new WhileExpression(condition,body);
		}
	};

	/// TODO ::= match expr { |pattern: ... }
	struct MatchParser: PrefixDefinition {
		MatchParser(): PrefixDefinition("match",Location()){}
		Node* parse(Parser* parser){
			auto expr = new MatchExpression(parser->parse());
			parser->expect("{");

			parser->expect("}");
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
			int flags = 0;
			if(parser->match("export")) flags |= Scope::ImportFlags::BROADCAST;
			if(parser->match("qualified")) flags |= Scope::ImportFlags::QUALIFIED;
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
					parser->currentScope()->import(new ImportedScope(modulePath.c_str(),parser->previousLocation(),moduleScope),flags);
				}else{
					//Error
					error(location,"module '%s' wasn't found!",modulePath);
				}
			}while(parser->match(","));
			return UnitExpression::getInstance();
		}
	};

	void defineCoreSyntax(Scope* scope){
		Location location(0,0);
		::arpha::scope = scope;

		blockParser = new BlockParser;
		scope->define(blockParser);
		scope->define(new ParenthesisParser);
		scope->define(new CallParser);
		scope->define(new TupleParser);
		scope->define(new AccessParser);
		scope->define(new AssignmentParser);

		scope->define(new DefParser);
		scope->define(new TypeParser);
		scope->define(new VarParser);
		scope->define(new OperatorParser);
		scope->define(new ImportParser);

		scope->define(new ReturnParser);
		scope->define(new IfParser);
		scope->define(new WhileParser);
		scope->define(new MatchParser);


		//true & false

	}
};


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
	ModulePtr compilerModule;

	std::string packageDir;

	ModulePtr newModule(const char* moduleName,const char* source){
		Module module = {};
		auto insertionResult = modules.insert(std::make_pair(std::string(moduleName),module));

		auto prevModule = currentModule;
		currentModule = insertionResult.first;

		currentModule->second.directory = System::path::directory(moduleName);

		Scope* scope;
		//Special case for 'packages/arpha/compiler/compiler.arp'
		if((packageDir + "/arpha/compiler/compiler.arp") == moduleName){
			assert(compilerModule == modules.end());
			compilerModule = currentModule;
			scope = ::compiler::scope;
			//import 'arpha' by default
			auto def = new ImportedScope("arpha",Location(-1,0),::arpha::scope);
			scope->import(def);
		}else if((packageDir + "/arpha/arpha.arp") == moduleName){
			scope = new Scope(nullptr);
			arpha::defineCoreSyntax(scope);
			//import 'compiler' by default
			auto def = new ImportedScope("compiler",Location(-1,0),findModule("compiler"));
			scope->import(def,Scope::ImportFlags::QUALIFIED);
		}
		else {
			scope = new Scope(nullptr);
			//import 'compiler' by default
			auto def = new ImportedScope("compiler",Location(-2,0),::compiler::scope);
			scope->import(def,Scope::ImportFlags::QUALIFIED);
			//import 'arpha' by default
			def = new ImportedScope("arpha",Location(-1,0),findModule("arpha"));
			scope->import(def);
		}
		currentModule->second.scope = scope;


		Parser parser(source);
		currentModule->second.body = arpha::parseModule(&parser,scope);
		//TODO rm hACKS
		if((packageDir + "/arpha/ast/ast.arp") == moduleName){
			intrinsics::ast::init(scope);
		}else if((packageDir + "/arpha/types.arp") == moduleName){
			intrinsics::types::init(scope);
		}

		debug("------------------- AST: ------------------------------");
		debug("%s\n",currentModule->second.body);


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
			filename = dir + "/" + name + "/" + name + ".arp";
			if(!System::fileExists(filename.c_str())) return nullptr;
		}
		//load module
		auto module = modules.find(filename);
		if(module == modules.end()){
			System::debugPrint(format("A new module %s located at '%s' will be loaded.",name,filename));
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

	Scope* scope;


	void init(){
		compilerModule = currentModule = modules.end();

		packageDir = "D:/alex/projects/parser/packages";
		
		//scope for compiler module
		scope = new Scope(nullptr);

		intrinsics::types::preinit();

		//Load language definitions.
		auto arphaModule = newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str());

		Evaluator::init(scope,arphaModule->second.scope);
	
	}

	void onError(Location& location,std::string message){
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
