#include "common.h"
#include "base/system.h"
#include "scope.h"
#include "declarations.h"
#include "syntax/parser.h"
#include "ast/node.h"
#include "ast/evaluate.h"
#include "compiler.h"
#include "arpha.h"

unittest(scope){
	auto scope = new Scope(0);

	assert(!scope->lookupPrefix("foo"));
	assert(!scope->lookupPrefix("bar"));
	assert(!scope->lookupInfix("bar"));
	assert(!scope->lookupInfix("foo"));
	assert(!scope->containsPrefix("foo"));
	assert(!scope->containsPrefix("bar"));
	assert(!scope->containsInfix("bar"));
	assert(!scope->containsInfix("foo"));

	//normal scope lookup and contains
/*	auto pdata = (PrefixDefinition*)0xDEADBEEF;
	scope->define(Location(),"foo",pdata);
	auto pdef = scope->lookupPrefix("foo");
	assert(pdef);
	assert(pdef == pdata);
	pdef = scope->containsPrefix("foo");
	assert(pdef);
	assert(pdef == pdata);

	auto idata = (InfixDefinition*)0xDEADBEEF;
	scope->define(Location(),"foo",idata);
	auto idef = scope->lookupInfix("foo");
	assert(idef);
	assert(idef == idata);
	idef = scope->containsInfix("foo");
	assert(idef);
	assert(idef == idata);

	//lookup from parent
	auto scope2 = new Scope(scope);
	scope->define(Location(),"bar",pdata);
	scope->define(Location(),"bar",idata);
	assert(scope->containsPrefix("bar") && scope->containsInfix("bar"));
	assert((!scope2->containsPrefix("bar")) && (!scope2->containsInfix("bar")));
	assert(scope2->lookupPrefix("bar") == scope->containsPrefix("bar"));
	assert(scope2->lookupInfix("bar") == scope->containsInfix("bar"));*/

	//


	//delete scope2;
	delete scope;
	//clean up
	//symbols.~SymbolTable();
}



//TODO functions arguments vector!
namespace arpha {
	Scope *scope;

	//core types & functions
	Type* Nothing;
	Type *int8,*uint8,*int16,*uint16,*int32,*uint32,*int64,*uint64;
	Type *float64,*float32;
	Type *boolean;	
	Type *constantString;

	Type* builtInType(const char* name,int size){
		auto t = new Type(SymbolID(name),Location());
		t->size = size;
		scope->define(t);
		return t;
	}


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
				_block->children.push_back(parser->evaluate(parser->parse()));
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
			BlockExpression* block = BlockExpression::create(new Scope(oldScope));
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
		BlockExpression* block = BlockExpression::create(scope);
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
			if( parser->match(closingParenthesis) ){
				error(parser->previousLocation(),"() is an illegal expression!");
				return ConstantExpression::create(compiler::Error);
			}
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
			if( parser->match(closingParenthesis) ) arg = ConstantExpression::create(arpha::Nothing);//TODO ??? compiler.nothing
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
		TupleParser(): InfixDefinition(",",arpha::Precedence::Tuple,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return TupleExpression::create(node,parser->parse(arpha::Precedence::Tuple)); 
		}
	};

	/// ::= expression '.' expression
	struct AccessParser: InfixDefinition {
		AccessParser(): InfixDefinition(".",arpha::Precedence::Access,Location()) {}
		Node* parse(Parser* parser,Node* node){
			parser->lookedUpToken.type = Token::Symbol;
			parser->lookedUpToken.symbol = parser->expectName();
			//scope.something
			if(auto val = node->asConstantExpression()){
				if(val->type == compiler::scopeRef){
					auto def = val->refScope->lookupImportedPrefix(parser->lookedUpToken.symbol);
					if(!def){
						error(node->location,"Unresolved symbol - '%s' isn't defined in module!",parser->lookedUpToken.symbol);
						return ConstantExpression::create(compiler::Error);
					}
					debug("accessing '%s'",def->id);
					auto expression = def->parse(parser);
					//apply the correct overload lookup scope
					if(auto overloadSet = expression->asOverloadSetExpression()) overloadSet->scope = val->refScope;
					return expression;
				}
			}
			return AccessExpression::create(node,parser->lookedUpToken.symbol,parser->currentScope());
		}
	};

	/// ::= expression '=' expression
	struct AssignmentParser: InfixDefinition {
		AssignmentParser(): InfixDefinition("=",arpha::Precedence::Assignment,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return AssignmentExpression::create(node,parser->parse(arpha::Precedence::Assignment)); 
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
				func->body = BlockExpression::create(func->bodyScope);
				auto oldScope = parser->currentScope();
				parser->currentScope(func->bodyScope);
				if(parser->match("="))
					func->body->children.push_back(parser->evaluate(ReturnExpression::create(parser->parse())));
				else {
					parser->expect("{");
					blockParser->body(parser,BlockParser::BlockChildParser(func->body));
				}
				parser->currentScope(oldScope);
			}
		}


		static Node* function(SymbolID name,Location location,Parser* parser){
			//Function
			auto func = new Function(name,location);
			func->bodyScope = new Scope(parser->currentScope());

			//parse arguments
			func->argument = arpha::Nothing; ///??? compiler.nothing?
			if(!parser->match(")")){

				std::vector<Function::Argument> arguments;
				while(1){
					//parse argument's name
					auto loc = parser->currentLocation();
					auto argName = parser->expectName();
					auto var = new Variable(argName,loc);
					var->type = compiler::anyType;
					func->bodyScope->define(var);
					arguments.push_back(Function::Argument(var));

					if(parser->match(")")) break;
					if(!parser->match(",")){
						//parse additional information, like argument's type	
						arguments.back().variable->type = parser->expectType(arpha::Precedence::Tuple);
						if(parser->match(")")) break;
						parser->expect(",");
					}
				}
				func->arguments = arguments;

				//give the argument an appropriate type representation
				if(arguments.size()>1){
					std::vector<std::pair<SymbolID,Type*>> fields;
					for(auto i=arguments.begin();i!=arguments.end();++i) fields.push_back(std::make_pair((*i).variable->id,(*i).variable->type));
					func->argument = Type::tuple(fields);
				}else func->argument = arguments.begin()->variable->type;
			}
			debug("Function's argumentType = %s",func->argument->id);
			parser->currentScope()->defineFunction(func);

			//return type & body
			auto token = parser->peek();
			if(token.isLine() || token.isEOF() || (token.isSymbol() && token.symbol == blockParser->lineAlternative)){
				func->returnType = compiler::Nothing;
				func->body = nullptr;
			}
			else {
				func->returnType = compiler::Unresolved;	
				if(!(token.isSymbol() && (token.symbol == "=" || token.symbol == "{"))){
					func->returnType = parser->expectType(arpha::Precedence::Assignment);
				}
				debug("Function's returnType = %s",func->returnType->id);
				functionBody(func,parser);
			}
			return ConstantExpression::createFunctionReference(func);
		}

		Node* parse(Parser* parser){
			auto location  = parser->previousLocation();
			auto name = parser->expectName();
			if(parser->match("(")) return function(name,location,parser);
			else{
				parser->expect("=");
				auto sub = new Variable(name,location);
				sub->value = parser->parse(arpha::Precedence::Tuple);
				parser->currentScope()->define(sub);
				return sub->value;
			}
		}
	};

	/// ::= 'type' <name> {body|'=' type}
	struct TypeParser: PrefixDefinition {
		TypeParser(): PrefixDefinition("type",Location()) {  }
		// fields ::= {'var'|'val'} <name>,... {type ['=' initialValue]|['=' initialValue]}
		static void fields(Type* type,Parser* parser){
			std::vector<std::pair<SymbolID,Location>> vars;
			do vars.push_back(std::make_pair(parser->expectName(),parser->previousLocation()));
			while(parser->match(","));
			auto t = parser->expectType(arpha::Precedence::Assignment);
			for(auto i = vars.begin(); i != vars.end(); ++i){
				Variable v((*i).first,(*i).second);
				v.inferType(t);
				type->add(v);
			}
		}
		// body ::= '{' fields ';' fields ... '}'
		struct BodyParser {
			Type* _type;
			BodyParser(Type* type) : _type(type) {}
			bool operator()(Parser* parser){
				auto token = parser->consume();
				if(token.isSymbol()){
					if(token.symbol == "var"){
						fields(_type,parser);
						return true;
					}
				}
				error(parser->previousLocation(),"Unexpected %s - a var is expected inside a type %s body!",token,_type->id);
				return false;
			}
		};
		
		Node* parse(Parser* parser){
			auto location  = parser->previousLocation();
			auto name = parser->expectName();
			auto type = new Type(name,location);
			parser->currentScope()->define(type);
			//fields
			if(parser->match("{")) blockParser->body(parser,BodyParser(type));

			return type->parse(parser);
		}
	};

	/// ::= 'var' <names> [type|unresolvedExpression (hopefully) resolving to type|Nothing]
	struct VarParser: PrefixDefinition {
		VarParser(): PrefixDefinition("var",Location()) {}
		Node* parse(Parser* parser){
			std::vector<Node*> vars;
			do{
				auto name = parser->expectName();
				auto var = new Variable(name,parser->previousLocation());
				parser->currentScope()->define(var);
				vars.push_back(VariableExpression::create(var));
			}while(parser->match(","));
			//variable references expression
			Node* result;
			if(vars.size() == 1) 
				result = vars[0];
			else{  
				auto tuple = TupleExpression::create();
				tuple->children = vars;
				result = tuple;
			}
			//::= [type|unresolvedExpression (hopefully) resolving to type|Nothing]
			auto typeOrFutureType = parser->matchTypeOrUnresolved(arpha::Precedence::Assignment);
			if(typeOrFutureType.second)
				return VariableDeclaration::create(result,typeOrFutureType.second);	
			if(typeOrFutureType.first){
				for(auto i = vars.begin();i!=vars.end();++i) (*i)->asVariableExpression()->variable->inferType(typeOrFutureType.first);
			}
			return result;
		}
	};

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
				auto stickiness = parser->expectInteger();
				parser->expect("=");
				auto op = new InfixOperator(name,stickiness,loc);
				debug("defined operator %d with stickiness %d",name,op->stickiness);
				op->function = parser->expectName();
				parser->currentScope()->define(op);
			}
			return ConstantExpression::create(compiler::Nothing);
		}
	};

	/// ::= 'return' expression
	struct ReturnParser: PrefixDefinition {
		ReturnParser(): PrefixDefinition("return",Location()) {}
		Node* parse(Parser* parser){
			return ReturnExpression::create(parser->parse());
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
			return IfExpression::create(condition,consq,alt);
		}
	};

	/// ::= 'while' condition body
	struct WhileParser: PrefixDefinition {
		WhileParser(): PrefixDefinition("while",Location()) {}
		Node* parse(Parser* parser){
			auto condition = parser->parse();
			auto body = parser->parse();
			return WhileExpression::create(condition,body);
		}
	};

	/// TODO ::= match expr { to pattern: ... }

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
			return ConstantExpression::create(compiler::Nothing);
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


		Nothing = builtInType("Nothing",0);

		boolean = builtInType("bool",1);
		int8 = builtInType("int8",1);
		uint8 = builtInType("uint8",1);
		int16 = builtInType("int16",2);
		uint16 = builtInType("uint16",2);
		int32 = builtInType("int32",4);
		uint32 = builtInType("uint32",4);
		int64 = builtInType("int64",8);
		uint64 = builtInType("uint64",8);
		float64 = builtInType("double",8);
		float32 = builtInType("float",4);


		constantString = builtInType("String",0);

		//true & false
		auto value = ConstantExpression::create(boolean);
		value->u64 = 1;
		auto constant = new Variable("true",location);
		constant->value = value;
		scope->define(constant);

		value = ConstantExpression::create(boolean);
		value->u64 = 0;
		constant = new Variable("false",location);
		constant->value = value;
		scope->define(constant);

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
		}
		else {
			scope = new Scope(nullptr);
			//import 'arpha' by default
			auto def = new ImportedScope("arpha",Location(-1,0),findModule("arpha"));
			scope->import(def);
		}
		currentModule->second.scope = scope;


		Parser parser(source);
		currentModule->second.body = arpha::parseModule(&parser,scope);

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

	Type* expression;
	Type* type;
	Type* Nothing; 
	Type* Error;
	Type* Unresolved;
	Type* anyType; 

	Type* function;
	Type* scopeRef;

	Type* builtInType(const char* name){
		auto t = new Type(SymbolID(name),Location());
		scope->define(t);
		return t;
	}

	void init(){
		compilerModule = currentModule = modules.end();

		packageDir = "D:/alex/projects/parser/packages";
		
		//scope for compiler module
		scope = new Scope(nullptr);

		expression = builtInType("Expression");
		type = builtInType("Type");
		Nothing = builtInType("void");
		Error = builtInType("Error");
		Unresolved = builtInType("Unresolved");
		anyType = builtInType("anyType");
		function = builtInType("funtype");
		scopeRef = builtInType("scope");

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
		std::string source = "import arpha.testing.testing\n";
		char buf[1024];
		while(true){
			std::cout<<"> ";
			std::cin.getline(buf,1024);
			if(buf[0]=='\0') break;
			source+=buf;
			source+="\n";
		}
		auto mod = compiler::newModule("source",source.c_str());
	}else{
		System::print("\nSorry, can't accept files yet!\n");
	}

	memory::shutdown();
	System::shutdown();
			
	return 0;
}
