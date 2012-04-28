#include "common.h"
#include "base/system.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"
#include "interpreter.h"
#include "syntax/ast.h"
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

	SymbolID closingParenthesis;

	/// ::= '(' expression ')'
	struct ParenthesisParser: PrefixDefinition {
		ParenthesisParser(): PrefixDefinition("(",Location()) {}
		Node* parse(Parser* parser){
			if( parser->match(closingParenthesis) ){
				error(parser->previousLocation(),"() is an illegal expression!");
				return ConstantExpression::create(compiler::Error);
			}
			auto e = parser->_parse();
			parser->expect(closingParenthesis);
			return e;
		}
	};

	/// ::= expression '(' expression ')'
	struct CallParser: InfixDefinition {
		CallParser(): InfixDefinition("(",arpha::Precedence::Call,Location()) {}
		Node* parse(Parser* parser,Node* node){
			Node* arg;
			if( parser->match(closingParenthesis) ) arg = ConstantExpression::create(arpha::Nothing);
			else{
				arg = parser->_parse();
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
			return TupleExpression::create(node,parser->_parse(arpha::Precedence::Tuple)); 
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
			return AccessExpression::create(node,parser->lookedUpToken.symbol);
		}
	};

	/// ::= expression '=' expression
	struct AssignmentParser: InfixDefinition {
		AssignmentParser(): InfixDefinition("=",arpha::Precedence::Assignment,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return AssignmentExpression::create(node,parser->_parse(arpha::Precedence::Assignment)); 
		}
	};

	Node* parseFunction(SymbolID name,Location location,Parser* parser){
		//Function
		auto func = new Function(name,location);

		//parse arguments
		func->argument = compiler::Nothing;
		if(!parser->match(")")){

			std::vector<Function::Argument> arguments;
			while(1){
				//parse argument's name
				SymbolID argName = parser->expectName();
				auto var = Variable(argName,parser->previousLocation());
				var.type = compiler::anyType;
				arguments.push_back(Function::Argument(var));

				if(parser->match(")")) break;
				if(!parser->match(",")){
					//parse additional information, like argument's type
					auto node = parser->_parse(arpha::Precedence::Tuple);
					const ConstantExpression* val;
					if( (val = node->asConstantExpression()) && val->type == compiler::type) arguments.back().variable.type = val->refType;
					else error(node->location,"a valid type is expected instead of %s",node);

					if(parser->match(")")) break;
					parser->expect(",");
				}
			}
			func->arguments = arguments;

			//give the argument an appropriate type representation
			if(arguments.size()>1){
				std::vector<std::pair<SymbolID,Type*>> fields;
				for(auto i=arguments.begin();i!=arguments.end();++i) fields.push_back(std::make_pair((*i).variable.id,(*i).variable.type));
				func->argument = Type::tuple(fields);
			}else func->argument = arguments.begin()->variable.type;
		}
		debug("Function's argumentType = %s",func->argument->id);
		parser->currentScope()->defineFunction(func);

		//parse returnType
		func->returnType = compiler::inferred;									//def f(...) = 2             #returns int32, as indicated by 2
		if(parser->peek().isEndExpression() || parser->peek().isEOF()) func->returnType = compiler::Nothing; //def definitionOnly(...);   #returns void
		else if(auto t = parser->parseOptionalType()) func->returnType = t;	    //def foo(...) int32 { ... } #returns int32
		debug("Function's returnType = %s",func->returnType->id);

		//parse body
	
		//
		return ConstantExpression::create(compiler::Nothing);
	}

	/// := 'def' <name> '=' expression
	struct DefParser: PrefixDefinition {
		DefParser(): PrefixDefinition("def",Location()) {  }
		Node* parse(Parser* parser){
			auto location  = parser->previousLocation();
			auto name = parser->expectName();
			if(parser->match("(")){ 
				return parseFunction(name,location,parser);
			}
			else{
				parser->expect("=");
				auto sub = new Substitute(name,location);
				sub->expression = parser->_parse(arpha::Precedence::Tuple);
				parser->currentScope()->define(sub);
				return sub->expression;
			}
		}
	};

	/// ::= 'type' <name> '{' <fields> type '}'
	struct TypeParser: PrefixDefinition {
		TypeParser(): PrefixDefinition("type",Location()) {  }
		Node* parse(Parser* parser){
			auto location  = parser->previousLocation();
			auto name = parser->expectName();
			auto type = new Type(name,location);
			parser->currentScope()->define(type);
			//fields
			if(parser->match("{")){
				parser->expect("}");
			}

			return type->parse(parser);
		}
	};

	/// ::= 'var' <names> [type]
	struct VarParser: PrefixDefinition {
		VarParser(): PrefixDefinition("var",Location()) {}
		Node* parse(Parser* parser){
			std::vector<Node*> vars;
			do{
				auto name = parser->expectName();
				auto var = new Variable(name,parser->previousLocation());
				parser->_currentScope->define(var);
				vars.push_back(VariableExpression::create(var));
			}while(parser->match(","));

			if(auto type = parser->parseOptionalType()){
				for(auto i = vars.begin();i!=vars.end();++i) (*i)->asVariableExpression()->variable->inferType(type);
			}

			if(vars.size() == 1) return vars[0];
			auto tuple = TupleExpression::create();
			tuple->children = vars;
			return tuple;
		}
	};

	/// ::= 'operator' <name> [with priority <number>] = functionName
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
			return ReturnExpression::create(parser->_parse());
		}
	};

	/// ::= 'if' condition 'then' trueExpression 'else' falseExpression
	struct IfParser: PrefixDefinition {
		IfParser(): PrefixDefinition("if",Location()) {}
		Node* parse(Parser* parser){
			auto condition = parser->_parse();
			parser->expect("then");
			auto expr = parser->_parse();
			Node* elseExpr = parser->match("else") ? parser->_parse() : nullptr;
			return IfExpression::create(condition,expr,elseExpr);
		}
	};

	/// ::= 'for' values 'in' sequence statement

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

		closingParenthesis = SymbolID(")");
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
		//TOREVIEW the need to set isLiteral
		auto constant = new Substitute("true",location);
		constant->expression = value;
		scope->define(constant);

		value = ConstantExpression::create(boolean);
		value->u64 = 0;
		//TOREVIEW the need to set isLiteral
		constant = new Substitute("false",location);
		constant->expression = value;
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


		Parser parser(source,scope);
		currentModule->second.body = parser._parseModule();

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
	Type* inferred;
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
		Nothing = builtInType("cAbsolutelyNothing");
		Error = builtInType("Error");
		Unresolved = builtInType("Unresolved");
		inferred = builtInType("inferred");
		anyType = builtInType("anyType");
		function = builtInType("funtype");
		scopeRef = builtInType("scope");

		//Load language definitions.
		auto arphaModule = newModuleFromFile((packageDir + "/arpha/arpha.arp").c_str());

		Interpreter::init(scope,arphaModule->second.scope);
	
	}

	void onError(Location& location,std::string message){
		std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
	}

}


int main(int argc, char * const argv[]){
	
	System::init();
	memory::init();
	compiler::init();
	
	if(argc < 2){

		System::print("\nEnter arpha code followed by 2 newlines here:\n");
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
	}

	memory::shutdown();
	System::shutdown();
			
	return 0;
}
