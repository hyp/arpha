#include "common.h"
#include "base/system.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"
#include "interpreter.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"



void printFunc(std::string message){
	std::cout<<message;
}







//a table of unique symbols for fast symbol comparison





//lexing

Location::Location(int line,int column){
	lineNumber = line;
	this->column = column;
}

unittest(location){
	auto loc = Location(2,3);
	assert(loc.line() == 2);
	assert(loc.column == 3);
}




//parsing








Scope::Scope(Scope* parent){
	this->parent = parent;
}


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




unittest(type){
	/*auto t = new Type(nullptr,SymbolID("foo"),4);
	assert(t->size == 4);
	auto t2 = new Type(nullptr,SymbolID("bar"),8);
	t->add(Type::Field(t2,SymbolID("b")));
	assert(t->size == 12);
	assert( (t->operator[](SymbolID("b")))->type == t2 );

	delete t2;
	delete t;
	//clean up
	symbols.~SymbolTable();*/
}



unittest(variable){
	Location location(0,0);
	/*auto t = new Type(nullptr,SymbolID(),0);
	auto v = new Variable(nullptr,SymbolID(),location,t);
	assert(v->type == t);
	assert(v->bindedConstant() == nullptr);

	v->bindToConstant((Expression*)0xDEADBEEF);
	assert(v->bindedConstant() == (Expression*)0xDEADBEEF);

	delete v;
	delete t;*/
}




//TODO functions arguments vector!
namespace arpha {
	Scope *scope;
	

	//core types & functions
	Type* type;
	Type* expression;
	Type* Nothing,*Unresolved,*inferred;
	Type *constant;
	Type *int8,*uint8,*int16,*uint16,*int32,*uint32,*int64,*uint64;
	Type *float64,*float32;
	Type *boolean;	

	enum {
		BOOL,CNST,I8,U8,I16,U16,I32,U32,I64,U64,F64,F32,TYPE_COUNT
	};
	Type* types[TYPE_COUNT];

	/*Function* createFunction(Scope* scope,const char* name,Type* returns,Type* arg0 = 0,const char *arg0n = 0,Type* arg1 = 0,const char *arg1n = 0){
		//SymbolID name;
		Location location(0,0);
		Type* argument = arpha::Nothing;
		//if(arg0) argument = arg1 ? Type::tuple(arg0,arg1) : arg0;
		//else if(arg1) argument = arg1;
		//if(argument->isTuple) argument->fields[0].name = arg0n;
		auto func = new Function(scope,name,argument,returns,0,0);
		//if(arg0) func->arguments.push_back(Function::Argument(Variable(0,arg0n,location,arg0),0,0));
		//if(arg1){ assert(arg0); func->arguments.push_back(Function::Argument(Variable(0,arg1n,location,arg1),0,0)); }
		return func;
	}

	OverloadSet* ArithmeticFunction(Scope*,const char*,int,int sticky = 0,bool returnSigned=false);
	OverloadSet* ArithmeticFunction(Scope* scope,const char* str,int numArguments,int sticky,bool returnSigned){
		Function* f;
		if(numArguments==1){
			createFunction(scope,str,constant,constant,"x");
			if(returnSigned){
				for(int i=I8;i<=U64;i++)
					createFunction(scope,str,types[ i%2==1 ? i-1 : i],types[i],"x");
			}else{
				for(int i=I8;i<=U64;i++)
					createFunction(scope,str,types[i],types[i],"x");
			}
			createFunction(scope,str,float32,float32,"x");
			f = createFunction(scope,str,float64,float64,"x");
		}else{	
			for(int i=CNST;i<TYPE_COUNT;i++)
				f =createFunction(scope,str,types[i],types[i],"x",types[i],"y");
			f->set->stickiness = sticky;
		}
		debug("%s set = %d",str,f->set);
		return f->set;
	}
	OverloadSet* ComparisonFunction(Scope* scope,const char* str,int sticky = -1){
			OverloadSet* set;
			SymbolID name(str);
			for(int i=0;i<TYPE_COUNT;i++)
				set = createFunction(scope,str,arpha::boolean,types[i],"x",types[i],"y")->set;
			set->stickiness = sticky;
			return set;
	}

	OverloadSet *add,*sub,*mul,*divide,*mod,*plus,*minus;
	OverloadSet *equals,*notequals,*less,*more,*lesseq,*moreeq;



	void basicFunctions(){
		add= ArithmeticFunction(scope,"add",2,30);
		sub= ArithmeticFunction(scope,"subtract",2,30);
		mul= ArithmeticFunction(scope,"multiply",2,35);
		divide= ArithmeticFunction(scope,"divide",2,35);
		mod= ArithmeticFunction(scope,"mod",2,35);
		plus= ArithmeticFunction(scope,"plus",1);
		minus= ArithmeticFunction(scope,"minus",1,-1,true);
		equals = ComparisonFunction(scope,"equals",25);
	}*/

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
				return parser->expressionFactory->makeError();
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
			if( parser->match(closingParenthesis) ) arg = parser->expressionFactory->makeNothing();
			else{
				arg = parser->_parse();
				parser->expect(closingParenthesis);
			}
			return parser->expressionFactory->makeCall(node,arg);
		}
	};

	/// ::= expression ',' expression
	struct TupleParser: InfixDefinition {
		TupleParser(): InfixDefinition(",",arpha::Precedence::Tuple,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return parser->expressionFactory->makeTuple(node,parser->_parse(arpha::Precedence::Tuple)); 
		}
	};

	/// ::= expression '.' expression
	struct AccessParser: InfixDefinition {
		AccessParser(): InfixDefinition(".",arpha::Precedence::Access,Location()) {}
		Node* parse(Parser* parser,Node* node){
			parser->lookedUpToken.type = Token::Symbol;
			parser->lookedUpToken.symbol = parser->expectName();
			//scope.something
			if(auto val = node->is<ConstantExpression>()){
				if(val->type == compiler::scopeRef){
					auto def = val->refScope->lookupImportedPrefix(parser->lookedUpToken.symbol);
					if(!def){
						error(node->location,"Unresolved symbol - '%s' isn't defined in module!",parser->lookedUpToken.symbol);
						return parser->expressionFactory->makeError();
					}
					debug("accessing '%s'",def->id);
					auto expression = def->parse(parser);
					//apply the correct overload lookup scope
					if(expression->is<OverloadSetExpression>()) ((OverloadSetExpression*)expression)->scope = val->refScope;
					return expression;
				}
			}
			return parser->expressionFactory->makeAccess(node,parser->lookedUpToken.symbol);
		}
	};

	/// ::= expression '=' expression
	struct AssignmentParser: InfixDefinition {
		AssignmentParser(): InfixDefinition("=",arpha::Precedence::Assignment,Location()) {}
		Node* parse(Parser* parser,Node* node){
			return nullptr;
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
				if( (val = node->is<ConstantExpression>()) && val->type == compiler::type) arguments.back().variable.type = val->refType;
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
	return parser->expressionFactory->makeCompilerNothing();
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
				vars.push_back(parser->expressionFactory->makeVariable(var));
			}while(parser->match(","));

			if(auto type = parser->parseOptionalType()){
				debug("variables are of type %s",type->id);
				for(auto i = vars.begin();i!=vars.end();++i) ((VariableExpression*)(*i))->variable->type = type;
			}

			if(vars.size() == 1) return vars[0];
			auto tuple = parser->expressionFactory->makeTuple();
			tuple->children = vars;
			return tuple;
		}
	};

	/// ::= 'operator' <name> [priority <number>] = functionName
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
				parser->expect("priority");
				auto stickiness = parser->expectInteger();
				parser->expect("=");
				auto op = new InfixOperator(name,stickiness,loc);
				debug("defined operator %d with stickiness %d",name,op->stickiness);
				op->function = parser->expectName();
				parser->currentScope()->define(op);
			}
			return parser->expressionFactory->makeCompilerNothing();
		}
	};

	/// ::= 'return' expression
	struct ReturnParser: PrefixDefinition {
		ReturnParser(): PrefixDefinition("return",Location()) {}
		Node* parse(Parser* parser){
			return parser->expressionFactory->makeReturn(parser->_parse());
		}
	};

	void init(Scope* scope){
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

		scope->define(new ReturnParser);

		Nothing = builtInType("Nothing",0);

		int i = 0;
		types[i++] = boolean = builtInType("bool",1);
		types[i++] = constant;
		types[i++] = int8 = builtInType("int8",1);
		types[i++] = uint8 = builtInType("uint8",1);
		types[i++] = int16 = builtInType("int16",2);
		types[i++] = uint16 = builtInType("uint16",2);
		types[i++] = int32 = builtInType("int32",4);
		types[i++] = uint32 = builtInType("uint32",4);
		types[i++] = int64 = builtInType("int64",8);
		types[i++] = uint64 = builtInType("uint64",8);
		types[i++] = float64 = builtInType("double",8);
		types[i++] = float32 = builtInType("float",4);

		//basicFunctions();

		//test();
	}
};


namespace compiler {

	struct Module {
		Scope* scope;
		Node*  body;
		ExpressionFactory expressionFactory; //TODO pointer it
		bool compile;
	};

	typedef std::map<std::string,Module>::iterator ModulePtr;

	std::map<std::string,Module> modules;

	ModulePtr currentModule;

	//packages/arpha/arpha.arp
	std::string arphaModuleName;
	ModulePtr arphaModule; 

	ModulePtr loadModule(const char* moduleName,const char* source){
		Module module = {};
		auto insertionResult = modules.insert(std::make_pair(std::string(moduleName),module));
		if(!insertionResult.second) return insertionResult.first;
		debug("new module %s created!",moduleName);

		auto prevModule = currentModule;
		currentModule = insertionResult.first;

		auto scope = currentModule->second.scope = new Scope(nullptr);
		//import 'compiler'
		auto def = new ImportedScope("compiler",Location(-2,0),::compiler::scope);
		scope->import(def,Scope::ImportFlags::FORCE_ALIAS);

		if(arphaModuleName == moduleName){
			//the original arpha module
			arphaModule = currentModule;
			arpha::init(scope);
		}else{
			//import 'arpha'
			auto def = new ImportedScope("arpha",Location(-1,0),arphaModule->second.scope);
			scope->import(def);
		}

		Parser parser(source,scope);
		parser.expressionFactory = &currentModule->second.expressionFactory;
		currentModule->second.body = parser._parseModule();

		debug("------------------- AST: ------------------------------");
		debug("%s\n",currentModule->second.body);


		//restore old module ptr
		currentModule = prevModule;
		return insertionResult.first;
	}

	std::string packageDir;

	Scope* importPackage(const char* name){
		auto filename = packageDir + name + "/" + name + ".arp";
		auto src = System::fileToString(filename.c_str());
		auto module = loadModule(filename.c_str(),(const char*)src);
		System::free((void*)src);
		return module->second.scope;
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

	packageDir = "D:/alex/projects/parser/packages/";
	arphaModuleName = packageDir + "arpha/arpha.arp";
	
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

	currentModule = modules.end();
}





void compile(const char* name,const char* source){
	auto mod = loadModule(name,source);
	mod->second.compile = true;
}

void onError(Location& location,std::string message){
	std::cout<< currentModule->first << '(' << location.line() << ':' << location.column << ')' <<": Error: " << message << std::endl;
}

}


int main()
{
	//the language definitions	
	compiler::init();

	compiler::importPackage("arpha");

	
	std::string source;
	char buf[1024];
	while(true){
		
		std::cin.getline(buf,1024);
		if(buf[0]=='\0') break;
		source+=buf;
		source+="\n";
	}
	compiler::compile("source",source.c_str());
			
	return 0;
}
