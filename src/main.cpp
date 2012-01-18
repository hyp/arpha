#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"
#include "interpreter.h"
#include "ast.h"
#include "compiler.h"
#include "arpha.h"

#ifdef  _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#undef max
#undef min
#endif

namespace testing {

	
	int iterations = 100;
}

std::string format(const char *s){
	std::ostringstream stm;
    while (*s) {
        if (*s == '%' && *(++s) != '%')
            throw std::runtime_error("invalid format string: missing arguments");
		stm << *s++;
    }
	return stm.str();
}

void printFunc(std::string message){
	std::cout<<message;
#ifdef  _WIN32
	OutputDebugString(message.c_str());
#endif
}

void debugPrint(std::string message){
	std::cout<<"Debug: "<<message<<std::endl;
#ifdef  _WIN32
	OutputDebugString(message.c_str());
#endif
}




namespace testing {
	Unittest::Unittest(const char* name, void (*func)()) {
		std::cout<<"Running unittest "<<name<<"... \n";
		func();
		std::cout<<"   success!\n";
	}
}

void _assert(const char* file, int line, const char* what) {
	std::cout<<"Assertion failed: file: "<<file<<" line: "<<line<<'('<<what<<")!"<<std::endl;
}







unittest(format) {
	assert(format("Hello world") == "Hello world");
	assert(format("A %d %s C %c B",12,"KIAI",'_') == "A 12 KIAI C _ B");
}





//a table of unique symbols for fast symbol comparison

SymbolTable symbols;
std::ostream& operator<< (std::ostream& stream,const SymbolID symbol){
	return stream<<symbol.ptr();
}

inline size_t hashString(const char* src, size_t length) {
	return length + (size_t) src[0];
}

SymbolTable::Symbol* SymbolTable::create(const char* src, size_t length) {
	if(length == 0) return 0;
	size_t hsh = hashString(src, length) % hashTableLength;
	//compare strings in current chain
	Symbol** current = &hashTable[hsh];
	while ((*current) != 0) {
		if ((*current)->length == length) {
			if (memcmp((*current)->ptr, src, length) == 0) {
				return *current;
			}
		}
		current = &((*current)->next);
	}
	//new entry
	(*current) = (Symbol*) malloc(sizeof (Symbol) + sizeof (char) *(length + 1));
	(*current)->next = 0;
	(*current)->length = length;
	memcpy((*current)->ptr, src, length);
	(*current)->ptr[length] = 0; //null terminate
	return *current;
}

SymbolTable::SymbolTable() {
	for (size_t i = 0; i < hashTableLength; i++) hashTable[i] = 0;
}

SymbolTable::~SymbolTable() {
	for (size_t i = 0; i < hashTableLength; i++) {
		Symbol* current = hashTable[i], *next;
		for (; current != 0; current = next) {
			next = current->next;
			free(current);
		}
		hashTable[i] = 0;
	}
}
SymbolID::SymbolID(const char* str){
	assert(str);
	symbol = symbols.create(str,strlen(str));
}

unittest(symbolTable){
	SymbolTable symbols;
	char test[5];
	int length,i;

	srand(time(0));

	for(int j=0;j<testing::iterations;j++){
		length = rand()%4+1;
		for(i=0;i<length;i++) test[i]=rand()%255+1;
		test[i] = '\0';

		auto a = symbols.create(test,length) , b = symbols.create(test,length);
		test[0]=~test[0];
		auto c  = symbols.create(test,length);
		assert(a == b);
		assert(a != c);
		assert(b != c);
		assert(strcmp(test,c->ptr)==0);
		SymbolID x = *((SymbolID*)&a),y = *((SymbolID*)&b),z = *((SymbolID*)&c);
		assert(x == y);
		assert(x != z);
		assert(y != z);
	}
}

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

Token::Token(){
	type = -1;	
	uinteger = 0;	
}

std::ostream& operator<< (std::ostream& stream,const Token& token){
	if(token.isSymbol()) stream<<token.symbol.ptr();
	else if(token.isUinteger()) stream<<token.uinteger;
	else if(token.isEndExpression()) stream<<"';'";
	else if(token.isEOF()) stream<<"EOF";
	else assert(false);
	return stream;
}

Lexer::Lexer(const char* source) : location(0,0) {
	ptr= source;
}

bool isDigit(char c){
	return c>='0' && c<='9';
}

bool isLetter(char c){
	return (c>='a' && c<='z') || (c>='A' && c<='Z') || c == '_';
}

Token Lexer::consume(){
	Token token;	
	while((*ptr) <= ' ' && (*ptr)!='\0' && (*ptr)!='\n') ptr++; //skip spaces
	
	if( *ptr == '\n' || *ptr ==';'){
		if(*ptr == '\n') location = Location(location.line()+1,0);
		else location.column++;
		token.type = Token::EndExpression;
		ptr++;			
	}
	else if(*ptr == '(' || *ptr==')' || *ptr == ',' || *ptr == '{' || *ptr == '}' || *ptr == ':'){
		token.symbol = SymbolID(ptr,1);
		ptr++;
		token.type = Token::Symbol;
	}
	else if( isDigit(*ptr)){
		token.uinteger=0;
		for(;isDigit(*ptr);ptr++)
			token.uinteger = token.uinteger*10 + int((*ptr) -	'0');
		token.type = Token::Uinteger;	
	}
	else if(*ptr=='\0') {
		token.type = Token::Eof;	
	}		
	else if( isLetter(*ptr) ){
		const char* start = ptr;
		for(;(*ptr) > ' ' && (isLetter(*ptr) || isDigit(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type = Token::Symbol;
	}else{
		const char* start = ptr;
		for(;(*ptr) > ' ' && (!isDigit(*ptr)) && (!isLetter(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.type = Token::Symbol;	
	}			
	return token;		
}

Token Lexer::peek(){
	auto ptr2 = ptr;
	auto t = consume();
	ptr = ptr2;
	return t;
}

unittest(lexer){
	Token token;
#define expectSymbol(which) token = lexer.consume();assert(token.isSymbol() && token.symbol==SymbolID(which))
#define expectUinteger(n) token = lexer.consume();assert(token.isUinteger() && token.uinteger == n)
#define expectEof() token = lexer.consume();assert(token.isEOF())
	
	auto lexer = Lexer("foo 2 =");
	assert(lexer.currentLocation().line() == 0);
	expectSymbol("foo");
	expectUinteger(2);
	expectSymbol("=");
	assert(lexer.currentLocation().line() == 0);
	expectEof();
	

	lexer = Lexer("a_b bar + 5 - 7");
	assert(lexer.currentLocation().line() == 0);
	expectSymbol("a_b");
	expectSymbol("bar");
	expectSymbol("+");
	expectUinteger(5);
	expectSymbol("-");
	expectUinteger(7);
	assert(lexer.currentLocation().line() == 0);
	expectEof();

	//clean up
	symbols.~SymbolTable();
#undef expectSymbol
#undef expectUinteger
#undef expectEof
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
	symbols.~SymbolTable();
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

	void* readFile(const char* filename){
		FILE* file = fopen(filename, "rb");
		assert(file);
		fseek(file, 0, SEEK_END);
		size_t size = (size_t) ftell(file);
		rewind(file);
		void* data = malloc(size+1);
		assert(data);
		fread(data, size, 1, file);
		((char*)data)[size]='\0';
		fclose(file);
		return data;
	}

	std::string packageDir;

	Scope* importPackage(const char* name){
		auto filename = packageDir + name + "/" + name + ".arp";
		auto src = readFile(filename.c_str());
		auto module = loadModule(filename.c_str(),(const char*)src);
		free(src);
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
