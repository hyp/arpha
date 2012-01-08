#include "common.h"
#include "parser.h"



namespace testing {

	struct Unittest {
		Unittest(const char* name,void (*func)());
	};	
	
	int iterations = 100;
}

void errorFunc(Location& location,std::string message){
	std::cout<<"Error @"<<location.line()<<" : "<<message<<std::endl;
}

void printFunc(std::string message){
	std::cout<<message;
}

void debugPrint(std::string message){
	std::cout<<"Debug: "<<message<<std::endl;
}

namespace testing {
	Unittest::Unittest(const char* name, void (*func)()) {
		std::cout<<"Running unittest "<<name<<"... \n";
		func();
		std::cout<<"   success!\n";
	}
}

void _assert(const char* file, int line, const char* what) {
	std::cout<<"Assertion failed: line "<<line<<'('<<what<<")!"<<std::endl;
}

#define unittest(name) void __unittest__##name(); testing::Unittest __Utest__##name(#name,& __unittest__##name); void __unittest__##name()



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

Location::Location(int line){
	lineNumber = line;
}

unittest(location){
	auto loc = Location(2);
	assert(loc.line() == 2);
}

Token::Token(){
	isEndExpression=isNumber=isName=isEof=false;	
	uinteger = 0;	
}

std::ostream& operator<< (std::ostream& stream,const Token& token){
	if(token.isName) stream<<token.symbol.ptr();
	else if(token.isNumber) stream<<token.uinteger;
	else if(token.isEndExpression) stream<<"';'";
	else if(token.isEof) stream<<"EOF";
	else assert(false);
	return stream;
}

Lexer::Lexer(const char* source) : location(0) {
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
		token.isEndExpression = true;
		ptr++;			
	}
	else if(*ptr == '(' || *ptr==')' || *ptr == ',' || *ptr == '{' || *ptr == '}' || *ptr == ':'){
		token.symbol = SymbolID(ptr,1);
		ptr++;
		token.isName = true;
	}
	else if( isDigit(*ptr)){
		token.uinteger=0;
		for(;isDigit(*ptr);ptr++)
			token.uinteger = token.uinteger*10 + int((*ptr) -	'0');
		token.isNumber = true;		
	}
	else if(*ptr=='\0') {
		token.isEof = true;
	}		
	else if( isLetter(*ptr) ){
		const char* start = ptr;
		for(;(*ptr) > ' ' && (isLetter(*ptr) || isDigit(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.isName = true;
	}else{
		const char* start = ptr;
		for(;(*ptr) > ' ' && (!isDigit(*ptr)) && (!isLetter(*ptr));ptr++);
		token.symbol = SymbolID(start,ptr);
		token.isName = true;	
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

struct Expression;
struct Scope;
struct Definition;
struct OverloadSet;

struct Parser : Lexer {
	//Current parsing state
	Scope* currentScope;
	bool dontLookupNextSymbol;

	struct State {
		const char* ptr;
		bool dontLookupNextSymbol;
	};
	State getState(){
		State state;
		state.ptr = ptr;
		state.dontLookupNextSymbol = dontLookupNextSymbol;
		return state;
	}
	void restoreState(State& state){
		ptr = state.ptr;
		dontLookupNextSymbol = state.dontLookupNextSymbol;
	}

	
	Parser(const char* src,Scope* scope) : Lexer(src) { dontLookupNextSymbol = false;currentScope=scope; }



	size_t unresolvedExpressions,solvedExpressions;

	void expect(SymbolID token);
	bool match(SymbolID token);
	bool isEndExpressionNext();

	SymbolID expectName();
	
	/// Returns a raw expression, which may contain unresolved symbols
	Expression* parse(int stickiness = 0);

	Expression* parseBlock();

	/// Resolve symbols, find the matching function call overloads, constant fold
	Expression* evaluate(Expression*);
};





//Defines how to parse a name
struct Definition {
	SymbolID id;
	int stickiness;
	Scope* scope;
	int lineNumber;
	

	virtual Expression* prefixParse(Parser*,Token);
	virtual Expression* infixParse(Parser*,Token,Expression*);

	//Tries to resolve an unresolved expression
	//Returns the given expression if resolving failed
	virtual Expression* resolve(Expression* expr) { return expr; }

	virtual bool isOverloadSet(){ return false; }
	
	Definition(Scope* scp,SymbolID name){ scope=scp;id=name;stickiness = -1; }	
	Definition(Scope* scp,SymbolID name,int sticky){ scope=scp;id=name;stickiness = sticky; }

protected:
	OverloadSet* getSet();
};

struct Scope {

	std::map<SymbolID,Definition*>  definitions;
	Scope* parent;
	Definition* owner;

	Scope(Scope* parent);
	void define(Definition* definition);
	Definition* lookup(SymbolID name);
	Definition* contains(SymbolID name);
};

Scope::Scope(Scope* parent){
	this->parent = parent;
}
void Scope::define(Definition* definition){
	definitions[definition->id]=definition;
}
Definition* Scope::lookup(SymbolID name){
	auto var = definitions.find(name);			
	if (var != definitions.end()) return var->second;
	if(parent) return parent->lookup(name);
	return 0;	
}
Definition* Scope::contains(SymbolID name){
	auto var = definitions.find(name);			
	if (var != definitions.end()) return var->second;
	return 0;
}

struct Type: public Definition {
	OverloadSet* set;
	uint32 size;
	uint32 alignment;
	bool isTuple;

	struct Field {
		Type* type;
		SymbolID name;

		Field(Type* type,SymbolID name);
		bool isUnnamed();
	};
	std::vector<Field> fields;

	//map like interface for field queries
	Type::Field* operator[](const SymbolID fieldName);
	
	Type(Scope* scope,SymbolID name,size_t sz);

	void add(Field field);
	Expression* prefixParse(Parser*,Token);

	//implicit type cast
	bool canAssignFrom(Type* other);

	static std::vector<Type*> tuples;


	//can it be converted
	static Type* tuple(Type* a,Type* b);
	static Type* flattenedTuple(Type* a,Type* b);
	static Type* tuple(std::vector<Field>& fields);

	//not expanded
private:
	Type(SymbolID name,size_t sz);
};

Type::Field::Field(Type* type,SymbolID name){ this->type=type;this->name=name; }
bool Type::Field::isUnnamed(){
	return name.isNull();
}
void Type::add(Type::Field field){
	fields.push_back(field);
	size += field.type->size;
}
std::vector<Type*> Type::tuples;


Type* Type::tuple(Type* a,Type* b){
	std::vector<Field> fields;
	fields.push_back(Type::Field(a,SymbolID()));fields.push_back(Type::Field(b,SymbolID()));
	return tuple(fields);
}

Type* Type::flattenedTuple(Type* a,Type* b){
	std::vector<Field> fields;
	if(a->isTuple) fields.insert(fields.end(),a->fields.begin(),a->fields.end());
	else fields.push_back(Type::Field(a,SymbolID()));
	if(b->isTuple) fields.insert(fields.end(),b->fields.begin(),b->fields.end());
	else fields.push_back(Type::Field(b,SymbolID()));

	debug("Make tuple (%s,%s) : %d",a->id.ptr(),b->id.ptr(),fields.size());
	return tuple(fields);
}

Type* Type::tuple(std::vector<Field>& fields){
	assert(fields.size());
	if(fields.size()==1) return fields[0].type;
	//check if such tuple already exists
	for(std::vector<Type*>::iterator i=tuples.begin();i!=tuples.end();++i){
		if((*i)->fields.size() == fields.size()){
			bool exists = true;
			for(size_t j=0;j < fields.size();++j) if(((*i)->fields[j].type != fields[j].type ) || ((*i)->fields[j].name != fields[j].name)) exists=false;
			if(exists) return *i;
		}
	}
	char buffer1[1024] = "tuple(";
	char buffer2[1024];
	for(size_t i=0;i<fields.size();i++){
		sprintf(buffer2," %s",fields[i].type->id.ptr());
		strcat(buffer1,buffer2);
	}
	strcat(buffer1,")");
	Type* tuple=new Type(buffer1,0);
	tuple->isTuple = true;
	for(size_t i=0;i<fields.size();i++) tuple->add(fields[i]);
	tuples.push_back(tuple);
	return tuple;
}

struct Variable: public Definition {
	Type* type;
	Expression* substitute;

	Variable(Scope* scope,SymbolID name,Type* type);
	
	Expression* prefixParse(Parser*,Token);

	void bindToConstant(Expression* expr);
	Expression* bindedConstant();
};

void Variable::bindToConstant(Expression* expr){
	substitute = expr;
}
Expression* Variable::bindedConstant(){
	return substitute;
}


struct OverloadSet;

//a function's argument


struct Function: public Definition {
	OverloadSet* set;
	Type* argument;
	Type* returnType;
	Scope* bodyScope;
	Expression* body;
	Expression* constraint; //constraint for inferred functions

	struct Argument {
		Variable variable;			 // so that code inside the functions has access to arguments
		Function* typeConstraint;  // Arithmetic
		Definition* valueConstraint; // x <- int32 //can be type or function!
		
		Argument(const Variable& var,Function* typeConstraint,Definition* valueConstraint);
	};

	std::vector<Argument> arguments;

	Function(Scope* scope,SymbolID name,Type* argumentType,Type* retType,Scope* bodyScope,Expression* body);

	Function* infer(Type* type);
};

Function::Argument::Argument(const Variable& var,Function* typeConstraint,Definition* valueConstraint) : variable(var) {
	this->typeConstraint = typeConstraint;
	this->valueConstraint = valueConstraint;
}


struct OverloadSet: public Definition {
	OverloadSet* parent;
	Type* type;
	std::vector<Function*> functions;

	OverloadSet(Scope* scope,SymbolID name);
	Expression* prefixParse(Parser*,Token);
	Expression* infixParse(Parser*,Token,Expression*);
	bool isOverloadSet(){ return true; }

	//Tries to resolve an unresolved expression using the set symbol hierarchy
	//Returns Unresolved expression if resolving fails
	Expression* resolve(Expression*);

	Function* add(Function* func);
	Function* find(Expression* expr);
};

Type::Type(SymbolID name,size_t sz) : Definition(0,name) { 
	size=sz; 
	isTuple = true;
}

Type::Type(Scope* scope,SymbolID name,size_t sz) : Definition(scope,name) { 
	size=sz; 
	isTuple = false;
	set=getSet();
	set->type = this;
}

Type::Field* Type::operator[](const SymbolID fieldName){
	for(auto i = fields.begin();i!=fields.end();++i){
		if( (*i).name == fieldName ) return i._Ptr;
	}
	return 0;
}



//Behaves as ( ... )
struct Parentheses: public Definition {

	Parentheses(SymbolID open,SymbolID close);
	Expression* prefixParse(Parser*,Token);
private:
	SymbolID closer;
};

//Behaves as .
struct AccessOperator: public Definition {
	AccessOperator(SymbolID name,int stickiness);
	Expression* infixParse(Parser*,Token,Expression*);
};

//Behaves as ,
struct ListOperator: public Definition {
	ListOperator(SymbolID name,int stickiness);
	Expression* infixParse(Parser*,Token,Expression*);
};

//Behaves as []
struct IndexOperator: public Definition {
	SymbolID closingSymbol;

	IndexOperator(SymbolID start,SymbolID end,int stickiness);
	Expression* infixParse(Parser*,Token,Expression*);
};

//statement and prefix operator
struct IfMacro: public Definition {
	SymbolID elseSymbol;
	IfMacro(SymbolID ifSymbol,SymbolID elseSymbol,int stickiness);

	Expression* infixParse(Parser*,Token,Expression*);
};

struct DefStatement: public Definition {
	DefStatement(SymbolID name);
	Expression* prefixParse(Parser*,Token);
};

struct TypeStatement : Definition {
	TypeStatement();
	Expression* prefixParse(Parser* parser,Token);
};

struct VarStatement: public Definition {
	VarStatement(SymbolID name);
	Expression* prefixParse(Parser*,Token);
};

struct ReturnStatement: public Definition {
	ReturnStatement() : Definition(0,"return") {}
	Expression* prefixParse(Parser*,Token);
};



struct AssignmentOperator : public Definition {
	AssignmentOperator(SymbolID name,int stickiness);
	Expression* infixParse(Parser*,Token,Expression*);
};

//an expression
struct Expression {
	enum {
		Constant = 0, //1
		FunctionCall, //f()
		TypeRef,      //int32
		Tuple, //a,b
		Unresolved, //somethingNotDefinedYet //overloadableFunc(x,y) -- If children are present this is unresolved function call else it's an unresolved symbol
		VariableRef,//x
		Assignment, //x = 5
		Access,     //doNotCare.howToAccess -- unresolved access ok!
		FieldAccess, //instance.field
		Index,	 //(1,2)[0] -- resolved into index() -- needed to index tuples not by function?
		Match, //match (expr) to a to b ...
		Label, //a:
		Return, //return expr
		Block // { a ; b ; ... }
	};
	int flags;
	Type* returnType;
	union {
		uint64 uinteger;
		int64 integer;
		double real; 
		const char* string;
		Function* function;
		Type* type;
		Scope* scope;
		Variable* variable;
		Type::Field* field;
		Expression* expression;
	};
	SymbolID symbol;
	union {
		Type* constantType;
	};
	Expression* children;
	Expression* next;

	Expression(int flags,Type* type);
	bool isNothing();
	bool isConstant(){ return flags==Constant; }
	bool isTuple(){ return flags==Tuple; }

	bool isUnresolved();
	bool is(int type);

	bool sameAs(Expression* other);
	Expression* lastChild();

	//inferredType isn't the same as returnType for constants!
	Type* inferredType();
};

Expression* Expression::lastChild(){
	Expression* result = children;
	if(result){
		while(result->next) result = result->next;
	}
	return result;
}

void Parser::expect(SymbolID token){
	Token tok = consume();
	if(tok.isName==false || tok.symbol!=token) error(previousLocation(),"'%s' expected!",token);
}

bool Parser::match(SymbolID token){
	Token tok = peek();
	if(tok.isName==false || tok.symbol!=token) return false;
	consume();
	return true;
}

bool Parser::isEndExpressionNext(){
	Token tok = peek();
	if(!tok.isEndExpression) return false;
	return true;
}

SymbolID Parser::expectName(){
	Token tok = consume();
	if(tok.isName==false) error(previousLocation(),"A valid name is expected!");
	return tok.symbol;
}

Expression* Definition::prefixParse(Parser* parser,Token token){
	error(parser->previousLocation(),"Can't prefix parse %s!",token);
	return 0;
}

Expression* Definition::infixParse(Parser* parser,Token token,Expression*){
	error(parser->previousLocation(),"Can't prefix parse %s!",token);
	return 0;
}

IfMacro::IfMacro(SymbolID ifSymbol,SymbolID elseSymbol,int stickiness) : Definition(0,ifSymbol,stickiness) {
	this->elseSymbol = elseSymbol;
}





ListOperator::ListOperator(SymbolID name,int stickiness) : Definition(0,name,stickiness) {}

//Doesn't flatten tuples, only has 2 elements
Expression* Tuple(Expression* a,Expression* b);

//Flattens the tuple
Expression* flattenTuple(Expression* tuple);

Expression* ListOperator::infixParse(Parser* parser,Token,Expression* prev){
	return Tuple(prev,parser->parse(stickiness));
}

//TODO functions arguments vector!
namespace arpha {
	Scope* globalScope,*scope;
	Type* type;
	Type* expression;
	Type* Nothing,*Unresolved,*inferred;
	

	Function* line;
	Function* typeof;
	Function* _sizeof;
	Function* typeEquals;

	//core types & functions
	Type *constant;
	Type *int8,*uint8,*int16,*uint16,*int32,*uint32,*int64,*uint64;
	Type *float64,*float32;
	Type *boolean;
	enum {
		BOOL,CNST,I8,U8,I16,U16,I32,U32,I64,U64,F64,F32,TYPE_COUNT
	};
	Type* types[TYPE_COUNT];

	Function* createFunction(Scope* scope,const char* name,Type* returns,Type* arg0 = 0,const char *arg0n = 0,Type* arg1 = 0,const char *arg1n = 0){
		Type* argument = arpha::Nothing;
		if(arg0) argument = arg1 ? Type::tuple(arg0,arg1) : arg0;
		else if(arg1) argument = arg1;
		//if(argument->isTuple) argument->fields[0].name = arg0n;
		auto func = new Function(scope,name,argument,returns,0,0);
		if(arg0) func->arguments.push_back(Function::Argument(Variable(0,arg0n,arg0),0,0));
		if(arg1){ assert(arg0); func->arguments.push_back(Function::Argument(Variable(0,arg1n,arg1),0,0)); }
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
		add= ArithmeticFunction(scope,"+",2,30);
		sub= ArithmeticFunction(scope,"-",2,30);
		mul= ArithmeticFunction(scope,"*",2,35);
		divide= ArithmeticFunction(scope,"/",2,35);
		mod= ArithmeticFunction(scope,"%",2,35);
		plus= ArithmeticFunction(globalScope,"+",1);
		minus= ArithmeticFunction(globalScope,"-",1,-1,true);
		equals = ComparisonFunction(scope,"==",25);
	}

	inline bool isReal(Type* type){
		return type == float32 || type == float64;
	}
	inline bool isInteger(Type* type){
		return type==int32 || type==uint32 || type == int64|| type == uint64 || type==int16 || type==uint16 || type==int8 || type==uint8;
	}
	inline bool isSignedInteger(Type* type){
		return type==int32 || type == int64 ||  type==int16 ||  type==int8;
	}
	inline bool isAssignableFromConstant(Type* type){
		return type==int32 || type==uint32 || type == int64 || type == uint64 || type==int16 || type==uint16 || type==int8 || type==uint8 || type == float32 || type ==float64 || type==boolean;
	}

	void test();



	void init(){
		scope = new Scope(0);
		globalScope = new Scope(0);
		globalScope->parent = scope;

		globalScope->define(new Parentheses(SymbolID("("),SymbolID(")")));
		globalScope->define(new ListOperator(SymbolID(","),15));
		globalScope->define(new IfMacro(SymbolID("if"),SymbolID("else"),15));
		globalScope->define(new AccessOperator(".",110));
		globalScope->define(new IndexOperator("[","]",80));
		globalScope->define(new DefStatement("def"));
		globalScope->define(new TypeStatement);
		globalScope->define(new VarStatement("var"));
		globalScope->define(new AssignmentOperator("=",10));
		globalScope->define(new ReturnStatement);
		
		type = new Type(scope,SymbolID("Type"),0);
		expression = new Type(scope,SymbolID("expression"),0);
		Nothing = new Type(scope,SymbolID("Nothing"),0);
		Unresolved = new Type(scope,SymbolID("Unresolved"),0);
		inferred = new Type(scope,SymbolID("inferred"),0); //TOFIX & TODO use it as a wildcard '_' type
		constant = new Type(scope,SymbolID("constant"),0);

		int i = 0;
		types[i++] = boolean = new Type(scope,"bool",1);
		types[i++] = constant;
		types[i++] = int8 = new Type(scope,SymbolID("int8"),1);
		types[i++] = uint8 = new Type(scope,SymbolID("uint8"),1);
		types[i++] = int16 = new Type(scope,SymbolID("int16"),2);
		types[i++] = uint16 = new Type(scope,SymbolID("uint16"),2);
		types[i++] = int32 = new Type(scope,SymbolID("int32"),4);
		types[i++] = uint32 = new Type(scope,SymbolID("uint32"),4);
		types[i++] = int64 = new Type(scope,SymbolID("int64"),8);
		types[i++] = uint64 = new Type(scope,SymbolID("uint64"),8);
		types[i++] = float64 = new Type(scope,SymbolID("double"),8);
		types[i++] = float32 = new Type(scope,SymbolID("float"),4);
		
		Type* c = new Type(scope,SymbolID("struct"),0);
		c->add(Type::Field(int32,"field"));
		c->add(Type::Field(c,"bar"));
		scope->define(new Variable(scope,"foo",c));
		new Function(scope,"field",Nothing,Nothing,0,0);
		new Function(scope,"bar",Nothing,Nothing,0,0);

		auto v = new Variable(scope,"bar",float64);
		Expression* expr = new Expression(Expression::Constant,constant);
		expr->constantType = float64;
		expr->real = 3.14;
		v->bindToConstant( expr );
		scope->define(v);

		basicFunctions();

		//line = new Function(scope,SymbolID("line"),Nothing,float64,0,0);
		typeof = createFunction(scope,"typeof",type,expression,"expression");
		_sizeof = createFunction(scope,"sizeof",constant,expression,"expression"); 
		typeEquals = createFunction(scope,"==",boolean,type,"type",type,"another-type");

		test();
	}
};

void print(Expression* expr);

bool Expression::isNothing(){ return flags==Constant && returnType == arpha::Nothing; }

Expression* Tuple(Expression* a,Expression* b){
	if(!a || a->isNothing()) return b;
	if(!b || b->isNothing()) return a;
	Expression* tuple = new Expression(Expression::Tuple, a->returnType != arpha::Unresolved && b->returnType != arpha::Unresolved ? Type::tuple(a->returnType,b->returnType) : arpha::Unresolved);
	tuple->children = a;
	tuple->children->next = b;
	return tuple;
}



//TODO flatten tuple type unresolved!!
Expression* flattenTuple(Expression* tuple){
	assert(tuple && tuple->flags == Expression::Tuple);
	Expression* a = tuple->children,*b = tuple->children->next;
	std::vector<Expression*> expressions;
	if( a->isTuple() ){
		a = flattenTuple(a);
		for(Expression* child= a->children;child!=0;child = child->next) expressions.push_back(child);
	}else expressions.push_back(a);
	if( b->isTuple() ){
		b = flattenTuple(b);
		for(Expression* child= b->children;child!=0;child = child->next) expressions.push_back(child);
	}else expressions.push_back(b);
	//set children
	Expression* prev = 0;
	for(auto i = expressions.begin();i!=expressions.end();++i){
		if(prev) prev->next = *i;
		else tuple->children = *i;
		prev = *i;
	}
	tuple->returnType = Type::flattenedTuple(a->returnType,b->returnType);
	return tuple; 
}

OverloadSet* Definition::getSet(){
	Definition* def = scope->contains(id);
	OverloadSet* set;
	if(def){
		set=dynamic_cast<OverloadSet*>(def);
		if(!set)
			printf("Error: name already taken not for function!\n");
	}else{
		set = new OverloadSet(scope,id);
		scope->define(set);	
	}
	return set;
}

Function::Function(Scope* scope,SymbolID name,Type* argumentType,Type* retType,Scope* bodyScope,Expression* body) : Definition(scope,name) { 
	argument=argumentType;
	returnType =retType; 
	this->body = body;
	this->bodyScope = bodyScope;
	set=getSet();
	set->add(this);
}

Function* Function::infer(Type* type){
	//TODO argument infer type coz of constants!
	Function* dup = new Function(scope,id,type,returnType,bodyScope,body); //TODO expr tree dup and eval
	size_t i = 0;

	Scope* dupBodyScope = bodyScope;
	if(type->isTuple){
		for(auto arg = type->fields.begin();arg!=type->fields.end();++arg,++i){
			dup->arguments.push_back(Argument(Variable(dupBodyScope,"#",(*arg).type),0,0));
			debug(" Arg %d inferred as %s!",i,(*arg).type->id);
		}
	}else{
			dup->arguments.push_back(Argument(Variable(dupBodyScope,"#",type),0,0));
			debug(" Arg %d inferred as %s!",i,type->id);
	}

	return dup;
}

Expression::Expression(int flags,Type* type){
	this->flags=flags;
	this->returnType=type;
	children=next=0;
}

Expression* Constant(uint64 x){
	Expression* e = new Expression(Expression::Constant,arpha::constant);
	e->uinteger = x;
	e->constantType = arpha::uint64;
	return e;
}

Expression* Constant(int64 x){
	Expression* e = new Expression(Expression::Constant,arpha::constant);
	e->integer = x;
	e->constantType = arpha::int64;
	return e;
}

Expression* Constant(double x){
	Expression* e = new Expression(Expression::Constant,arpha::constant);
	e->real = x;
	e->constantType = arpha::float64;
	return e;
}

Expression* Constant(Type* type,uint64 value = 0){
	Expression* e = new Expression(Expression::Constant,type);
	e->uinteger = value;
	return e;
}

Expression* VariableReference(Variable* variable){
	Expression* e= new Expression(Expression::VariableRef,variable->type);
	e->variable = variable;
	return e;
}

Expression* Match(Expression* what,Expression* chain){
	Expression* e = new Expression(Expression::Match,arpha::Unresolved); // evaluate type later
	e->expression = what;
	e->children = chain;
	return e;
}

Expression* Access(Expression* a,Expression* b){
	Expression* e = new Expression(Expression::Access,arpha::Unresolved); // evaluate type later
	e->children = a;
	a->next = b;
	return e;
}

Expression* FieldAccess(Expression* expr,Type::Field* field){
	Expression* e = new Expression(Expression::FieldAccess,field->type);
	e->children = expr;
	e->field = field;
	return e;
}

Expression* Index(Scope* scope,Expression* a,Expression* b){
	Expression* e = new Expression(Expression::Index,arpha::Unresolved); // evaluate type later
	e->scope = scope;
	e->children = a;
	a->next = b;
	return e;
}

Expression* Return(Scope* scope,Expression* expr){
	assert(expr);
	Expression* e = new Expression(Expression::Return,arpha::Nothing); // evaluate type later
	e->scope = scope;
	e->children = expr;
	return e;
}

Expression* Label(SymbolID name,Expression* expr){
	assert(expr);
	Expression* e=new Expression(Expression::Label,expr->returnType);
	e->children = expr;
	e->symbol = name;
	return e;
}

Expression* Assignment(Expression* rhs,Expression* value){
	Expression* e = new Expression(Expression::Assignment,arpha::Unresolved); // evaluate type later
	e->children = rhs;
	rhs->next = value;
	return e;
}

IndexOperator::IndexOperator(SymbolID start,SymbolID end,int stickiness) : Definition(0,start,stickiness) {
	closingSymbol = end;
}

Expression* IndexOperator::infixParse(Parser* parser,Token token,Expression* expr){
	Expression* inner = parser->parse();
	parser->expect(closingSymbol);
	return Index(parser->currentScope,expr,inner);
}

AccessOperator::AccessOperator(SymbolID name,int stickiness) : Definition(0,name,stickiness){
}

Expression* AccessOperator::infixParse(Parser* parser,Token,Expression* expression){
	parser->dontLookupNextSymbol = true;//IMPORTANT
	return Access(expression,parser->parse(stickiness));
}

Expression* IfMacro::infixParse(Parser* parser,Token,Expression* a){
	Expression* condition = parser->parse();
	parser->expect(elseSymbol);
	Expression* b = parser->parse();
	return Match(condition,a);
}

Parentheses::Parentheses(SymbolID open,SymbolID close) : Definition(0,open) {
	closer= close;
}

Expression* Parentheses::prefixParse(Parser* parser,Token){
	if(!parser->match(closer)){
		Expression* e=parser->parse();
		parser->expect(closer);
		return e;
	}else return Constant(arpha::Nothing);
}


VarStatement::VarStatement(SymbolID name) : Definition(0,name) {}

Expression* VarStatement::prefixParse(Parser* parser,Token){
	Expression* tuple = 0;
	std::vector<Variable*> vars;
	do{
		SymbolID name = parser->expectName();
		Variable* var = new Variable(parser->currentScope,name,arpha::Unresolved);
		parser->currentScope->define(var);

		if(tuple) tuple = Tuple(tuple,VariableReference(var));
		else tuple = VariableReference(var);
		vars.push_back(var);
	}while(parser->match(","));

	auto st = parser->getState();
	auto t = parser->evaluate(parser->parse(20));
	if(t->flags == Expression::TypeRef){
		debug("the variables are of type %s",t->type->id);
		for(auto i=vars.begin();i!=vars.end();++i){
			(*i)->type = t->type;
		}
	}
	else parser->restoreState(st);

	return tuple;
}

Variable::Variable(Scope* scope,SymbolID name,Type* type) : Definition(scope,name) {
	this->type = type;
	substitute = 0;
}

Expression* TypeReference(Type* type){
	Expression* e = new Expression(Expression::TypeRef,arpha::type);
	e->type = type;
	return e;
}

Expression* Unresolved(Scope* scope,SymbolID symbol,Expression* children = 0){
	Expression* e = new Expression(Expression::Unresolved,arpha::Unresolved);
	e->scope = scope;
	e->symbol = symbol;
	e->children = children;
	return e;
}

Expression* Variable::prefixParse(Parser*,Token){
	return VariableReference(this);
}

AssignmentOperator::AssignmentOperator(SymbolID name,int stickiness) : Definition(0,name,stickiness) {
}

Expression* AssignmentOperator::infixParse(Parser* parser,Token,Expression* expression){
	return Assignment(expression,parser->parse(stickiness-1)); //-1 for right associativity!
}

struct Substitute: Definition {
	Expression* substitute;

	Substitute(Scope* scope,SymbolID name,Expression* expr) : Definition(scope,name,0) { substitute = expr; }
	Expression* prefixParse(Parser* parser,Token);
	Expression* resolve(Expression*){ return substitute; }
};

TypeStatement::TypeStatement() : Definition(0,"type") {}

Expression* TypeStatement::prefixParse(Parser* parser,Token){
	SymbolID name = parser->expectName();
	return TypeReference(new Type(parser->currentScope,name,0));
}

DefStatement::DefStatement(SymbolID name) : Definition(0,name) {}


//facilitates the parsing of int32 or Arithmetic
Expression* typeOrConstraint(Parser* parser){
	auto loc = parser->currentLocation();
	auto expr = parser->evaluate(parser->parse(1000));
	if(expr->flags != Expression::TypeRef){
		if(expr->flags!=Expression::Unresolved) error(loc,"Expected a valid type or a function describing type's constraint!");
		//TODO resolve the constraint function -> into OverloadSet yes! prob not as it has to expand in caller's scope
		assert(false);//constraints not implemented yet
	}
	return expr;
}

Expression* DefStatement::prefixParse(Parser* parser,Token){
	SymbolID name = parser->expectName();
	//Function
	if( parser->match("(") ){
		Type* argumentType = arpha::Nothing;
		Scope* bodyScope = new Scope(parser->currentScope);
		std::vector<Function::Argument> arguments;
		Variable* var;
		if(!parser->match(")")){
			while(1){
				SymbolID argName = parser->expectName();
				arguments.push_back(Function::Argument(Variable(bodyScope,argName,arpha::inferred),0,0));
				if(parser->match(")")) break;
				if(!parser->match(",")){
					if(parser->match("->")){
						
						auto constr = typeOrConstraint(parser);
						debug("Argument's type inferred as type with value constrainted to %s",constr->type->id);
						arguments.back().variable.type   = arpha::type;
						arguments.back().valueConstraint = constr->type;
					}else{
						Expression* type = parser->evaluate(parser->parse(1000));
						if(type->flags != Expression::TypeRef) printf("Error: a valid type expected!\n");
						arguments.back().variable.type = type->type;
					}
					if(parser->match(")")) break;
					parser->expect(",");
				}
			}
			std::vector<Type::Field> fields;
			for(auto i = arguments.begin();i!=arguments.end();++i){
				bodyScope->define(&((*i).variable));
				fields.push_back(Type::Field((*i).variable.type,(*i).variable.id));
			}
			argumentType = Type::tuple(fields);
		}
		Expression* body;
		
		Type* returnType = arpha::Unresolved;
		Scope* oldScope= parser->currentScope;
		parser->currentScope = bodyScope;
		if(parser->match("=")){
			body = Return(bodyScope,parser->parse());
		}else{
			body = parser->parseBlock();
		}
		parser->currentScope = oldScope;
		
		
		auto f = new Function(oldScope,name,argumentType,returnType,bodyScope,body);
		bodyScope->owner = f;
		f->arguments = arguments;
		debug("AST for function's body:");
		print(parser->evaluate(body));
		if(f->returnType == arpha::Unresolved){
			debug("Function's return type infered as Nothing because no return statement was used!");
			f->returnType = arpha::Nothing;
		}
	}
	//Or substitute
	else{
		parser->expect("=");
		parser->currentScope->define( new Substitute(parser->currentScope,name,parser->parse()) );
	}
	return Constant(arpha::Nothing);
}

Expression* FunctionCall(Function* func,Expression* arguments){
	Expression* e = new Expression(Expression::FunctionCall,func->returnType);
	e->function = func;
	e->children = arguments;
	return e;
}

Expression* ReturnStatement::prefixParse(Parser* parser,Token token){
	return Return(parser->currentScope,parser->isEndExpressionNext() ?  Constant(arpha::Nothing) : parser->parse());
}


Expression* Substitute::prefixParse(Parser* parser,Token){
	return Unresolved(scope,id);
}


OverloadSet::OverloadSet(Scope* scope,SymbolID name): Definition(scope,name) {
	type = 0;
	//build set hierarchy
	parent = 0;
	if(scope->parent){
		Definition* def= scope->parent->lookup(name);
		if(def && def->isOverloadSet()){
			parent = (OverloadSet*) def;
			stickiness = parent->stickiness;
		}
	}
}

Function* OverloadSet::add(Function* func){
	for(std::vector<Function*>::iterator i=functions.begin();i!=functions.end();++i){
		if((*i)->argument == func->argument){//TODO same type but diff name bug!!!
			printf("Error: overload alreday exists %s(%s)!\n",id.ptr(),func->argument->id.ptr());
			return 0;
		}
	}
	functions.push_back(func);
	return func;
}

//type inferal := best thing ever 
bool canInfer(Type* a,Type* other){
	if(!a->isTuple){
		if(!other->isTuple){
			if(a == arpha::inferred && other!=arpha::Unresolved && other!=arpha::inferred) return true;
		}
		return false;
	}
	if(a->fields.size() != other->fields.size()) return false;
	for(size_t i = 0;i<a->fields.size();i++){
		if(a->fields[i].type != arpha::inferred){
			if(!a->fields[i].type->canAssignFrom(other->fields[i].type)) return false;
		}else if(other->fields[i].type == arpha::Unresolved) return false;
	}
	return true;
}

Function* OverloadSet::find(Expression* expr){
	Type* argumentType = expr->returnType; 
	Function *implicitMatch = 0,*inferMatch = 0,*exprMatch = 0;//lastResort
	
	for(std::vector<Function*>::iterator i=functions.begin();i!=functions.end();++i){
		if((*i)->argument == argumentType && argumentType!=arpha::inferred){
			//QUICK HACKZ
			debug("%d",(*i)->arguments.size());
			if((*i)->arguments[0].valueConstraint && ((Type*) (*i)->arguments[0].valueConstraint) != expr->type){
				debug("Value constraint %s in action against %s!",(*i)->arguments[0].valueConstraint->id,expr->type->id);
			}
			else return *i;
		}
		else if( (*i)->argument->canAssignFrom(argumentType) ){
			debug("Imlicit function call function %s with args %s",id,argumentType->id);
			if(implicitMatch) printf("Error: multiple overloads possible!\n");
			implicitMatch = *i;
		}
		else if( canInfer((*i)->argument,argumentType) ){
			debug("Infering function %s with args %s",id,argumentType->id);
			if(inferMatch) printf("Error: multiple infer overloads possible!\n");
			inferMatch = *i;
		}
		else if((*i)->argument == arpha::expression) exprMatch = *i;
	}
	if(implicitMatch){
		if(inferMatch) printf("Error: both overload and infer possible!\n");
		return implicitMatch;
	}
	if(inferMatch){
		return inferMatch->infer(argumentType);
	}
	if(exprMatch) return exprMatch;
	return 0;
}

Expression* OverloadSet::resolve(Expression* expr){
	//TODO set hierarchy
	if(expr->children){
		Function* f=find(expr->children);
		if(f) return FunctionCall(f,expr->children);		
	}else{
		if(type) return TypeReference(type);
	}
	if(parent) return parent->resolve(expr);
	return expr;
}

Expression* OverloadSet::prefixParse(Parser* parser,Token token){
	if(parser->match("(")){
		if(parser->match(")")) return Unresolved(parser->currentScope,token.symbol,Constant(arpha::Nothing));
		Expression * expr = Unresolved(parser->currentScope,token.symbol,parser->parse());
		parser->expect(")");
		return expr;
	}
	return Unresolved(parser->currentScope,token.symbol);
}

Expression* OverloadSet::infixParse(Parser* parser,Token token,Expression* expr){
	return Unresolved(parser->currentScope,token.symbol,Tuple(expr,parser->parse(stickiness)));
}

Expression* Type::prefixParse(Parser* parser,Token){
	return TypeReference(this);
}

bool Type::canAssignFrom(Type* other){
	if(other->isTuple){
		//check if tuple has the same underlying structure
		if(fields.size() != other->fields.size()) return false;
		for(size_t i =0;i<fields.size();i++){
			if(!fields[i].type->canAssignFrom(other->fields[i].type)) return false;
		}
		return true;
	}else{
		if( ( other == arpha::constant ) && arpha::isAssignableFromConstant(this)){
			debug("%s can assign from constant!",this->id);
			return true;
		}
	}
	return false;
}

//
Type* Expression::inferredType(){
	if(flags == Constant){
		if(constantType == arpha::int64)
			return abs(integer) <= int64(std::numeric_limits<int>::max()) ? arpha::int32 : arpha::int64;
		else if(constantType == arpha::uint64){
			if(uinteger <= uint64(std::numeric_limits<int>::max())) return arpha::int32;
			else if(uinteger <= uint64(std::numeric_limits<uint32>::max())) return arpha::uint32;
			else if(uinteger <= uint64(std::numeric_limits<int64>::max())) return arpha::int64;
			else return arpha::uint64;		
		}
		else if(constantType == arpha::float64) return arpha::float64;
	}
	return returnType;
}

//real and int64/uint64 => real
//int64 and uint64 => int64
//x and x => x
//TODO checks
Type* match2Constants(Expression* a,Expression* b){
	if(a->constantType != b->constantType){
		Expression* notreal = 0;
		if(a->constantType == arpha::float64) notreal = b;
	    else if(b->constantType == arpha::float64) notreal = a;
		if(notreal){
				notreal->real = notreal->constantType == arpha::uint64 ? double(notreal->uinteger) : double(notreal->integer);
				notreal->constantType = arpha::float64;
		}else{
			Expression* notsigned = a->constantType == arpha::int64 ? b : a;
			notsigned->integer = int64(notsigned->uinteger);
			notsigned->constantType = arpha::int64;
		}
	}
	return a->constantType;
}

static bool isConstant(Expression* expr){
	if(expr->flags == Expression::Constant) return true;
	else if(expr->flags == Expression::Tuple){
		for(auto child = expr->children;child!=0;child=child->next){
			if(child->flags != Expression::Constant) return false;
		}
		return true;
	}
	return false;
}

static Expression* evalFunctionCall(Expression* expr,Function* function,Expression* arguments){
	if(arguments && isConstant(arguments)){
		debug(" ctfe %s %s \n",function->set->id,arguments->returnType->id);
		if(function->set == arpha::minus){
			if(arguments->constantType == arpha::uint64){
				arguments->constantType = arpha::int64;
				//TODO overflow checks
				arguments->integer = - int64(arguments->uinteger);
			}
			else if(arguments->constantType == arpha::int64) arguments->integer= - arguments->integer;
			else arguments->real = - arguments->real;
			return arguments;
		}
		else if(function->set == arpha::plus) return arguments;
#define Func(name,op) \
		else if(function->set == arpha::##name){ \
			Type* t = match2Constants(arguments->children,arguments->children->next); \
			if(t == arpha::float64) return Constant(double(arguments->children->real op arguments->children->next->real)); \
			else if(t == arpha::int64) return Constant(int64(arguments->children->integer op arguments->children->next->integer)); \
			else return Constant(uint64(arguments->children->uinteger op arguments->children->next->uinteger)); \
		}
		Func(add,+)
		Func(sub,-)
		Func(mul,*)
		Func(divide,/)
		else if(function->set == arpha::mod){ 
			Type* t = match2Constants(arguments->children,arguments->children->next);
			if(t == arpha::float64) return Constant(fmod(arguments->children->real, arguments->children->next->real)); 
			else if(t == arpha::int64) return Constant(int64(arguments->children->integer % arguments->children->next->integer)); 
			else return Constant(uint64(arguments->children->uinteger % arguments->children->next->uinteger)); 
		}
#undef Func
#define Func(name,op) \
		else if(function->set == arpha::##name){ \
			Type* t = match2Constants(arguments->children,arguments->children->next); \
			if(t == arpha::float64) return Constant(arpha::boolean,arguments->children->real op arguments->children->next->real); \
			else if(t == arpha::int64) return Constant(arpha::boolean,arguments->children->integer op arguments->children->next->integer); \
			else return Constant(arpha::boolean,arguments->children->uinteger op arguments->children->next->uinteger); \
		}
		Func(equals,==)
#undef Func
	}
	//non constant
	if(function == arpha::_sizeof){
		if(arguments->returnType!=arpha::type) return Constant(uint64(arguments->returnType->size));
		return Constant(uint64(arguments->type->size));
	}
	else if(function == arpha::line){
		return Constant(2.0);
	}
	else if(function == arpha::typeof) return TypeReference(arguments->inferredType());
	else if(function == arpha::typeEquals){
		return Constant(arpha::boolean,arguments->children->type == arguments->children->next->type);
	}
	return expr;
}

Expression* getTupleField(Expression* tuple,int fieldId){
	int i=0;
	for(Expression* child = tuple->children;child!=0;child=child->next,i++){
		if(i==fieldId) return child;
	}
	return 0;
}

//Walk through ast tree 
void walk(Expression* ast,Expression* (*action)(Expression*)){
	Expression* prev = 0,*next;
	for(Expression* child = ast->children;child!=0;child=child->next){
		next = child->next;
		child = action(child);
		child->next = next;
		if(prev) prev->next = child;
		else ast->children = child;
		prev = child;
	}
	ast = action(ast);
}

//Duplicates the entire expression tree
Expression* duplicate(Expression* expr){
	Expression* dup = new Expression(*expr);

	Expression** cur = &dup->children;
	for(Expression* child = expr->children;child!=0;child=child->next){
		*cur = duplicate(child);
		cur = &((*cur)->next);
	}
	*cur = 0;

	return dup;
}

bool Expression::isUnresolved(){
	if(returnType == arpha::Unresolved) return true;
	if(flags == Expression::Tuple){
		for(auto i=children;i!=0;i=i->next){ if(i->isUnresolved()) return true; }
	}
	return false;
}
bool Expression::is(int type){
	if(flags == type) return true;
	else if(flags == Expression::Tuple){
		for(auto i=children;i!=0;i=i->next){ if(!i->is(type)) return false; }
		return true;
	}
	return false;
}

//The magic happens here!
Expression* Parser::evaluate(Expression* expr){
	const Expression* initialExpr = expr;
	const Type* initialType = expr->returnType;

	Expression* prev=0,*next;
	Definition* def;

	switch(expr->flags){
		case Expression::Block:
			for(Expression* child = expr->children;child!=0;child=child->next){
				next = child->next;
				child = evaluate(child);
				child->next = next;
				if(prev) prev->next = child;
				else expr->children = child;
				prev = child;
			}
			break;

		case Expression::Tuple:
			for(Expression* child = expr->children;child!=0;child=child->next){
				next = child->next;
				child = evaluate(child);
				child->next = next;
				if(prev) prev->next = child;
				else expr->children = child;
				prev = child;
			}
			expr = flattenTuple(expr);
			break;

		case Expression::FunctionCall: 
			if(expr->children) expr->children = evaluate(expr->children);
			expr = evalFunctionCall(expr,expr->function,expr->children);
			break;

		case Expression::Label:
			assert(expr->children);
			expr->children = evaluate(expr->children);
			expr->returnType = expr->children->returnType;
			break;

		case Expression::Unresolved:
			if(expr->children) expr->children = evaluate(expr->children);
			def = expr->scope->lookup(expr->symbol);	
			if(def){
				assert(def->isOverloadSet());//TO
				expr = def->resolve(expr);
				//need check before evaluation to avoid infinite recursion when resolve fails
				if(expr->flags != Expression::Unresolved) expr = evaluate( expr ); 
			}
			break;

		case Expression::VariableRef:
			if( auto cnst = expr->variable->bindedConstant()){
				assert(isConstant(cnst));
				debug("A variable %s was replaced by a constant !",expr->variable->id);
				expr = cnst;
			}
			//possibly update expr type on inferral by parent expression
			else if(expr->returnType != expr->variable->type) expr->returnType = expr->variable->type;
			break;

		case Expression::Return:
			//Return only allowed in function body!
			if( auto func = dynamic_cast<Function*>(expr->scope->owner) ){ 
				assert(expr->children); //coz even when return; treat it as return ();
				expr->children = evaluate(expr->children);

				if(!expr->children->isUnresolved()){
					if(func->returnType == arpha::Unresolved){
						func->returnType = expr->children->returnType;
						debug("Inferred function(%s)'s return type as %s",func->id,expr->children->returnType->id);
					}else{
						//TODO type validation
					}
				}
			}
			else printf("Error: return statement is only valid inside a function's body");
			break;

		case Expression::Assignment:
			//Only evaluate & infer rhs when lhs was evaluated
			expr->children->next = evaluate(expr->children->next);
			if(expr->children->next->returnType != arpha::Unresolved){	
				//TODO var binded tuple assignment
				if(expr->children->flags == Expression::VariableRef && expr->children->variable->bindedConstant() ){
					debug("Assigment to a %s binded with constant",expr->children->variable->id);
					//TODO TYPE CHECKS ETC
					expr->children->variable->bindToConstant(expr->children->next);
					expr = expr->children->next;
				}
				//
				else {
					next = expr->children->next;
					expr->children = evaluate(expr->children);
					expr->children->next = next;
					//Assign to variable(s), possibly infer types
					if( expr->children->is(Expression::VariableRef) ){
						if(expr->children->isTuple()){//Assign to tuples
							//TODO
							for(auto i = expr->children->children;i!=0;i=i->next){
								if(i->returnType == arpha::Unresolved){
									debug("Inferring a variable type(unrslvd) %s : %s",i->variable->id,expr->children->next->returnType->id);
									i->returnType = i->variable->type = expr->children->next->inferredType();
								}
							}
						}
						else if(expr->children->returnType == arpha::Unresolved){//or single var
							debug("Inferring a variable type(unrslvd) %s : %s",expr->children->variable->id,expr->children->next->returnType->id);
							expr->children->returnType = expr->children->variable->type = expr->children->next->inferredType();
						}
						expr->returnType = expr->children->returnType;
					}

					else if(expr->children->flags == Expression::FieldAccess){
						expr->returnType = expr->children->returnType;
					}
					else printf("Error: can only assign to variables or fields!\n");
				}
			}
			break;

		case Expression::Access:
			//resolve the left only when right was resolved
			next =  expr->children->next;
			expr->children = evaluate(expr->children);
			expr->children->next = next;
			if(expr->children->returnType != arpha::Unresolved){
				//TODO type.field vs type.func when both possible
				if(( expr->children->flags == Expression::VariableRef || expr->children->flags ==Expression::FieldAccess) && next->children == 0){
					debug("--. might be field");
					if(auto field = expr->children->returnType->operator[](next->symbol)){
						debug(" It is");
						expr = FieldAccess(expr->children,field);
						break;
					}
				}
				expr->children->next = 0;
				expr = Unresolved(next->scope,next->symbol,Tuple(expr->children,next->children));
			}
			break;
		case Expression::Index:
			next =  expr->children->next;
			expr->children = evaluate(expr->children);
			expr->children->next = next;
			if(expr->children->returnType != arpha::Unresolved){
				expr->children->next = evaluate(expr->children->next);
				expr->children->next->next = 0;
				//tuple[i] returns it's field #i
				if(expr->children->flags == Expression::Tuple){
					if(expr->children->next->returnType != arpha::Unresolved){
						expr = getTupleField(expr->children,expr->children->next->uinteger);
					}
				}
				//otherwise we use index function
				else {
					expr = Unresolved(expr->scope,SymbolID("[]"),Tuple(expr->children,expr->children->next));
				}
			}
			break;

		case Expression::Match:
			//TODO
			next =  expr->children->next;
			expr->children = evaluate(expr->children);
			expr->children->next = next;
			next = expr->children->next->next;
			expr->children->next = evaluate(expr->children->next);
			expr->children->next->next = next;
			expr->children->next->next = evaluate(expr->children->next->next);
			if(expr->children->returnType != arpha::Unresolved){
				if(expr->children->returnType != arpha::boolean) printf("Error: boolean expression expected!\n");
				expr->returnType = expr->children->next->returnType;
			}
			break;
	}

	//solving pass counters
	if(expr->returnType == arpha::Unresolved){
		if(expr != initialExpr) solvedExpressions++;
		unresolvedExpressions++;
	}
	else if(initialType == arpha::Unresolved) solvedExpressions++;
	
	return expr;
}

Expression* evalAll(Parser* p,Expression* e){
	int passes = 1;
	p->unresolvedExpressions = 0;
	size_t prevUnresolvedExpressions;
	while(1){
		prevUnresolvedExpressions  = p->unresolvedExpressions;
		p->unresolvedExpressions = 0;
		p->solvedExpressions = 0;

		printf("\nAST before pass %d\n>",passes);
		print(e);
		printf("\n>");
		print(duplicate(e));
		printf("\n");

		e = p->evaluate(e);
		
		printf("Evaluation pass %d: solved expressions(%d), unresolved expressions(%d)\n",passes,p->solvedExpressions,p->unresolvedExpressions);
		
		if(p->unresolvedExpressions == 0 || p->solvedExpressions == 0) break;
		passes++;
	}
	if(p->unresolvedExpressions > 0)
		printf("Error: not all expressions were resolved!\n");
	
	printf("\nFinal AST:\n>");
	print(e);
	printf("\n>");
	print(duplicate(e));
	printf("\n");
	return e;
}

void print(Expression* expr){
	printf(" (");
	switch(expr->flags){
		case Expression::Constant: 
			if(expr->returnType == arpha::constant){
				if(expr->constantType == arpha::uint64) printf("%l",expr->uinteger);
				else if(expr->constantType == arpha::int64) printf("%l",expr->integer);
				else printf("%f",expr->real);
			}
			else if(expr->returnType == arpha::Nothing) printf("nothing");
			else if(expr->returnType == arpha::boolean) printf("%s",expr->uinteger!=0 ? "true" : "false");
			else assert(false);
			break;
		case Expression::Tuple: 
			printf("tuple ");
			for(Expression* child = expr->children;child!=0;child=child->next){
				print(child);
				if(child->next!=0) printf(",");
			}
			break;
		case Expression::FunctionCall:
			printf("call %s ",expr->function->id.ptr());
			if(expr->children) print(expr->children);
			break;
		case Expression::Label:
			printf("%s : ",expr->symbol);
			print(expr->children);
			break;
		case Expression::TypeRef:
			printf("type-ref %s",expr->type->id.ptr()); break;
		case Expression::VariableRef:
			printf("var-ref %s",expr->variable->id.ptr()); break;
		case Expression::Return:
			printf("return "); print(expr->children); break;
		case Expression::Unresolved:
			printf("unresolved %s",expr->symbol);
			if(expr->children) print(expr->children);
			break;
		case Expression::FieldAccess:
			print(expr->children);
			printf(" . %s",expr->field->name);
			break;
		case Expression::Access:
			printf("access(");print(expr->children);printf(" ");print(expr->children->next);printf(")");
			break;
		case Expression::Index:
			printf("index[](");print(expr->children);printf(" ");print(expr->children->next);printf(")");
			break;
		case Expression::Assignment:
			print(expr->children);printf(" = ");print(expr->children->next);
			break;
		case Expression::Match:
			printf("conditional(if ");
			print(expr->children);
			printf(" then ");
			print(expr->children->next);
			printf(" else ");
			print(expr->children->next->next);
			printf(")");
			break;
		case Expression::Block:
			printf("{");
			for(auto i = expr->children;i!=0;i=i->next){ print(i);printf(";\n  "); }
			printf("}");
			break;
	}
	printf("):%s ",expr->returnType->id.ptr());
}


Expression* Parser::parseBlock(){
	expect("{");
	Expression* expr;
	Expression* block = new Expression(Expression::Block,arpha::Nothing);
	Expression* lastChild = 0;
	while(1){
		Token token = peek();
		if(token.isName && token.symbol == SymbolID("}")){
			consume();
			break;
		}
		if(token.isEof){
			error(previousLocation(),"EOF reached without the matching '}'");
			break;
		}
		if(token.isEndExpression){
			consume();
			continue;
		}

		expr = parse();
		if(lastChild) lastChild->next = expr;
		else block->children = expr;
		lastChild = expr;

		token = consume();
		if(token.isName && token.symbol == SymbolID("}")) break;
		if(token.isEof){
			error(previousLocation(),"EOF reached without the matching '}'");
			break;
		}
		if(!token.isEndExpression) error(previousLocation(),"';' expected!");
	}
	return block;
}

Expression* Parser::parse(int stickiness){
	
	Expression* expression;
	Token token = consume();
	//prefix
	if(token.isNumber) expression = Constant(token.uinteger);
	else if(token.isName){
		auto next = peek();
		if(next.isName && next.symbol == SymbolID(":")){
			consume();
			debug("Named argument %s %s",token,next);
			expression = Label(token.symbol,parse(16));
		}else{
			if(dontLookupNextSymbol){
				expression = Unresolved(currentScope,token.symbol);
				dontLookupNextSymbol = false;
			}
			else{
				Definition* parselet = currentScope->lookup(token.symbol);
				if(!parselet) { error(previousLocation(),"Can't prefix parse %s!",token); return 0; }
				expression = parselet->prefixParse(this,token);
			}
		}
	}
	else {
		error(previousLocation(),"Can't parse %s!",token);
		return 0;
	}

	//infix parsing
	//Token token;
	while(1){
		token = peek();
		if(token.isName){
			Definition* parselet = currentScope->lookup(token.symbol);
			if(parselet && stickiness < parselet->stickiness) expression = parselet->infixParse(this,consume(),expression);						
			else break;
		}else break;	
	}	
	return expression;			
}

//interpretation

bool Expression::sameAs(Expression* other){
	if(flags != other->flags) return false;
	if(returnType != other->returnType) return false;
	if(flags == Constant && uinteger == other->uinteger) return true;
	else if(flags == TypeRef && type == other->type) return true;
	else if(flags == Tuple){
			
		for(Expression* child = children,*child2 = other->children;true;child=child->next,child2=child2->next){
			if(child == 0){
				if(child2 == 0) return true;
				else break;
			}
			if(child2 == 0) break;
			if(!child->sameAs(child2)) break;
		}
	}
	return false;
}


void arpha::test(){
	//Parser testing
	printf("Running language tests...\n");

	Scope* scope = new Scope(arpha::globalScope);

	auto f = [&scope](const char* str,Expression* expected,const char* file,int line){
		Parser parser(str,scope);

		Expression* expr = parser.parse();
		expr = parser.evaluate(expr);

		if(expected->sameAs(expr)){
			if(!expr->sameAs(expected)) assert(false); //also unittest same as
			return;
		}

		printf("  !!! Test '%s' failed: Expected ", str);
		print(expected);
		printf(" but got ");
		print(expr);
		printf(" instead!\n");
	};

#define ensure(src,expr) f(src,expr,__FILE__,__LINE__)

	//constants and tuples
	ensure("()",Constant(arpha::Nothing));
	char str[128];
	for(int i=0;i<1;i++){
		sprintf(str,"%d",i);
		ensure(str,Constant(::uint64(i)));
		sprintf(str,"%d,%d",i,i+1);
		ensure(str, Tuple(Constant(::uint64(i)),Constant(::uint64(i+1))) );
	}

	//core types
	for(int i=0;i<TYPE_COUNT;i++){
		ensure(types[i]->id.ptr(),TypeReference(types[i]));
		sprintf(str,"%s,%s",types[i]->id.ptr(),types[i]->id.ptr());
		//ensure(str,Tuple(TypeReference(types[i]),TypeReference(types[i])),TypeReference(Type::makeTuple(types[i],types[i])) );
	}

	//substitute
	scope->define( new Substitute(scope,"magicNumber",Constant(::uint64(0xDEADBEEF))));
	ensure("magicNumber",Constant(::uint64(0xDEADBEEF)));

	//.

#undef ensure



	printf("  done!\n");
}


int eval(const char* source){
	Expression* node = (Expression*)1;
	int result = 0;	
	Parser parser(source,arpha::globalScope);
	while(node!=0){
		node = parser.parse();
		Token token = parser.consume();
		
		if(!token.isEndExpression && token.isEof == false) error(parser.previousLocation(),"Error: ';' exprected!\n");
		if(node){
			node = evalAll(&parser,node);
		}
		if(token.isEof) break;
	}
	return result;							 				
}


int main()
{
	//the language definitions			
	arpha::init();
	
	std::string source;
	char buf[1024];
	while(true){
		
		std::cin.getline(buf,1024);
		if(buf[0]=='\0') break;
		source+=buf;
		source+="\n";
	}
	eval(source.c_str());
			
	return 0;
}
