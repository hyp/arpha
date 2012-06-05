#include "../base/base.h"
#include "../compiler.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "../syntax/parser.h"
#include "ast.h"
#include "types.h"

struct IntrinsicModule {

	struct ARG {
		const char* name;
		TypeExpression* type;
	};

	Scope* scope;
	TypeExpression* last;
	TypeExpression* parent;

	IntrinsicModule(){
		scope = new Scope(nullptr);
		last = nullptr;
		parent = nullptr;
	}
	virtual void init() =0 ;
	static TypeExpression* pointerType(IntrinsicType* t){
		return new TypeExpression(TypeExpression::POINTER,t->reference());
	}
	Function* defineFunction(const char* name,ARG arguments[],size_t count,TypeExpression* returnType,Node* (*e)(Node*)){
		Function* func = new Function(name,Location(),nullptr);
		for(size_t i = 0; i < count; i++){
			auto a = new Argument(arguments[i].name,Location(),func);
			a->type.infer(arguments[i].type);
			func->arguments.push_back(a);
		}
		if(returnType)
			func->_returnType.infer(returnType);
		else 
			func->_returnType.infer(intrinsics::types::Void);
		if(scope) scope->defineFunction(func);
		func->_resolved = true;
		func->_argsResolved = true;
		func->constInterpreter = e;
		func->setFlag(Function::INTERPRET_ONLY_INSIDE);
		return func;
	}
	Function* defineFunction(const char* name,ARG argument,TypeExpression* returnType,Node* (*e)(Node*)){
		return defineFunction(name,&argument,1,returnType,e);
	}
	Function* defineFunction(const char* name,TypeExpression* returnType,Node* (*e)(Node*)){
		return defineFunction(name,nullptr,0,returnType,e);
	}
	TypeExpression* defineType(const char* name){
		last = new TypeExpression(TypeExpression::POINTER,(new IntrinsicType(name,Location(),parent ? parent->argument->intrinsic : nullptr))->reference());
		scope->define(last->argument->intrinsic);
		return last;
	}
	Function* defineConstructor(Node* (*constructor)(Node*) ,ARG arguments[] = nullptr,size_t count = 0){
		return defineFunction((std::string("new")+last->argument->intrinsic->id.ptr()).c_str(),arguments,count,last,constructor);
	}
};

namespace intrinsics {
	namespace ast {
		TypeExpression* ScopePtr = nullptr;

		TypeExpression* ExprPtr = nullptr;
		TypeExpression* UnitPtr = nullptr;
		TypeExpression* LoopPtr = nullptr;
		TypeExpression* ReturnPtr = nullptr;
		TypeExpression* IfPtr = nullptr;
		TypeExpression* ControlFlowPtr = nullptr;
		TypeExpression* PointerOpPtr = nullptr;
		TypeExpression* TypeExprPtr = nullptr;
		TypeExpression* BoolPtr = nullptr;

		Node* passedExpr(Node* arg){
			if(auto v = arg->asValueExpression()){
				if(auto block = reinterpret_cast<Node*>(v->data)->asBlockExpression()){
					if(block->children.size() == 1 && block->scope->numberOfDefinitions() == 0) return block->children[0];
				}
				return reinterpret_cast<Node*>(v->data);
			}
			else return arg;
		}

		/**
		* Provides access to AST definitions which can be accessed from both macroes and definitons
		*/
		struct General : IntrinsicModule {
			static Node* currentScope(Node* arg){
				return new ValueExpression(compiler::currentUnit()->evaluator->currentScope(),ScopePtr);
			}
			static Node* isConst(Node* arg){
				bool result;
				if(auto v = arg->asValueExpression()){
					result = reinterpret_cast<Node*>(v->data)->isConst();
					if(auto block = reinterpret_cast<Node*>(v->data)->asBlockExpression()){
						if(block->children.size() == 1 && block->scope->numberOfDefinitions() == 0) result = block->children[0]->isConst();
					}
				}
				return new BoolExpression(result);
			}
			//
			static Node* newUnit(Node* arg){
				return new ValueExpression(new UnitExpression,UnitPtr);
			}
			static Node* newLoop(Node* arg){
				auto t = arg->asTupleExpression();
				return new ValueExpression(new LoopExpression(passedExpr(arg)),LoopPtr);
			}
			static Node* newReturn(Node* arg){
				return new ValueExpression(new ReturnExpression(passedExpr(arg)),ReturnPtr);
			}
			static Node* newIfElse(Node* arg){
				auto t = arg->asTupleExpression();
				return new ValueExpression(new IfExpression(passedExpr(t->children[0]),passedExpr(t->children[1]),passedExpr(t->children[2])),IfPtr);
			}
			static Node* newControlFlowContinue(Node* arg){
				return new ValueExpression(new ControlFlowExpression(ControlFlowExpression::CONTINUE),ControlFlowPtr);
			}
			static Node* newControlFlowBreak(Node* arg){
				return new ValueExpression(new ControlFlowExpression(ControlFlowExpression::BREAK),ControlFlowPtr);
			}
			static Node* newPointerOpAddress(Node* arg){
				auto t = arg->asTupleExpression();
				return new ValueExpression(new PointerOperation(passedExpr(t->children[0]),PointerOperation::ADDRESS),PointerOpPtr);
			}
			static Node* newPointerOpDeref(Node* arg){
				auto t = arg->asTupleExpression();
				return new ValueExpression(new PointerOperation(passedExpr(t->children[0]),PointerOperation::DEREFERENCE),PointerOpPtr);
			}
			void init() {

				ScopePtr = defineType("Scope");
				defineFunction("currentScope",ScopePtr,&currentScope);

				ExprPtr = defineType("Expression");
				{
					ARG arg = {"expression",ExprPtr};
					defineFunction("isConst",arg,intrinsics::types::boolean,&isConst);
				}
				parent = ExprPtr;
				UnitPtr = defineType("Unit");
				defineConstructor(newUnit);
				auto TuplePtr = defineType("Tuple");
				auto UnresolvedPtr = defineType("Unresolved");

				LoopPtr = defineType("Loop");
				ARG args[] = {{"body",ExprPtr}};
				defineConstructor(newLoop,args,1);
				{
				ReturnPtr = defineType("Return");
				ARG args[] = {{"value",ExprPtr}};
				defineConstructor(newReturn,args,1);
				}

			

				
				{
				IfPtr = defineType("IfElse");
				ARG args[] = {{"condition",ExprPtr},{"consequence",ExprPtr},{"alternative",ExprPtr}};
				defineConstructor(newIfElse,args,3);
				}

				{
				ControlFlowPtr = defineType("ControlFlow");
				ARG args[] = {{"continue",intrinsics::types::boolean}};
				defineConstructor(newControlFlowContinue,args,1);
				args[0].name = "break";
				defineConstructor(newControlFlowBreak,args,1);
				}

				{
				PointerOpPtr = defineType("PointerOp");
				ARG args[] = {{"expression",ExprPtr},{"addressof",intrinsics::types::boolean}};
				defineConstructor(newPointerOpAddress,args,2);
				ARG args2[] = {{"expression",ExprPtr},{"dereference",intrinsics::types::boolean}};
				defineConstructor(newPointerOpDeref,args2,2);
				}

			parent = nullptr;
			}
		};

		struct ParserModule : IntrinsicModule {

			static Node* parse(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				return new ValueExpression(parser->parse(),ExprPtr);
			}
			static Node* parse2(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				auto sticky = arg->asIntegerLiteral();
				return new ValueExpression(parser->parse((int)sticky->integer.u64),ExprPtr);
			}
			struct NewlineIgnorer {
				bool ignore;
				Parser::State state;
				Parser* parser;
				NewlineIgnorer(bool doIgnore,Parser* parser) : ignore(doIgnore) {				
					if(ignore){
						parser->saveState(&state);
						for(auto next = parser->peek();next.isLine();next = parser->peek()) parser->consume();
						this->parser = parser;
					}
				}
				void rollback(){
					if(ignore) parser->restoreState(&state);
				}
			};
			static Node* expectSymbol(Node* arg){
				auto t = arg->asTupleExpression();
				auto parser = compiler::currentUnit()->parser;
				auto symbol = t->children[0]->asStringLiteral();
				NewlineIgnorer i(t->children[1]->asBoolExpression()->value,parser);
				parser->expect(SymbolID(symbol->block.ptr(),symbol->block.length()));
				return new UnitExpression();
			}
			static Node* matchSymbol(Node* arg){
				auto t = arg->asTupleExpression();
				auto parser = compiler::currentUnit()->parser;
				auto symbol = t->children[0]->asStringLiteral();
				NewlineIgnorer i(t->children[1]->asBoolExpression()->value,parser);
				auto result = parser->match(SymbolID(symbol->block.ptr(),symbol->block.length()));
				if(!result) i.rollback();
				return new BoolExpression(result);
			}
			static Node* isNextSymbol(Node* arg){
				auto t = arg->asTupleExpression();
				auto parser = compiler::currentUnit()->parser;
				auto symbol = t->children[0]->asStringLiteral();
				NewlineIgnorer i(t->children[1]->asBoolExpression()->value,parser);
				auto next = parser->peek();
				auto result = next.isSymbol() && next.symbol == SymbolID(symbol->block.ptr(),symbol->block.length());
				i.rollback();
				return new BoolExpression(result);
			}
			static Node* consumeSymbol(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				auto sym = parser->expectName();
				return new StringLiteral(sym);
			}
			static Node* matchNewline(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				bool res = parser->peek().isLine();
				if(res) parser->consume();
				return new BoolExpression(res);
			}


			void init(){
				defineFunction("symbol",intrinsics::types::StringLiteral->reference(),&consumeSymbol);
				//parsing
				defineFunction("parse",ExprPtr,&parse);
				ARG arg = {"precedence",new TypeExpression(new IntegerType("int32",Location()))};
				defineFunction("parse",arg,ExprPtr,&parse2);
				
				{
				ARG args[] = {{"symbol",intrinsics::types::StringLiteral->reference()},{"ignoreNewlines",intrinsics::types::boolean}};
				auto f = defineFunction("expect",args,2,intrinsics::types::Void,&expectSymbol);
				f->arguments[1]->defaultValue(new BoolExpression(false),false,false);
				f = defineFunction("match",args,2,intrinsics::types::boolean,&matchSymbol);
				f->arguments[1]->defaultValue(new BoolExpression(false),false,false);
				f = defineFunction("isNext",args,2,intrinsics::types::boolean,&isNextSymbol);
				f->arguments[1]->defaultValue(new BoolExpression(false),false,false);
				}
				defineFunction("matchNewline",intrinsics::types::boolean,&matchNewline);
				{
					ARG args[] ={{"until",intrinsics::types::StringLiteral->reference()},
					{"separator",intrinsics::types::StringLiteral->reference()},
					{"handler",new TypeExpression(intrinsics::types::Void,intrinsics::types::Void)}};
					
					auto f = defineFunction("loop",args,3,intrinsics::types::Void,&loopFull);
					f->arguments[0]->defaultValue(new StringLiteral(SymbolID("}")),false,false);
					f->arguments[1]->defaultValue(new StringLiteral(SymbolID(";")),false,false);
				}

			}
		};

		General* general;
		ParserModule* parser;
		void startup(){
			general = new General;
			general->init();
			parser = new ParserModule;
			parser->init();
		}

		void onMacroScope(Scope* scope){
			scope->import(general->scope,"ast",true,false);
			scope->import(parser->scope,"parser",true,false);
		}
	}


	void ast::init(Scope* moduleScope){

		//TODO
	}
};
