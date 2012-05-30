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
		return new TypeExpression((PointerType*)nullptr,t->reference());
	}
	Function* defineFunction(const char* name,ARG arguments[],size_t count,TypeExpression* returnType,Node* (*e)(Node*)){
		Function* func = new Function(name,Location(),nullptr);
		for(size_t i = 0; i < count; i++){
			auto a = new Argument(arguments[i].name,Location());
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
		func->setProperty(Function::INTERPRET_ONLY_INSIDE);
		return func;
	}
	Function* defineFunction(const char* name,ARG argument,TypeExpression* returnType,Node* (*e)(Node*)){
		return defineFunction(name,&argument,1,returnType,e);
	}
	Function* defineFunction(const char* name,TypeExpression* returnType,Node* (*e)(Node*)){
		return defineFunction(name,nullptr,0,returnType,e);
	}
	TypeExpression* defineType(const char* name){
		last = new TypeExpression((PointerType*)nullptr,(new IntrinsicType(name,Location(),parent ? parent->argument->intrinsic : nullptr))->reference());
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
		TypeExpression* WhilePtr = nullptr;

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
			//
			static Node* newUnit(Node* arg){
				return new ValueExpression(new UnitExpression,UnitPtr);
			}
			static Node* newWhile(Node* arg){
				auto t = arg->asTupleExpression();
				return new ValueExpression(new WhileExpression(passedExpr(t->children[0]),passedExpr(t->children[1])),WhilePtr);
			}
			void init() {

				ScopePtr = defineType("Scope");
				defineFunction("currentScope",ScopePtr,&currentScope);

				ExprPtr = defineType("Expression");
				parent = ExprPtr;
				UnitPtr = defineType("Unit");
				defineConstructor(newUnit);
				auto TuplePtr = defineType("Tuple");
				auto UnresolvedPtr = defineType("Unresolved");

				WhilePtr = defineType("While");
				ARG args[] = {{"condition",ExprPtr},{"body",ExprPtr}};
				defineConstructor(newWhile,args,2);
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
			static Node* parseFull(Node* arg){
				auto params = &arg->asTupleExpression()->children[0];
				auto parser = compiler::currentUnit()->parser;
				//ignoreNewline?
				if(!params[1]->asIntegerLiteral()->integer.isZero()){
					auto next = parser->peek();
					if(next.isLine()) parser->consume();
				}
				
				auto sticky = params[0]->asIntegerLiteral();
				return new ValueExpression(parser->parse((int)sticky->integer.u64),ExprPtr);
			}
			static Node* expectString(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				auto symbol = arg->asStringLiteral();
				parser->expect(SymbolID(symbol->block.ptr(),symbol->block.length()));
				return new UnitExpression();
			}
			static Node* matchString(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				auto symbol = arg->asStringLiteral();;
				auto res = parser->match(SymbolID(symbol->block.ptr(),symbol->block.length()));
				auto x = new IntegerLiteral(BigInt((int64)res));
				x->_type = intrinsics::types::boolean;
				return x;
			}
			static Node* consumeSymbol(Node* arg){
				auto parser = compiler::currentUnit()->parser;
				auto sym = parser->expectName();
				return new StringLiteral(sym);
			}

			void init(){
				defineFunction("symbol",intrinsics::types::StringLiteral->reference(),&consumeSymbol);
				//parsing
				defineFunction("parse",ExprPtr,&parse);
				ARG arg = {"precedence",(new IntegerType("int32",Location()))->reference()};
				defineFunction("parse",arg,ExprPtr,&parse2);
				//Full function in beacker scope for efficiency because it's not always used
				ARG args[] = {{"precedence",(new IntegerType("int32",Location()))->reference()},{"ignoreNewline",(new IntegerType("bool",Location()))->reference()}};
				auto old = scope;
				scope = new Scope(nullptr);
				auto f = defineFunction("parse",args,2,ExprPtr,&parseFull);
				f->arguments[0]->defaultValue(new IntegerLiteral(BigInt((int64)0)),false,false);
				f->arguments[1]->defaultValue(new IntegerLiteral(BigInt((int64)0)),false,false);
				old->import(scope,"%",false,true);
				scope = old;
				
				{
				ARG arg = {"symbol",intrinsics::types::StringLiteral->reference()};
				defineFunction("expect",arg,intrinsics::types::Void,&expectString);
				defineFunction("match",arg,(new IntegerType("bool",Location()))->reference(),&matchString);//TODO bool
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
