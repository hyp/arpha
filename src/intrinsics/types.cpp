#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "../compiler.h"
#include "types.h"
#include "ast.h"

#define INTRINSIC_INTTYPE(x) x = new TypeExpression(ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix(#x)) ))

namespace intrinsics {
	namespace types {
		TypeExpression *Void,*Type;
		IntrinsicType  *StringLiteral;

		Function* PointerTypeGenerator,*FunctionTypeGenerator,*RangeTypeGenerator;

		TypeExpression* boolean = nullptr;
		TypeExpression* int8 = nullptr;
		TypeExpression* int16 = nullptr;
		TypeExpression* int32 = nullptr;
		TypeExpression* int64 = nullptr;
		TypeExpression* uint8 = nullptr;
		TypeExpression* uint16 = nullptr;
		TypeExpression* uint32 = nullptr;
		TypeExpression* uint64 = nullptr;
		
		//Performs a comparison between 2 types
		Node* equals(Node* parameters){
			auto t = parameters->asTupleExpression();
			return new BoolExpression(t->children[0]->asTypeExpression()->isSame(t->children[1]->asTypeExpression()));
		}

		//Boots up arpha's type system.
		void startup() {
			Void = new TypeExpression(TypeExpression::VOID);
			Type = new TypeExpression(TypeExpression::TYPE);
			StringLiteral = new IntrinsicType("StringLiteral",Location());

			boolean = new TypeExpression(TypeExpression::BOOL);
		};

		//Define some types before arpha/types is loaded so that we can use them in the module already.
		void preinit(Scope* moduleScope){
			struct Substitute : PrefixDefinition {
				Substitute(SymbolID name,Node* expr) : PrefixDefinition(name,Location()),expression(expr) {}
				Node* parse(Parser* parser){
					DuplicationModifiers mods;
					return expression->duplicate(&mods);
				}
				Node* expression;
			};
			moduleScope->define(new Substitute("Nothing",Void));
			moduleScope->define(new Substitute("Type",Type));
			moduleScope->define(new Substitute("bool",boolean));
			moduleScope->define(new Substitute("true" ,new BoolExpression(true)));
			moduleScope->define(new Substitute("false",new BoolExpression(false)));

			struct TypeFunc {
				TypeFunc(SymbolID name,Scope* moduleScope,Function** dest,Node* (*eval)(Node*),int args = 1){
					Function* func = new Function(name,Location(),new Scope(moduleScope));
					*dest = func;
					func->body.scope->_functionOwner = func;
					if(args == 1){
						func->arguments.push_back(new Argument("type",Location(),func));
						func->arguments[0]->specifyType(Type);
					}else{
						func->arguments.push_back(new Argument("parameter",Location(),func));
						func->arguments[0]->specifyType(Type);
						func->arguments.push_back(new Argument("return",Location(),func));
						func->arguments[1]->specifyType(Type);
					}
					func->constInterpreter = eval;
					func->setFlag(Function::TYPE_GENERATOR_FUNCTION);
					func->_returnType.specify(Type);
					func->_resolved = func->_argsResolved = true;
					moduleScope->defineFunction(func);
				}

				static Node* Pointer(Node* arg){
					return new TypeExpression(TypeExpression::POINTER,arg->asTypeExpression());
				}
				static Node* FunctionType(Node* arg){
					auto t = arg->asTupleExpression();
					return new TypeExpression(t->children[0]->asTypeExpression(),t->children[1]->asTypeExpression());
				}
			};
			TypeFunc("Pointer",moduleScope,&PointerTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Range",moduleScope,&RangeTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Function",moduleScope,&FunctionTypeGenerator,&TypeFunc::FunctionType,2);
		}

		//Perform additional typesystem bindings after arpha/types is loaded
		void init(Scope* moduleScope){
			auto f = ensure( ensure(moduleScope->lookupPrefix("equals"))->asOverloadset() )->functions[0];
			f->constInterpreter = equals;
			f->setFlag(Function::PURE);
			
			INTRINSIC_INTTYPE(int8);
			INTRINSIC_INTTYPE(int16);
			INTRINSIC_INTTYPE(int32);
			INTRINSIC_INTTYPE(int64);
			INTRINSIC_INTTYPE(uint8);
			INTRINSIC_INTTYPE(uint16);
			INTRINSIC_INTTYPE(uint32);
			INTRINSIC_INTTYPE(uint64);
		};
	}
}
