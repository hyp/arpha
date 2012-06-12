#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "../compiler.h"
#include "types.h"
#include "ast.h"


namespace intrinsics {
	namespace types {
		TypeExpression *Void,*Type;
		IntrinsicType  *StringLiteral;

		Function* PointerTypeGenerator,*FunctionTypeGenerator,*RangeTypeGenerator,*StaticArrayTypeGenerator;

		TypeExpression* boolean = nullptr;
		TypeExpression* int8 = nullptr;
		TypeExpression* int16 = nullptr;
		TypeExpression* int32 = nullptr;
		TypeExpression* int64 = nullptr;
		TypeExpression* uint8 = nullptr;
		TypeExpression* uint16 = nullptr;
		TypeExpression* uint32 = nullptr;
		TypeExpression* uint64 = nullptr;
		TypeExpression* natural;
		
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

			BigInt min;
			BigInt max;
#define DEF_INT_TYPE(n,t,s) \
			min = std::numeric_limits<t>::min(); \
			max = (::uint64)(std::numeric_limits<t>::max()); \
			n = new TypeExpression(IntegerType::make(min,max)); \
			n->integer->id = #n; n->integer->_size = s;

			DEF_INT_TYPE(int8,signed char,1)
			DEF_INT_TYPE(int16,signed short,2)
			DEF_INT_TYPE(int32,signed int,4)
			DEF_INT_TYPE(int64,::int64,8)

			DEF_INT_TYPE(uint8,unsigned char,1)
			DEF_INT_TYPE(uint16,unsigned short,2)
			min = 0;
			max = (::uint64)(std::numeric_limits<unsigned int>::max());
			uint32 = new TypeExpression(IntegerType::make(min,max));
			uint32->integer->id = "uint32";uint32->integer->_size = 4;
			DEF_INT_TYPE(uint64,::uint64,8)

			natural = uint32;

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
			//temporary
			moduleScope->define(new Substitute("int8",int8));
			moduleScope->define(new Substitute("int16",int16));
			moduleScope->define(new Substitute("int32",int32));
			moduleScope->define(new Substitute("int64",int64));

			moduleScope->define(new Substitute("uint8",uint8));
			moduleScope->define(new Substitute("uint16",uint16));
			moduleScope->define(new Substitute("uint32",uint32));
			moduleScope->define(new Substitute("uint64",uint64));

			moduleScope->define(new Substitute("natural",natural));

			struct TypeFunc {
				TypeFunc(SymbolID name,Scope* moduleScope,Function** dest,Node* (*eval)(Node*),int args = 1){
					Function* func = new Function(name,Location(),new Scope(moduleScope));
					if(dest) *dest = func;
					func->body.scope->_functionOwner = func;
					if(args == 1){
						func->arguments.push_back(new Argument("type",Location(),func->body.scope));
						func->arguments[0]->specifyType(Type);
					}else{
						if(name == SymbolID("Integer")){//TODO
							func->arguments.push_back(new Argument("min",Location(),func->body.scope));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("max",Location(),func->body.scope));
							func->arguments[1]->specifyType(Type);
						} else if(name == "Array"){
							func->arguments.push_back(new Argument("T",Location(),func->body.scope));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("N",Location(),func->body.scope));
							func->arguments[1]->specifyType(natural);
						}
						else{
							func->arguments.push_back(new Argument("parameter",Location(),func->body.scope));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("return",Location(),func->body.scope));
							func->arguments[1]->specifyType(Type);
						}
					}
					func->constInterpreter = eval;
					func->setFlag(Function::TYPE_GENERATOR_FUNCTION | Function::PURE);
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
				static Node* StaticArray(Node* arg){
					auto t = arg->asTupleExpression();
					return new TypeExpression(t->children[0]->asTypeExpression(),(size_t)t->children[1]->asIntegerLiteral()->integer.u64);
				}

			};
			TypeFunc("Pointer",moduleScope,&PointerTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Range",moduleScope,&RangeTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Function",moduleScope,&FunctionTypeGenerator,&TypeFunc::FunctionType,2);
			TypeFunc("Array",moduleScope,&StaticArrayTypeGenerator,&TypeFunc::StaticArray,2);
		}

		//Perform additional typesystem bindings after arpha/types is loaded
		void init(Scope* moduleScope){
			auto f = ensure( ensure(moduleScope->lookupPrefix("equals"))->asOverloadset() )->functions[0];
			f->constInterpreter = equals;
			f->setFlag(Function::PURE);
			
		};
	}
}
