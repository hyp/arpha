#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/resolve.h"
#include "../ast/interpret.h"
#include "../compiler.h"
#include "../syntax/parser.h"
#include "types.h"


namespace intrinsics {
	namespace types {
		::Type *Void,*Type;
		::Type *StringLiteral;

		::Type *NodePointer;

		Function* PointerTypeGenerator,*FunctionTypeGenerator,*RangeTypeGenerator,*StaticArrayTypeGenerator;
		Function* BoundedPointerTypeGenerator,*BoundedConstantLengthPointerTypeGenerator;

		::Type* boolean = nullptr;
		::Type* int8 = nullptr;
		::Type* int16 = nullptr;
		::Type* int32 = nullptr;
		::Type* int64 = nullptr;
		::Type* uint8 = nullptr;
		::Type* uint16 = nullptr;
		::Type* uint32 = nullptr;
		::Type* uint64 = nullptr;
		::Type* natural;
		

		//Boots up arpha's type system.
		void startup() {
			Void = new ::Type(::Type::VOID);
			Type = new ::Type(::Type::TYPE);
			

			boolean = new ::Type(::Type::BOOL);

			NodePointer = new ::Type(::Type::POINTER,new ::Type(::Type::NODE,-1));

			BigInt min;
			BigInt max;
#define DEF_INT_TYPE(n,t,s) \
			min = std::numeric_limits<t>::min(); \
			max = (::uint64)(std::numeric_limits<t>::max()); \
			n = &IntegerType::make(min,max)->_type; \
			n->integer->id = #n; n->integer->_size = s;

			DEF_INT_TYPE(int8,signed char,1)
			DEF_INT_TYPE(int16,signed short,2)
			DEF_INT_TYPE(int32,signed int,4)
			DEF_INT_TYPE(int64,::int64,8)

			DEF_INT_TYPE(uint8,unsigned char,1)
			DEF_INT_TYPE(uint16,unsigned short,2)
			min = 0;
			max = (::uint64)(std::numeric_limits<unsigned int>::max());
			uint32 = &IntegerType::make(min,max)->_type;
			uint32->integer->id = "uint32";uint32->integer->_size = 4;
			DEF_INT_TYPE(uint64,::uint64,8)

			natural = uint32;

		};

		//Define some types before arpha/types is loaded so that we can use them in the module already.
		void preinit(Scope* moduleScope){
			struct Substitute : IntrinsicPrefixMacro {
				Substitute(SymbolID name,Node* expr) : IntrinsicPrefixMacro(name),expression(expr) {}
				Node* parse(Parser* parser){
					DuplicationModifiers mods(parser->currentScope());
					return expression->duplicate(&mods);
				}
				Node* expression;
			};
			moduleScope->define(new Substitute("Nothing",new TypeReference(Void) ));
			moduleScope->define(new Substitute("Type",new TypeReference(Type) ));
			moduleScope->define(new Substitute("bool",new TypeReference(boolean) ));
			moduleScope->define(new Substitute("true" ,new BoolExpression(true)));
			moduleScope->define(new Substitute("false",new BoolExpression(false)));
			//temporary
			moduleScope->define(new Substitute("int8",new TypeReference(int8) ));
			moduleScope->define(new Substitute("int16",new TypeReference(int16) ));
			moduleScope->define(new Substitute("int32",new TypeReference(int32) ));
			moduleScope->define(new Substitute("int64",new TypeReference(int64) ));

			moduleScope->define(new Substitute("uint8",new TypeReference(uint8) ));
			moduleScope->define(new Substitute("uint16",new TypeReference(uint16) ));
			moduleScope->define(new Substitute("uint32",new TypeReference(uint32) ));
			moduleScope->define(new Substitute("uint64",new TypeReference(uint64) ));

			moduleScope->define(new Substitute("natural",new TypeReference(natural) ));

			struct TypeRefCreator : IntrinsicPrefixMacro {
				TypeRefCreator(SymbolID name,::Type* t) : IntrinsicPrefixMacro(name),type(t) {}
				Node* parse(Parser* parser){
					return new TypeReference(type);
				}
				::Type* type;
			};

			moduleScope->define(new TypeRefCreator("char8",::Type::getCharType(8)) );
			moduleScope->define(new TypeRefCreator("char16",::Type::getCharType(16)) );
			moduleScope->define(new TypeRefCreator("char32",::Type::getCharType(32)) );

			moduleScope->define(new TypeRefCreator("float",::Type::getFloatType(32)) );
			moduleScope->define(new TypeRefCreator("double",::Type::getFloatType(64)) );

			struct TypeFunc {
				TypeFunc(SymbolID name,Scope* moduleScope,Function** dest,Function::CTFE_Binder binder,int args = 1){
					Function* func = new Function(name,Location());
					if(dest) *dest = func;
					if(args == 1){
						func->arguments.push_back(new Argument("type",Location(),func));
						func->arguments[0]->specifyType(Type);
					}else{
						if(name == SymbolID("Integer")){//TODO
							func->arguments.push_back(new Argument("min",Location(),func));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("max",Location(),func));
							func->arguments[1]->specifyType(Type);
						} else if(name == "Array" || name == "BoundedPointer"){
							func->arguments.push_back(new Argument("T",Location(),func));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("N",Location(),func));
							func->arguments[1]->specifyType(natural);
						}
						else{
							func->arguments.push_back(new Argument("parameter",Location(),func));
							func->arguments[0]->specifyType(Type);
							func->arguments.push_back(new Argument("return",Location(),func));
							func->arguments[1]->specifyType(Type);
						}
					}
					func->intrinsicCTFEbinder = binder;
					func->setFlag(Function::TYPE_GENERATOR_FUNCTION | Function::PURE | Function::IS_INTRINSIC);
					func->_returnType.specify(Type);
					func->setFlag(Node::RESOLVED);
					moduleScope->defineFunction(func);
				}

				static void Pointer(CTFEintrinsicInvocation* invocation){
					invocation->ret(new ::Type(Type::POINTER,invocation->getTypeParameter(0)));
				}
				static void FunctionType(CTFEintrinsicInvocation* invocation){
					invocation->ret(new ::Type(invocation->getTypeParameter(0),invocation->getTypeParameter(1)));
				}
				static void StaticArray(CTFEintrinsicInvocation* invocation){
					invocation->ret(new ::Type(::Type::STATIC_ARRAY,invocation->getTypeParameter(0),(size_t)invocation->getInt32Parameter(1)));
				}
				static void BoundedPointer(CTFEintrinsicInvocation* invocation){
					invocation->ret(new ::Type(::Type::POINTER_BOUNDED,invocation->getTypeParameter(0)));
				}
				static void BoundedPointerConstantLength(CTFEintrinsicInvocation* invocation){
					invocation->ret(new ::Type(::Type::POINTER_BOUNDED_CONSTANT,invocation->getTypeParameter(0),(size_t)invocation->getInt32Parameter(1)));
				}
			};
			TypeFunc("Pointer",moduleScope,&PointerTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Range",moduleScope,&RangeTypeGenerator,&TypeFunc::Pointer);
			TypeFunc("Function",moduleScope,&FunctionTypeGenerator,&TypeFunc::FunctionType,2);
			TypeFunc("Array",moduleScope,&StaticArrayTypeGenerator,&TypeFunc::StaticArray,2);
			TypeFunc("BoundedPointer",moduleScope,&BoundedPointerTypeGenerator,&TypeFunc::BoundedPointer,1);
			TypeFunc("BoundedPointer",moduleScope,&BoundedConstantLengthPointerTypeGenerator,&TypeFunc::BoundedPointerConstantLength,2);
		}

	}
}
