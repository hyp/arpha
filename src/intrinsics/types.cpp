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

			NodePointer = ::Type::getPointerType(new ::Type(::Type::NODE,-1));


			int8 = ::Type::getIntegerType (8,true);
			int16 = ::Type::getIntegerType(16,true);
			int32 = ::Type::getIntegerType(32,true);
			int64 = ::Type::getIntegerType(64,true);

			uint8 = ::Type::getIntegerType (8,false);
			uint16 = ::Type::getIntegerType(16,false);
			uint32 = ::Type::getIntegerType(32,false);
			uint64 = ::Type::getIntegerType(64,false);

			natural = ::Type::getNaturalType();

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


			struct TypeRefCreator : IntrinsicPrefixMacro {
				TypeRefCreator(SymbolID name,::Type* t) : IntrinsicPrefixMacro(name),type(t) {}
				Node* parse(Parser* parser){
					return new TypeReference(type);
				}
				::Type* type;
			};

			moduleScope->define(new TypeRefCreator("natural",natural ) );
			moduleScope->define(new TypeRefCreator("uintptr",::Type::getUintptrType() ) );

			moduleScope->define(new TypeRefCreator("char8",::Type::getCharType(8)) );
			moduleScope->define(new TypeRefCreator("char16",::Type::getCharType(16)) );
			moduleScope->define(new TypeRefCreator("char32",::Type::getCharType(32)) );

			moduleScope->define(new TypeRefCreator("float",::Type::getFloatType(32)) );
			moduleScope->define(new TypeRefCreator("double",::Type::getFloatType(64)) );

		}

	}
}
