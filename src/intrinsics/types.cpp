#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "types.h"

#define INTRINSIC_INTTYPE(x) x = (ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix(#x)) ))->reference()

namespace intrinsics {
	namespace types {
		TypeExpression *Void,*AnyType;

		TypeExpression *Type,*Expression;

		TypeExpression* boolean = nullptr;
		TypeExpression* int8 = nullptr;
		TypeExpression* int16 = nullptr;
		TypeExpression* int32 = nullptr;
		TypeExpression* int64 = nullptr;
		TypeExpression* uint8 = nullptr;
		TypeExpression* uint16 = nullptr;
		TypeExpression* uint32 = nullptr;
		TypeExpression* uint64 = nullptr;

		void preinit(){
			auto VoidType = new IntrinsicType("void",Location());
			auto AnyTypeType = new IntrinsicType("any(_)type",Location());
			Void = VoidType->reference();
			AnyType = AnyTypeType->reference();

			Type = (new IntrinsicType("Type",Location()))->reference();
			Expression = (new IntrinsicType("Expression",Location()))->reference();
		}
		void init(Scope* moduleScope){


			boolean = (ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix("bool")) ))->reference();
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
