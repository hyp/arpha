#include "../base/base.h"
#include "../scope.h"
#include "../ast/declarations.h"
#include "../ast/node.h"
#include "types.h"

#define INTRINSIC_INTTYPE(x) x = new TypeExpression(ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix(#x)) ))

namespace intrinsics {
	namespace types {
		TypeExpression *Void,*Unresolved,*AnyType,*Inferred;

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
			Void = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);
			Unresolved = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);
			AnyType = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);
			Inferred = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);

			Type = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);
			Expression = new TypeExpression(TypeExpression::INTRINSIC_TYPE,nullptr);
		}
		void init(Scope* moduleScope){


			boolean = new TypeExpression(ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix("bool")) ));
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
