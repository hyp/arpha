#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "types.h"

#define INTRINSIC_INTTYPE(x) x = new TypeExpression(ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix(#x)) ))
#define INTRINSIC_FUNC(x)  \
	auto x = ensure( ensure(moduleScope->lookupPrefix(#x))->asOverloadset() )->functions[0]; \
	x->intrinsicEvaluator = &::x;

namespace intrinsics {
	namespace types {
		TypeExpression *Void,*Type;
		IntrinsicType  *StringLiteral;


		TypeExpression* boolean = nullptr;
		TypeExpression* int8 = nullptr;
		TypeExpression* int16 = nullptr;
		TypeExpression* int32 = nullptr;
		TypeExpression* int64 = nullptr;
		TypeExpression* uint8 = nullptr;
		TypeExpression* uint16 = nullptr;
		TypeExpression* uint32 = nullptr;
		TypeExpression* uint64 = nullptr;

		void startup() {
			Void = new TypeExpression(TypeExpression::VOID);
			Type = new TypeExpression(TypeExpression::TYPE);
			StringLiteral = new IntrinsicType("StringLiteral",Location());

			boolean = new TypeExpression(TypeExpression::BOOL);
		};
		void init(Scope* moduleScope){
			
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
