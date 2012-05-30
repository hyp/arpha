#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "types.h"

#define INTRINSIC_INTTYPE(x) x = (ensure( dynamic_cast<IntegerType*>(moduleScope->lookupPrefix(#x)) ))->reference()
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
			Void = (new IntrinsicType("Nothing",Location()))->reference();
			Type = (new IntrinsicType("Type",Location()))->reference();
			StringLiteral = new IntrinsicType("StringLiteral",Location());
		};
		void init(Scope* moduleScope){

			moduleScope->define(Void->intrinsic);
			moduleScope->define(Type->intrinsic);

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
