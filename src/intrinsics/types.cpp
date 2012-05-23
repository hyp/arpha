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

Node* equals(CallExpression* node,Evaluator* evaluator){
	auto t = node->arg->asTupleExpression();
	auto e = new IntegerLiteral(BigInt((int64) t->children[0]->asTypeExpression()->isSame(t->children[1]->asTypeExpression()) ));
	e->_type = intrinsics::types::boolean;
	return e;
}

namespace intrinsics {
	namespace types {
		TypeExpression *Void;

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

			Type = (ensure( dynamic_cast<IntrinsicType*>(moduleScope->lookupPrefix("Type")) ))->reference();
			Expression = (ensure( dynamic_cast<IntrinsicType*>(moduleScope->lookupPrefix("Expression")) ))->reference();
			Void = (ensure( dynamic_cast<IntrinsicType*>(moduleScope->lookupPrefix("Nothing")) ))->reference();

			//INTRINSIC_FUNC(equals); //type equality
		};
	}
}
