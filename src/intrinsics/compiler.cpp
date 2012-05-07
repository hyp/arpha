#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "compiler.h"
#include "types.h"

#define INTRINSIC_VAR(x) ensure(dynamic_cast<Variable*>(moduleScope->lookupPrefix(#x)))

namespace intrinsics {
	namespace compiler {

		void init(Scope* moduleScope){
			INTRINSIC_VAR(debug)->setImmutableValue(new IntegerLiteral(BigInt((uint64)1)));
			INTRINSIC_VAR(version)->setImmutableValue(new IntegerLiteral(BigInt((uint64)1)));
		}
	};
};
