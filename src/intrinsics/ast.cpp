#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "ast.h"
#include "types.h"

#define INTRINSIC_TYPE(x) ensure(dynamic_cast<Type*>(moduleScope->lookupPrefix(#x)))

#define INTRINSIC_FUNC(x,t) \
	Function* x = ensure(moduleScope->resolve(#x,t)); \
	x->intrinsicEvaluator = &::x;



namespace intrinsics {
	Function* ast::mixin = nullptr;


	void ast::init(Scope* moduleScope){
		//TODO
	}
};
