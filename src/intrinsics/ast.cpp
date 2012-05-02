#include "../base/base.h"
#include "../scope.h"
#include "../ast/declarations.h"
#include "../ast/node.h"
#include "ast.h"

#include "../compiler.h" //TODO rm later

template<typename T>
inline T* ensure(int line,const char* file,T* x){
	if(!x) _assert(file,line,"Expected a non-null result!");
	return x;
}

#define INTRINSIC_TYPE(x) ensure(__LINE__,__FILE__,dynamic_cast<Type*>(moduleScope->lookupPrefix(#x)))

#define INTRINSIC_FUNC(x,t) \
	Function* x = ensure(__LINE__,__FILE__,moduleScope->resolve(#x,t)); \
	x->intrinsicEvaluator = &::x;

static Node* returnType(CallExpression* node){
	auto t = TypeReference::create(node->arg->asExpressionReference()->expression->returnType());
	//delete node
	return t;
}
static Node* mixin(CallExpression* node){
	auto e = node->arg->asExpressionReference()->expression;
	//delete node
	return e;
}

namespace intrinsics {
	Type* ast::Expression = nullptr;


	void ast::init(Scope* moduleScope){
		Expression = INTRINSIC_TYPE(Expression);

		INTRINSIC_FUNC(returnType,Expression);
		INTRINSIC_FUNC(mixin,Expression);
	}
};
