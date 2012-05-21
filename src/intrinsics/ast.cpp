#include "../base/base.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "../compiler.h"
#include "ast.h"
#include "types.h"


#define INTRINSIC_FUNC(x)  \
	auto x = ensure( ensure(moduleScope->lookupPrefix(#x))->asOverloadset() )->functions[0]; \
	x->intrinsicEvaluator = &::x;

Node* mixin(CallExpression* node,Evaluator* evaluator){
	if(auto call = node->arg->asCallExpression()){
		return evaluator->mixinFunction(call);
	}else{
		error(node->location,"Mixin expression expects to recieve an call to a function as an argument!");
	}
	return node;
}

namespace intrinsics {
	Function* ast::mixin = nullptr;


	void ast::init(Scope* moduleScope){
		//TODO
		INTRINSIC_FUNC(mixin)
	}
};
