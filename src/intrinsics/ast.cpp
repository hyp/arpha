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
		return evaluator->mixinFunctionCall(call);
	}else{
		error(node->location,"Mixin expression expects to recieve an call to a function as an argument!");
	}
	return node;
}
Node* defineVariable(CallExpression* node,Evaluator* evaluator){
	auto tuple = node->arg->asTupleExpression();
	auto str = tuple->children[0]->asStringLiteral();
	auto var = new Variable(SymbolID(str->block.ptr(),str->block.length()),node->location,evaluator->currentScope()->functionOwner()!=nullptr);
	evaluator->currentScope()->define(var);
	//TODO type and isMutable
	return new VariableReference(var);
}

namespace intrinsics {
	Function* ast::mixin = nullptr;


	void ast::init(Scope* moduleScope){
		//TODO
		INTRINSIC_FUNC(mixin)
		INTRINSIC_FUNC(defineVariable)
	}
};
