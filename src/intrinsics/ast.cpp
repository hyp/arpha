#include "../base/base.h"
#include "../compiler.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../ast/evaluate.h"
#include "../syntax/parser.h"
#include "ast.h"
#include "types.h"


#define INTRINSIC_FUNC(x)  \
	auto x = ensure( ensure(moduleScope->lookupPrefix(#x))->asOverloadset() )->functions[0]; \
	x->intrinsicEvaluator = &::x;

Node* mixin(CallExpression* node,Evaluator* evaluator){
	if(auto call = node->arg->asValueExpression()){
		DuplicationModifiers mods;
		mods.location = node->location;
		return evaluator->mixin(&mods,reinterpret_cast<Node*>(call->data));
	}else{
		error(node->location,"Mixin expression expects to recieve a *Expression as an argument!");
	}
	return node;
}
#include "../ast/interpret.h"
Node* interpret(CallExpression* node,Evaluator* evaluator){
	if(auto call = node->arg->asCallExpression()){
		auto res = interpretFunctionCall(evaluator->interpreter(),call->object->asFunctionReference()->function,call->arg);
		if(!res){
			Node* position;
			const char* error;
			getFailureInfo(evaluator->interpreter(),&position,&error);
			error(node->location,"Can't interpret %s - Failed to interpret %s(%s) because of %s",node,position,position->location.line(),error?error:"");
		}
		else return res;
	}else{
		error(node->location,"Mixin expression expects to recieve a *Expression as an argument!");
	}
	return node;
}
TypeExpression* TuplePtr,* UnitPtr, * UnresolvedPtr, *CallPtr;
static int precedenceTuple = 20;


struct NewMacro : PrefixDefinition {
	NewMacro() : PrefixDefinition("new",Location()){}

	Node* parse(Parser* parser){
		auto loc = parser->currentLocation();
		auto type = parser->parse(110);//TODO fix precedence
		auto t = type->asTypeExpression();
		//TODO fallback
		if(t->intrinsic->construct){	
			Node* arg;
			if(parser->match("(")){
				arg = parser->parse();
				parser->expect(")");
			}else{
				arg = new UnitExpression;
			}
			//TODO typechcek
			auto x= new CallExpression(new FunctionReference(t->intrinsic->construct),arg);
			x->_resolved = true;
			return x;
		}else return new UnitExpression();
	}
};




namespace intrinsics {
	IntrinsicType* ast::ExprPtr = nullptr;
	Function* ast::mixin = nullptr;


	void ast::init(Scope* moduleScope){

		moduleScope->define(new NewMacro);

		auto e = new IntrinsicType("Expression",Location(),nullptr);
		ExprPtr = e;
		moduleScope->define(e);

#define ExprType(T) \
		{ \
		auto x = new IntrinsicType(#T,Location(),e); \
		T##Ptr = new TypeExpression((PointerType*)nullptr,x->reference()); \
		moduleScope->define(x); \
		auto f = new Function(#T,Location(),nullptr); \
		f->_returnType.infer(T##Ptr); \
		f->_resolved = true; \
		x->construct = f; \
		}

		ExprType(Tuple)
		ExprType(Unit)
		ExprType(Unresolved)
		ExprType(Call)
		//TODO
		INTRINSIC_FUNC(mixin)
		INTRINSIC_FUNC(interpret)
	}
};
