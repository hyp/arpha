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
TypeExpression* ExprPtr,* TuplePtr,* UnitPtr, * UnresolvedPtr, *CallPtr;
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

struct CaptureExpression : PrefixDefinition {
	CaptureExpression(): PrefixDefinition("`",Location()) {}

	Node* parse(Parser* parser){
		auto res = new ValueExpression(parser->parse(),ExprPtr);
		parser->expect("`");
		return res;
	}
};

Node* createUnit(Node* args,DuplicationModifiers* mods){
	return new ValueExpression(new UnitExpression(),UnitPtr);
}
Node* createTuple(Node* args,DuplicationModifiers* mods){
	return new ValueExpression(new TupleExpression(),TuplePtr);
}
Node* createUnresolved(Node* args,DuplicationModifiers* mods){
	return new UnitExpression();
}
Node* createCall(Node* args,DuplicationModifiers* mods){
	return new UnitExpression();
}




namespace intrinsics {
	Function* ast::mixin = nullptr;


	void ast::init(Scope* moduleScope){

		moduleScope->define(new NewMacro);
		moduleScope->define(new CaptureExpression);

		auto e = new IntrinsicType("Expression",Location(),nullptr);
		ExprPtr = new TypeExpression((PointerType*)nullptr,e->reference());
		moduleScope->define(e);

#define ExprType(T) \
		{ \
		auto x = new IntrinsicType(#T,Location(),e); \
		T##Ptr = new TypeExpression((PointerType*)nullptr,x->reference()); \
		moduleScope->define(x); \
		auto f = new Function(#T,Location(),nullptr); \
		f->_returnType.infer(T##Ptr); \
		f->_resolved = true; \
		f->mixinEvaluator = create##T; \
		x->construct = f; \
		}

		ExprType(Tuple)
		ExprType(Unit)
		ExprType(Unresolved)
		ExprType(Call)
		//TODO
		INTRINSIC_FUNC(mixin)
		INTRINSIC_FUNC(defineVariable)

	}
};
