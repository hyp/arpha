/**
* This module performs varios optimizations and call inlining on the AST.
*/

#include "../compiler.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "../intrinsics/types.h"

struct Optimizer {
	Scope* currentScope;
	//For plain and generated functions respectively. A function is inlined if its inlining weight < threshold.
	uint16 inliningThreshold[2]; 
	data::stat::Statistics statistics;

	Node* inlineCall(Function* function,Node* parameters);
};

void optimizeModule(Node* node){
	Optimizer optimizer;
	optimizer.inliningThreshold[0] = 10;
	optimizer.inliningThreshold[1] = 20;

	optimizer.statistics.functionCalls = 0;
	optimizer.statistics.externFunctionCalls = 0;
	optimizer.statistics.optimizations.functionCallsInlined = 0;
	node->optimize(&optimizer);
}

#undef  OPTIMIZE
#define OPTIMIZE(node) if(auto newNode = (node)->optimize(optimizer)) (node) = newNode

Node* Node::optimize(Optimizer* optimizer){
	return nullptr;
}
Node* NodeList::optimize(Optimizer* optimizer){
	for(auto i = begin();i!=end();++i){
		OPTIMIZE(*i);
	}
	return nullptr;
}

/**
* Function call inlining

* Scenarios: 
	f(x,y) = x * y

	f(a(),b()) => 
	{
		var x = a(); var y = b()
		var ret
		{ ret = x * y }
		ret
	}

	f(1,y) => 
	{
		var ret
		{ ret = 1 * y }
		ret
	}

	x = f(1,y) =>
	{
		x = { 1 * y }
	}
*/

namespace {
static bool isSimple(Node* node){
	if(node->isFlagSet(Node::CONSTANT) || node->asVariableReference()) return true;
	return false;
}

}

Node* Optimizer::inlineCall(Function* function,Node* parameters){
#ifdef DATA_STAT_COLLECT_STATISTICS
	statistics.optimizations.functionCallsInlined++;
#endif

	BlockExpression* wrapper = new BlockExpression();
	wrapper->scope->parent   = currentScope;
	wrapper->setFlag(BlockExpression::RETURNS_LAST_EXPRESSION);
	wrapper->_location = parameters->location();
	wrapper->setFlag(Node::RESOLVED);

	DuplicationModifiers mods(wrapper->scope);

	bool oneReturn = false;

	if(function->arguments.size()){
		Node** parametersPointer;
		if(auto tuple = parameters->asTupleExpression()) parametersPointer = tuple->childrenPtr();
		else parametersPointer = &parameters;

		size_t j = 0;
		for(auto i = function->arguments.begin();i!=function->arguments.end();++i,++j){
			
			if((*i)->isFlagSet(Variable::IS_IMMUTABLE) && isSimple(parametersPointer[j])){
				mods.expandArgument((*i),parametersPointer[j]);
			}
			else {
				auto var = new Variable((*i)->label(),parametersPointer[j]->location());
				var->type.specify((*i)->type.type());
				var->setFlag(Node::RESOLVED);
				auto assigns = new AssignmentExpression(var,parametersPointer[j]);
				assigns->setFlag(Node::RESOLVED);
				wrapper->addChild(assigns);
				mods.duplicateDefinition(static_cast<Variable*>(*i),var);
			}
			
		}
	}
	if(!function->returns()->isVoid()){
		if(function->body.childrenPtr()[0]->asReturnExpression()) oneReturn = true;
		else {
			auto var = new Variable(SymbolID(),parameters->location());
			var->type.specify(function->returns());
			var->setFlag(Node::RESOLVED);
			wrapper->addChild(var);
			mods.returnValueRedirector = var;
		}
	}

	auto  block = &function->body;
	if(block->size()){
		wrapper->addChild(oneReturn? block->childrenPtr()[0]->asReturnExpression()->expression->duplicate(&mods) : block->duplicateMixin(&mods));
	}

	if(!oneReturn){
		auto ret = mods.returnValueRedirector? (Node*)new VariableReference(mods.returnValueRedirector) : new UnitExpression();
		ret->setFlag(Node::RESOLVED);
		wrapper->addChild(ret);
	}
	return wrapper;	
}


Node* CallExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(arg);

	if(auto fref = object->asFunctionReference()){
		auto function = fref->function;

#ifdef DATA_STAT_COLLECT_STATISTICS
		if(!function->isIntrinsicOperation()) optimizer->statistics.functionCalls++;
		if(function->isExternal())            optimizer->statistics.externFunctionCalls++;
#endif
		if(!function->intrinsicCTFEbinder && !function->isExternal() && !function->isIntrinsicOperation() && 
			function->inliningWeight < optimizer->inliningThreshold[function->generatedFunctionParent? 1 : 0] &&
			function->callingConvention() == data::ast::Function::ARPHA){
			return optimizer->inlineCall(function,arg);
		}
	}
	return nullptr;
}
Node* LogicalOperation::optimize(Optimizer* optimizer){
	OPTIMIZE(parameters[0]);
	OPTIMIZE(parameters[1]);
	return nullptr;
}
Node* FieldAccessExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(object);
	return nullptr;
}

// var x = { var _ Foo; construct(&_); _ } -> { var _; construct(&x) }
Node* optimizeInitialization(Optimizer* optimizer,Node* object,BlockExpression* block){
	Node* vref = object->asVariableReference();
	bool assigningToVariable = vref == nullptr;
	if(!vref) vref = new VariableReference(object->asVariable());
	auto construct = block->childrenPtr()[1]->asCallExpression();
	if(auto tuple = construct->arg->asTupleExpression()) (*tuple->begin())->asPointerOperation()->expression = vref;
	else construct->arg->asPointerOperation()->expression = vref;
	if(!assigningToVariable) return construct;
	block->childrenPtr()[0] = object;
	block->childrenPtr()[2] = new UnitExpression();
	block->flags &= (~BlockExpression::RETURNS_LAST_EXPRESSION);
	block->setFlag(BlockExpression::USES_PARENT_SCOPE);
	block->scope = optimizer->currentScope;
	return block;
}

bool checkOptimizeInitialization(IfExpression* ifchain){
	bool match = false;
	if(auto block= ifchain->consequence->asBlockExpression()){
		match = block->isFlagSet(BlockExpression::OPTIMIZATION_CONSTRUCTS_VARIABLE);
	}
	if(!match) return false;
	if(auto block= ifchain->alternative->asBlockExpression()){
		match = block->isFlagSet(BlockExpression::OPTIMIZATION_CONSTRUCTS_VARIABLE);
	}
	if(!match){
		if(auto ifexpr = ifchain->alternative->asIfExpression())
			return checkOptimizeInitialization(ifexpr);
		return false;
	}
	return true;
}

// var x  = if(cond) Foo(0) else Foo(1) -> { var x; if(cond) construct(&x,0) else construct(&x,1) }
Node* optimizeInitialization(Optimizer* optimizer,Node* object,IfExpression* ifchain){
	bool assigningToVariable = object->asVariable() != nullptr;

	if(auto block= ifchain->consequence->asBlockExpression()){
		ifchain->consequence =  optimizeInitialization(optimizer,object,block);
	}

	if(auto block= ifchain->alternative->asBlockExpression()){
		ifchain->alternative =  optimizeInitialization(optimizer,object,block);
	}
	else if(auto ifexpr = ifchain->alternative->asIfExpression()){
		optimizeInitialization(optimizer,object,ifchain);
	}
	return ifchain;
}


Node* AssignmentExpression::optimize(Optimizer* optimizer){
	
	if(isFlagSet(AssignmentExpression::INITIALIZATION_ASSIGNMENT)){

		// var x = { var _ Foo; construct(&_); _ } -> { var _; construct(&x) }
		if(auto block= value->asBlockExpression()){
			if(block->isFlagSet(BlockExpression::OPTIMIZATION_CONSTRUCTS_VARIABLE)){
				return optimizeInitialization(optimizer,object,block);
			}
		}
		else if(auto ifexpr = value->asIfExpression()){
			if(!ifexpr->returnType()->isVoid()){
				if(checkOptimizeInitialization(ifexpr)) return optimizeInitialization(optimizer,object,ifexpr);
			}
		}
	}
	OPTIMIZE(object);
	OPTIMIZE(value);
	return nullptr;
}
Node* ReturnExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(expression);
	return nullptr;
}
Node* PointerOperation::optimize(Optimizer* optimizer){
	OPTIMIZE(expression);
	return nullptr;
}
Node* IfExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(condition);
	OPTIMIZE(consequence);
	OPTIMIZE(alternative);
	return nullptr;
}
Node* LoopExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(body);
	return nullptr;
}
Node* CastExpression::optimize(Optimizer* optimizer){
	OPTIMIZE(object);
	return nullptr;
}
Node* ScopedCommand::optimize(Optimizer* optimizer){
	if(child) OPTIMIZE(child);
	return nullptr;
}
Node* BlockExpression::optimize(Optimizer* optimizer){
	auto oldScope = optimizer->currentScope;
	optimizer->currentScope = scope;
	for(auto i = begin();i!=end();++i){
		OPTIMIZE(*i);
	}
	optimizer->currentScope = oldScope;
	return nullptr;
}
Node* Function::optimize(Optimizer* optimizer){
	body.optimize(optimizer);
	return nullptr;
}

#undef OPTIMIZE
