#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "../intrinsics/types.h"

//Also map local variables to registers for interpreting
struct Analyzer : NodeVisitor {
	struct FlaggedNode {
	private:
		FlaggedNode** _container;
	public:
		FlaggedNode* prev;
		Node* node;
		uint32 flags;

		FlaggedNode(FlaggedNode** container,Node* n) : _container(container),prev(*container),node(n),flags(0) {
			*container = this;
		}
		~FlaggedNode() {
			*_container = prev;
		}
		void setFlag(uint32 flag){
			flags |= flag;
		}
		void setFlagForAll(uint32 flag){
			flags |= flag;
			for(auto i = prev;i != nullptr;i=i->prev) i->flags |= flag;
		}
	};
	Function* functionOwner;
	FlaggedNode* lastWhileExpression;
	IfExpression* lastIfExpression;
	bool isDeadCode;
	bool isPureFunction;
	bool cantCtfe;
	bool mustCtfe;
	bool reportedCtfeHeadError;
	int inliningWeight;
	//TODO return.. not all control path return
	uint32 returnFlags;
	size_t mappingOffset;

	Analyzer(Function* owner) : functionOwner(owner) {
		lastWhileExpression = nullptr;
		lastIfExpression = nullptr;
		isDeadCode = false;
		isPureFunction = true;
		cantCtfe = false;
		mustCtfe = owner && owner->isFlagSet(Function::MACRO_FUNCTION | Function::CONSTRAINT_FUNCTION);
		reportedCtfeHeadError = false;
		inliningWeight = 0;
		returnFlags = 0;
		mappingOffset = 0;
		if(owner){
			for(auto i = owner->arguments.begin();i!=owner->arguments.end();i++){
				//map arguments
				(*i)->accept(this);
			}
		}
	}

	void addInliningWeight(int w){
		inliningWeight+=w;
	}
	void report(Node* node){
		if(reportedCtfeHeadError ==  false){
			compiler::headError(functionOwner->location(),
			format("The %s %s isn't evaluatable at compile time:",functionOwner->isFlagSet(Function::MACRO_FUNCTION) ? "macro" :"constraint",functionOwner->label()));
			reportedCtfeHeadError = true;
		}
		const char* str = "";
		if(auto v = node->asVariableReference()){
			if(!v->variable->functionOwner()) str = "Access to a global variable isn't allowed!";
			else str = "Access to a variable captured from the outer function isn't allowed!";
		}
		else if(node->asVariable()) str = "The function has too many locals!";
		else if(node->asPointerOperation()) str = "Pointer operations aren't allowed!";
		else str = "The call to this function can't be evaluated at compile time!";
		compiler::subError(node->location(),str);
	}
	void markAsNotInterpretable(Node* node){
		cantCtfe = true;
		if(mustCtfe) report(node);
	}
	void markImpure(Node* node){
		isPureFunction = false;
	}
	inline bool insideUnreachableCode(){
		return isDeadCode;
	}
	bool isEmptyNode(Node* node){
		return node->asUnitExpression() != nullptr;
	}
	int constantBooleanExpression(Node* node){
		if(auto constantBool = node->asBoolExpression()){
			return constantBool->value ? 1 : 0;
		}
		return -1;
	}

#undef error	
#define error(loc,...) { compiler::onError(loc,format(__VA_ARGS__)); cantCtfe = true; }

	bool isLocal(Node* node){
		if(auto pop = node->asPointerOperation()){
			if(pop->isAddress()){
				auto expr = pop->expression;
				if(auto fa = expr->asFieldAccessExpression()) expr = fa->object;
				if(auto vref = expr->asVariableReference()){
					 if(vref->variable->isLocal()) return true;
				} else if(expr->asVariable()) return true; //&(var x)
			}
		}
		return false;
	}

	Node* visit(IntegerLiteral* node){
		addInliningWeight(1);
		return node;
	}

	Node* visit(Variable* node){
		//map local vars
		if(functionOwner){
			if(mappingOffset >= std::numeric_limits<uint16>::max()){
				markAsNotInterpretable(node);
			}
			node->ctfeRegisterID = (uint16)mappingOffset;
			mappingOffset++;
		}
		addInliningWeight(3);
		return node;
	}

	Node* visit(VariableReference* node){
		if(node->variable->location().line() > node->location().line() && node->variable->location().column > node->location().column){
			error(node,"Variable usage before declaration!");
		}
		if(node->variable->functionOwner() != functionOwner){
			if(node->variable->functionOwner() != nullptr){
				//TODO closures
				error(node,"Closures aren't supported yet!");
				markAsNotInterpretable(node);
			}
			else markAsNotInterpretable(node);
			markImpure(node);
			addInliningWeight(10);
		}
		else addInliningWeight(2);
		return node;
	}

	Node* visit(AssignmentExpression* node){
		node->object->accept(this);
		node->value->accept(this);
		if(isLocal(node->value)){
			error(node,"Can't assign %s to %s - because of local semantics!",node->value,node->object);
		}
		addInliningWeight(5);
		return node;
	}

	Node* visit(FieldAccessExpression* node){
		node->object->accept(this);
		addInliningWeight(2);
		return node;
	}

	Node* visit(PointerOperation* node){
		node->expression->accept(this);
		markAsNotInterpretable(node);
		addInliningWeight(2);
		return node;
	}

	Node* visit(IfExpression* node){
		node->condition->accept(this);
		auto constantCondition = constantBooleanExpression(node->condition);

		auto _ = isDeadCode;
		isDeadCode = constantCondition == 0;
		auto prevIfExpression = lastIfExpression;
		lastIfExpression = node;
		node->consequence->accept(this);
		lastIfExpression = prevIfExpression;
		isDeadCode = constantCondition == 1;
		node->alternative->accept(this);
		isDeadCode = _;

		addInliningWeight(10);
		return node;
	}

	Node* visit(ReturnExpression* node){
		node->expression->accept(this);
		if(isLocal(node->expression)){
			error(node,"Can't return expression %s - it has local semantics!",node->expression);
		}
		if(!functionOwner){
			error(node,"Return statement can only be used inside a function's body!");
		}
		if(lastWhileExpression){
			lastWhileExpression->setFlagForAll(insideUnreachableCode() ? 0x8 : 0x4); //while hierarchy has return
		}
		returnFlags |= (insideUnreachableCode() ? 0x2 : 0x1);
		addInliningWeight(2);
		return node;
	}

	static bool  isLastExpression(Node* instr,Node* owner){
		BlockExpression* block;
		while(block = owner->asBlockExpression()){
			owner = block->childrenPtr()[block->size()-1];
		}
		return instr == owner;
	}

	Node* visit(ControlFlowExpression* node){
		if(node->isBreak()){
			if(lastWhileExpression) lastWhileExpression->setFlag(insideUnreachableCode() ? 0x2 : 0x1); //while has break
			else error(node,"Break statement can only be used inside a while loop!");
		}
		else if(node->isContinue()){
			if(!lastWhileExpression) error(node,"Continue statement can only be used inside a while loop!");
		}
		else if(node->isFallthrough()){
			//TODO
			if(!lastIfExpression) error(node,"Fallthrough statement can only be used inside the 'consequence' block of an if-else statement!");
			//verify that this is the last instuction
			if(!isLastExpression(node,lastIfExpression->consequence)) error(node,"Fallthrough statement has to be the last statement executed in the 'consequence' block of an if-else statement!");
		}
		addInliningWeight(2);
		return node;
	}

	Node* visit(CallExpression* node){
		node->object->accept(this);
		node->arg->accept(this);
		if(auto f = node->object->asFunctionReference()){
			if(f->function->isFlagSet(Function::CANT_CTFE)) markAsNotInterpretable(node);
			if(!f->function->isFlagSet(Function::PURE)) markImpure(node);
			addInliningWeight(f->function->isIntrinsic() ? 2 : 6);
		}
		else addInliningWeight(10000);
		return node;
	}

	Node* visit(LogicalOperation* node){
		node->parameters[0]->accept(this);
		node->parameters[1]->accept(this);
		addInliningWeight(5);
		return node;
	}

	Node* visit(LoopExpression* node){
		FlaggedNode self(&lastWhileExpression,node);
		
		node->body->accept(this);

		if(self.flags==0){
			error(node,"This is an infinite loop - please provide a return or break statement so that the loop will be stopped!");	
		}else if((self.flags & (0x1 | 0x4)) == 0){
			error(node,"This is an infinite loop - The return or break statement(s) contained in this loop are unreachable!");
		}

		addInliningWeight(40);
		return node;
	}
	Node* visit(TupleExpression* node){
		for(auto i = node->children.begin();i!=node->children.end();i++) (*i)->accept(this);
		addInliningWeight(1);
		return node;
	}
	Node* visit(BlockExpression* node){
		for(auto i = node->children.begin();i!=node->children.end();i++){
			(*i)->accept(this);
			bool warn = (*i)->isConst() && !(*i)->isDefinitionNode() && !(*i)->asUnitExpression() ; //NB: definition nodes have const flag overriden
			warn = warn ||( !(*i)->returnType()->isVoid() /*NB: GIVE WARNING FOR CALLS AS WELL! TO DISABLE USE: && !(*i)->asCallExpression()*/ );
			if(warn){
				warning((*i)->location(),"Ignored expression: The value from this expression is not used for anything!");
			}
		}
		return node;
	}
	Node* visit(CastExpression* node){
		markAsNotInterpretable(node);
		addInliningWeight(2);
		return node;
	}

	Node* visit(ErrorExpression* node){
		markAsNotInterpretable(node);
		return node;
	}

#undef error
#define error(loc,...) compiler::onError(loc,format(__VA_ARGS__))


};

void analyze(Node* node,Function* owner){
	Analyzer visitor(owner);
	node->accept(&visitor);
	if(owner){
		if(visitor.isPureFunction) owner->setFlag(Function::PURE);
		if(visitor.cantCtfe) owner->setFlag(Function::CANT_CTFE);

		owner->ctfeRegisterCount = (uint16)visitor.mappingOffset;
		owner->inliningWeight = (uint16)std::min((int)(visitor.inliningWeight),(int)std::numeric_limits<uint16>::max());
		if(!owner->returns()->isValidTypeForReturn()){
			error(owner,"The type '%s' isn't allowed to be used as a function's return type",owner->returns());
		}
		if(owner->body.size() && !owner->returns()->isVoid()){
			if(visitor.returnFlags == 0 && !owner->returns()->isVoid()){
				error(owner,"The function %s is expected to return a value of type %s, but has no return statements inside!",owner->label(),owner->returns());
				owner->setFlag(Function::CANT_CTFE);
			}
			else if(visitor.returnFlags & 0x2){
				error(owner,"The function %s only has return statement(s) which can't be reached!",owner->label());
				owner->setFlag(Function::CANT_CTFE);
			}
		}
		debug("Function %s analysis finished - isPure: %s, cant ctfe:%s, iw:%s, regs:%s",owner->label(),visitor.isPureFunction,visitor.cantCtfe,owner->inliningWeight,owner->ctfeRegisterCount);
	}
}
