#include "../compiler.h"
#include "../base/symbol.h"
#include "../base/bigint.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "visitor.h"
#include "../intrinsics/ast.h"
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
		isDeadCode = false;
		isPureFunction = true;
		cantCtfe = false;
		mustCtfe = owner && owner->isFlagSet(Function::MACRO_FUNCTION | Function::CONSTRAINT_FUNCTION);
		reportedCtfeHeadError = false;
		inliningWeight = 0;
		mappingOffset = 0;
	}

	void addInliningWeight(int w){
		inliningWeight+=w;
	}
	void report(Node* node){
		if(reportedCtfeHeadError ==  false){
			compiler::headError(functionOwner->location,
			format("The %s %s isn't evaluatable at compile time:",functionOwner->isFlagSet(Function::MACRO_FUNCTION) ? "macro" :"constraint",functionOwner->id));
			reportedCtfeHeadError = true;
		}
		const char* str = "";
		if(node->asVariableReference()) str = "Access to a global variable isn't allowed!";
		else if(node->asBlockExpression()) str = "The function has too many locals!";
		else str = "The call to this function can't be evaluated at compile time!";
		compiler::subError(node->location,str);
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

	Node* visit(VariableReference* node){
		if(node->variable->location.line() > node->location.line() && node->variable->location.column > node->location.column){
			error(node->location,"Variable usage before declaration!");
		}
		if(node->variable->functionOwner() != functionOwner){
			if(node->variable->functionOwner() != nullptr){
				//TODO closures
				error(node->location,"Closures aren't supported yet!");
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
		addInliningWeight(5);
		return node;
	}

	Node* visit(IfExpression* node){
		node->condition->accept(this);
		auto constantCondition = constantBooleanExpression(node->condition);

		auto _ = isDeadCode;
		isDeadCode = constantCondition == 0;
		node->consequence->accept(this);
		isDeadCode = constantCondition == 1;
		node->alternative->accept(this);
		isDeadCode = _;

		addInliningWeight(10);
		return node;
	}

	Node* visit(ReturnExpression* node){
		node->value->accept(this);
		if(!functionOwner){
			error(node->location,"Return statement can only be used inside a function's body!");
		}
		if(lastWhileExpression){
			lastWhileExpression->setFlagForAll(insideUnreachableCode() ? 0x8 : 0x4); //while hierarchy has return
		}
		returnFlags |= (insideUnreachableCode() ? 0x2 : 0x1);
		addInliningWeight(1);
		return node;
	}

	Node* visit(ControlFlowExpression* node){
		if(node->isBreak()){
			if(lastWhileExpression) lastWhileExpression->setFlag(insideUnreachableCode() ? 0x2 : 0x1); //while has break
			else error(node->location,"Break statement can only be used inside a while loop!");
		}
		else if(node->isContinue()){
			if(!lastWhileExpression) error(node->location,"Continue statement can only be used inside a while loop!");
		}
		else if(node->isFallthrough()){
			//TODO
		}
		addInliningWeight(1);
		return node;
	}

	Node* visit(CallExpression* node){
		node->object->accept(this);
		node->arg->accept(this);
		if(auto f = node->object->asFunctionReference()){
			if(f->function->isFlagSet(Function::CANT_CTFE)) markAsNotInterpretable(node);
			if(!f->function->isFlagSet(Function::PURE)) markImpure(node);
			addInliningWeight(f->function->constInterpreter ? 2 : 6);
		}
		else addInliningWeight(10000);
		return node;
	}

	Node* visit(WhileExpression* node){
		FlaggedNode self(&lastWhileExpression,node);
		node->condition->accept(this);
		
		auto constantCondition = constantBooleanExpression(node->condition);
		auto _ = isDeadCode;
		isDeadCode = constantCondition == 0;
		node->body->accept(this);
		isDeadCode = _;

		if(constantCondition == 1) {
			if(self.flags==0){
				error(node->location,"This is an infinite loop - please provide a return or break statement so that the loop will be stopped!");
				
			}else if((self.flags & (0x1 | 0x4)) == 0){
				error(node->location,"This is an infinite loop - The return or break statement(s) contained in this loop are unreachable!");
			}
		}
		else if(constantCondition == 0) {
			warning(node->location,"The body of this loop will never be executed!");
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
		//map local vars
		if(functionOwner){
			if(cantCtfe == false){
				for(auto i = node->scope->prefixDefinitions.begin();i != node->scope->prefixDefinitions.end();i++){
					if(auto v = dynamic_cast<Variable*>((*i).second)){
						if(mappingOffset >= std::numeric_limits<uint16>::max()){
							markAsNotInterpretable(node);
							break;
						}
						v->registerID = (uint16)mappingOffset;
						mappingOffset++;
					}
				}
			}
			addInliningWeight((int)node->scope->numberOfDefinitions()*5);
		}

		for(auto i = node->children.begin();i!=node->children.end();i++) (*i)->accept(this);
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
		owner->inliningWeight = (uint16)std::min((int)(visitor.inliningWeight + owner->arguments.size()*2),(int)std::numeric_limits<uint16>::max());
		if(visitor.returnFlags == 0){
			error(owner->location,"The function %s is expected to return a %s value, but has no return statements inside!",owner->id,owner->returnType());
			owner->setFlag(Function::CANT_CTFE);
		}
		else if(visitor.returnFlags & 0x2){
			error(owner->location,"The function %s only has return statement(s) which can't be reached!",owner->id);
			owner->setFlag(Function::CANT_CTFE);
		}
		debug("Function %s analysis finished - isPure: %s, cant ctfe:%s, iw:%s, regs:%s",owner->id,visitor.isPureFunction,visitor.cantCtfe,owner->inliningWeight,owner->ctfeRegisterCount);
	}
}
