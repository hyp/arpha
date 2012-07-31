#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Intrinsics.h"

#include "../../base/base.h"
#include "../../base/symbol.h"
#include "../../base/bigint.h"
#include "../../base/format.h"

#include "../../syntax/location.h"
#include "../../ast/scope.h"
#include "../../ast/node.h"
#include "../../ast/declarations.h"
#include "../../ast/visitor.h"

#include "gen.h"
#include "../mangler.h"

/**
* Globals
*/
enum {
	GEN_TYPE_I8,
	GEN_TYPE_I16,
	GEN_TYPE_I32,
	GEN_TYPE_I64,
	GEN_TYPE_SIZE_T,
	GEN_TYPE_VOID,
	GEN_TYPE_FLOAT,
	GEN_TYPE_DOUBLE,
	GEN_MAX_CORE_TYPES
};

namespace {
	llvm::Type* coreTypes[GEN_MAX_CORE_TYPES];
	llvm::TargetMachine* targetMachine;
};

/**
* LLVM IR construction.
*/
struct LLVMgenerator: NodeVisitor {
	llvm::IRBuilder<>  builder;
	llvm::LLVMContext& context;
	llvm::Module*      module;
	llvm::Value*       emmittedValue;
	gen::Mangler*      moduleMangler;
	Function* functionOwner;
	std::vector<Node*> moduleInitializerBody;
	int currentGeneratingRound;
	bool needsPointer;

	/**
	* Control flow chains
	*/
	//for building loops
	struct LoopChainNode {
		LoopExpression* loop;
		llvm::BasicBlock* body;
		llvm::BasicBlock* after;
		LoopChainNode* prev;
	};
	LoopChainNode* loopChain;
	//for building if fallthroughs
	bool ifFallthrough;

	llvm::FunctionPassManager* passManager;

	LLVMgenerator(llvm::LLVMContext& _context,Node* root,llvm::Module* module,llvm::FunctionPassManager* passManager,int round);
	llvm::Type* genType(Type* type);
	void emitConstant(llvm::Constant* value);
	inline void emit(llvm::Value* value){ emmittedValue = value; }
	llvm::Value* generateExpression(Node* node);
	llvm::Value* generatePointerExpression(Node* node);
	std::pair<llvm::Value*,llvm::Value*> generateLinearSequencePair(Node* node);
	static llvm::GlobalValue::LinkageTypes genLinkage(PrefixDefinition* def);

	inline void map(DefinitionNode* def,llvm::Value* value){
		def->generatorData = value;
		def->generatorDataRound = currentGeneratingRound;
	}
	inline llvm::Value* unmap(DefinitionNode* def){
		auto round = def->generatorDataRound;
		if(round != currentGeneratingRound) return nullptr;
		return reinterpret_cast<llvm::Value*>(def->generatorData);
	}
	inline static void map(DeclaredType* type,llvm::Type* value){
		type->generatorData = value;
	}
	inline static llvm::Type* unmap(DeclaredType* type){
		return reinterpret_cast<llvm::Type*>(type->generatorData);
	}
	inline void emitLoad(llvm::Value* value){
		emmittedValue = builder.CreateLoad(value);
	}
	inline void emitStore(llvm::Value* var,llvm::Value* val){
		emmittedValue = builder.CreateStore(val,var);
	}

	//
	Node* visit(IntegerLiteral* node);
	Node* visit(CharacterLiteral* node);
	Node* visit(BoolExpression* node);
	Node* visit(FloatingPointLiteral* node);
	Node* visit(StringLiteral* node);

	void emitCreateLinearSequence(llvm::Value* ptr,llvm::Value* length,Type* next);

	Node* visit(VariableReference* node);
	Node* visit(AssignmentExpression* node);
	Node* visit(FieldAccessExpression* node);
	Node* visit(CallExpression* node);
	Node* visit(CastExpression* node);
	Node* visit(LogicalOperation* node);
	Node* visit(PointerOperation* node);
	Node* visit(ReturnExpression* node);
	Node* visit(IfExpression* node);
	Node* visit(ControlFlowExpression* node);
	Node* visit(LoopExpression* node);
	Node* visit(BlockExpression* node);
	void  gen(BlockExpression* node);



	Node* visit(Variable* var);
	Node* visit(Function* func);
	Node* visit(TypeDeclaration* node);

	llvm::GlobalVariable* getGlobalVariableDeclaration(Variable* variable);
	inline llvm::Value*   getVariable(Variable* variable) {
		return variable->isLocal()? unmap(variable): getGlobalVariableDeclaration(variable);
	}
	llvm::Function* getFunctionDeclaration(Function* function);
	llvm::Function* getIntrinsicFunctionDeclaration(const char* name,llvm::ArrayRef<llvm::Type*> args,llvm::Type* ret);
};


LLVMgenerator::LLVMgenerator(
	llvm::LLVMContext& _context,
	Node* root,llvm::Module* module,
	llvm::FunctionPassManager* passManager,
	int round) : 
	context(_context),
	builder(_context) 
{
	this->module     = module;
	this->passManager= passManager;

	emmittedValue = nullptr;
	functionOwner = nullptr;
	loopChain = nullptr;
	ifFallthrough = false;
	currentGeneratingRound = round;
	needsPointer = false;

	//create a module initializer

	gen(root->asBlockExpression());
}

llvm::Type* generateAnonymousRecord(LLVMgenerator* generator,AnonymousAggregate* type){
	std::vector<llvm::Type*> fields(type->numberOfFields);
	for(size_t i = 0;i<type->numberOfFields;i++){
		fields[i] = generator->genType(type->types[i]);
	}
	return llvm::StructType::get(generator->context,fields,false);
}

llvm::Type* getRecordDeclaration(LLVMgenerator* generator,Record* record){
	if(record->generatorData) return generator->unmap(record);

	//mangle
	gen::Mangler::Element mangler(generator->moduleMangler);
	mangler.mangle(record->declaration);

	auto numberOfFields = record->fields.size();
	std::vector<llvm::Type*> fields(numberOfFields);
	for(size_t i = 0;i<numberOfFields;i++){
		fields[i] = generator->genType(record->fields[i].type.type());
	}
	auto t = llvm::StructType::create(generator->context,fields,mangler.stream.str(),false);	
	generator->map(record,t);
	return t;
}

//TODO
llvm::Type* generateAnonymousVariant(LLVMgenerator* generator,AnonymousAggregate* type){
	std::vector<llvm::Type*> fields(type->numberOfFields);
	for(size_t i = 0;i<type->numberOfFields;i++){
		fields[i] = generator->genType(type->types[i]);
	}
	return llvm::StructType::get(generator->context,fields,false);
}

//TODO
llvm::Type* getVariantDeclaration(LLVMgenerator* generator,Variant* variant){
	return llvm::Type::getInt32Ty(generator->context);
}

llvm::Type* generatePointerType(LLVMgenerator* generator,Type* next){
	return llvm::PointerType::get(generator->genType(next),0);
}

// { T* begin,T* end }
llvm::Type* generateLinearSequence(LLVMgenerator* generator,Type* next){
	auto type = llvm::PointerType::get(generator->genType(next),0);
	llvm::Type* fields[2] = { type,type };
	return llvm::StructType::get(generator->context,fields,false);
}

llvm::Type* generateFunctionPointerType(LLVMgenerator* generator,FunctionPointer* type){
	llvm::Type* result;
	auto arg = type->parameter();
	if(arg->isVoid()){
		result= llvm::FunctionType::get(generator->genType(type->returns()),false);
	}
	else if(auto rec = arg->asAnonymousRecord()){
		std::vector<llvm::Type*> types;
		for(auto i =rec->types;i!=rec->types + rec->numberOfFields;i++){
			types.push_back(generator->genType(*i));
		}
		result= llvm::FunctionType::get(generator->genType(type->returns()),types,false);
	}
	else {
		result= llvm::FunctionType::get(generator->genType(type->returns()),generator->genType(arg),false);
	}
	return llvm::PointerType::get(result,0);
}

Node* LLVMgenerator::visit(TypeDeclaration* node){
	return node;
}

llvm::Type* LLVMgenerator::genType(Type* type){
	switch(type->type){
	case Type::VOID: return llvm::Type::getVoidTy(context);
	case Type::TYPE: assert(false); break;
	case Type::BOOL: return builder.getInt1Ty();
	case Type::RECORD:
		return getRecordDeclaration(this,static_cast<Record*>(type));
	case Type::VARIANT:
		return getVariantDeclaration(this,static_cast<Variant*>(type));
	case Type::INTEGER:
		{
		auto bits = std::abs(type->bits);
		return coreTypes[bits == 32? GEN_TYPE_I32 : (bits == 16? GEN_TYPE_I16 : (bits == 64? GEN_TYPE_I64 : GEN_TYPE_I8))];
		}
	case Type::NATURAL: 
		return coreTypes[GEN_TYPE_SIZE_T];
	case Type::UINTPTRT:
		return coreTypes[GEN_TYPE_SIZE_T];//TODO
	case Type::FLOAT:
		return type->bits == 32? llvm::Type::getFloatTy(context) : llvm::Type::getDoubleTy(context);
	case Type::CHAR:
		return coreTypes[type->bits == 8? GEN_TYPE_I8 : (type->bits == 32? GEN_TYPE_I32 : GEN_TYPE_I16)];
	
	case Type::POINTER:
		return generatePointerType(this,type->next());
	case Type::LINEAR_SEQUENCE:
		return generateLinearSequence(this,type->next());

	case Type::FUNCTION_POINTER:
		return generateFunctionPointerType(this,static_cast<FunctionPointer*>(type));

	case Type::NODE: 
		assert(false); break;
	case Type::ANONYMOUS_RECORD:
		return generateAnonymousRecord(this,static_cast<AnonymousAggregate*>(type));
	case Type::ANONYMOUS_VARIANT:
		return generateAnonymousVariant(this,static_cast<AnonymousAggregate*>(type));


	case Type::TRAIT:
	case Type::LITERAL_INTEGER:
	case Type::LITERAL_FLOAT:
	case Type::LITERAL_CHAR:
	case Type::LITERAL_STRING:
		assert(false); break;
	}
	return coreTypes[0];
}
void LLVMgenerator::emitConstant(llvm::Constant* value){
	emmittedValue = value;
}
llvm::Value* LLVMgenerator::generateExpression(Node* node){
	emmittedValue = nullptr;
	node->accept(this);
	return emmittedValue;
}
llvm::Value* LLVMgenerator::generatePointerExpression(Node* node){
	auto old= needsPointer;
	needsPointer = true;
	emmittedValue = nullptr;
	node->accept(this);
	needsPointer = old;
	return emmittedValue;
}

llvm::GlobalValue::LinkageTypes LLVMgenerator::genLinkage(PrefixDefinition* def){
	return def->isPublic() ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage;
}

Node* LLVMgenerator::visit(IntegerLiteral* node){
	emitConstant(llvm::ConstantInt::get(genType(node->explicitType),node->integer.u64,false));
	return node;
}
Node* LLVMgenerator::visit(CharacterLiteral* node){
	emitConstant(llvm::ConstantInt::get(genType(node->explicitType),node->value,false));
	return node;
}
Node* LLVMgenerator::visit(BoolExpression* node){
	emitConstant(node->value? builder.getTrue() : builder.getFalse());
	return node;
}
Node* LLVMgenerator::visit(FloatingPointLiteral* node){
	emitConstant(llvm::ConstantFP::get(genType(node->explicitType),node->value));
	return node;
}
Node* LLVMgenerator::visit(StringLiteral* node){
	emitCreateLinearSequence(builder.CreateGlobalStringPtr(node->block.ptr()),builder.getInt64(node->block.length()),Type::getCharType(8));
	return node;
}

//Create a temporary stack variable for unoptimized transfer
void LLVMgenerator::emitCreateLinearSequence(llvm::Value* begin,llvm::Value* length,Type* next){
	auto block = builder.GetInsertBlock();
	llvm::IRBuilder<> _builder(block,block->begin());
	auto var = _builder.CreateAlloca(generateLinearSequence(this,next),nullptr,"seqtmp");
	
	builder.CreateStore(begin,builder.CreateStructGEP(var,0));
	builder.CreateStore(builder.CreateGEP(begin,length),builder.CreateStructGEP(var,1));

	if(needsPointer) emit(var);
	else emitLoad(var);
}

Node* LLVMgenerator::visit(VariableReference* node){
	//if(node->variable->asArgument())
	//	emit(unmap(node->variable));
	if(needsPointer) emit(getVariable(node->variable));
	else emitLoad(getVariable(node->variable));
	return node;
}

Node* LLVMgenerator::visit(FieldAccessExpression* node){
	auto neededPointer = needsPointer;
	if(neededPointer) needsPointer = false;
	auto ptr = node->object->returnType()->isPointer()? generateExpression(node->object) : generatePointerExpression(node->object);
	auto fieldPtr = builder.CreateStructGEP(ptr,node->field);
	if(neededPointer) emit(fieldPtr); 
	else emitLoad(fieldPtr);

	return node;
}

inline bool isSignedInteger(Type* t){
	return t->isInteger() && t->bits<0 ? true : false;
}

// TODO: comparisons
llvm::Value* genIntegerOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,Type* operand1Type,llvm::Value* operand1,llvm::Value* operand2){
	using namespace data::ast::Operations;

	static const llvm::ICmpInst::Predicate ucmp [] = 
	{ llvm::ICmpInst::ICMP_EQ,llvm::ICmpInst::ICMP_ULT,llvm::ICmpInst::ICMP_UGT,llvm::ICmpInst::ICMP_ULE,llvm::ICmpInst::ICMP_UGE };
	static const llvm::ICmpInst::Predicate icmp [] = 
	{ llvm::ICmpInst::ICMP_EQ,llvm::ICmpInst::ICMP_SLT,llvm::ICmpInst::ICMP_SGT,llvm::ICmpInst::ICMP_SLE,llvm::FCmpInst::ICMP_SGE };

	switch(op){
	case NEGATION:
		return generator->builder.CreateNeg(operand1);

	case ADDITION:
		return generator->builder.CreateAdd(operand1,operand2);
	case SUBTRACTION:
		return generator->builder.CreateSub(operand1,operand2);
	case MULTIPLICATION:
		return generator->builder.CreateMul(operand1,operand2);
	case DIVISION:
		return isSignedInteger(operand1Type)? generator->builder.CreateSDiv(operand1,operand2) : generator->builder.CreateUDiv(operand1,operand2);
	case REMAINDER:
		return isSignedInteger(operand1Type)? generator->builder.CreateSRem(operand1,operand2) : generator->builder.CreateURem(operand1,operand2);

	case BIT_NOT:
		return generator->builder.CreateNot(operand1);
	case BIT_AND:
		return generator->builder.CreateAnd(operand1,operand2);
	case BIT_OR:
		return generator->builder.CreateOr(operand1,operand2);
	case BIT_XOR:
		return generator->builder.CreateXor(operand1,operand2);

	case LEFT_SHIFT:
		return generator->builder.CreateShl(operand1,operand2);
	case RIGHT_SHIFT:
		return isSignedInteger(operand1Type)? generator->builder.CreateAShr(operand1,operand2) : generator->builder.CreateLShr(operand1,operand2);

	case EQUALITY_COMPARISON:
	case LESS_COMPARISON:
	case GREATER_COMPARISON:
	case LESS_EQUALS_COMPARISON:
	case GREATER_EQUALS_COMPARISON:
		return generator->builder.CreateICmp( (isSignedInteger(operand1Type)?icmp:ucmp)[op-EQUALITY_COMPARISON] ,operand1,operand2);
	}

	assert(false && "Invalid int operation");
	return nullptr;
}

//TODO fabs,tan, inverse trig
llvm::Value* genIntrinsicRealOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,llvm::Value* operand1,llvm::Value* operand2){
	using namespace data::ast::Operations;

	if(op <= TRIG_TAN){

		static llvm::Intrinsic::ID intrinsicFunctions[] = 
		{ llvm::Intrinsic::sin,
		  llvm::Intrinsic::pow,
		  llvm::Intrinsic::sqrt,
		  llvm::Intrinsic::exp,
		  llvm::Intrinsic::log,
		  llvm::Intrinsic::sin,
		  llvm::Intrinsic::cos,
		  llvm::Intrinsic::sin 
		};
			
		auto type = operand1->getType();
		if(op == MATH_POW){
			llvm::Type* args[2] = {type,type};
			auto func = llvm::Intrinsic::getDeclaration(generator->module,intrinsicFunctions[op-MATH_ABS],args);
			return generator->builder.CreateCall2(func,operand1,operand2,"calltmp");
		}
		else {
			auto func = llvm::Intrinsic::getDeclaration(generator->module,intrinsicFunctions[op-MATH_ABS],type);
			return generator->builder.CreateCall(func,operand1,"calltmp");
		}
		
	}

	assert(false && "Invalid real intrinsic operation");
	return nullptr;
}

llvm::Value* genRealOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,llvm::Value* operand1,llvm::Value* operand2){
	using namespace data::ast::Operations;

	static const llvm::FCmpInst::Predicate ocmp [] = 
	{ llvm::FCmpInst::FCMP_OEQ,llvm::FCmpInst::FCMP_OLT,llvm::FCmpInst::FCMP_OGT,llvm::FCmpInst::FCMP_OLE,llvm::FCmpInst::FCMP_OGE };

	if(op>=MATH_ABS) return genIntrinsicRealOperation(generator,op,operand1,operand2);

	switch(op){
	case NEGATION:
		return generator->builder.CreateFNeg(operand1);

	case ADDITION:
		return generator->builder.CreateFAdd(operand1,operand2);
	case SUBTRACTION:
		return generator->builder.CreateFSub(operand1,operand2);
	case MULTIPLICATION:
		return generator->builder.CreateFMul(operand1,operand2);
	case DIVISION:
		return generator->builder.CreateFDiv(operand1,operand2);

	case EQUALITY_COMPARISON:
	case LESS_COMPARISON:
	case GREATER_COMPARISON:
	case LESS_EQUALS_COMPARISON:
	case GREATER_EQUALS_COMPARISON:
		return generator->builder.CreateFCmp(ocmp[op-EQUALITY_COMPARISON],operand1,operand2);

	}

	assert(false && "Invalid real operation");
	return nullptr;
}

llvm::Value* genBoolOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,llvm::Value* operand1,llvm::Value* operand2){
	using namespace data::ast::Operations;

	switch(op){
	case NEGATION:
		return generator->builder.CreateXor(operand1,1);

	case EQUALITY_COMPARISON:
		return generator->builder.CreateICmpEQ(operand1,operand2);
	}

	assert(false && "Invalid boolean operation");
	return nullptr;
}

// TODO: bounds checking, slicing

struct LinearSequenceValues {
	llvm::Value *beginptr,*begin;
	llvm::Value *end;
};

void loadLinearSequence(LLVMgenerator* generator,llvm::Value* operand1,LinearSequenceValues& sequence,bool loadBegin = true,bool loadEnd = true){
	sequence.beginptr = generator->builder.CreateStructGEP(operand1,0);
	if(loadBegin) sequence.begin =  generator->builder.CreateLoad(sequence.beginptr);
	if(loadEnd)   sequence.end   =  generator->builder.CreateLoad(generator->builder.CreateStructGEP(operand1,1));
}
std::pair<llvm::Value*,llvm::Value*> LLVMgenerator::generateLinearSequencePair(Node* node){
	llvm::Value *begin,*end;
	if(auto str = node->asStringLiteral()){
		begin = builder.CreateGlobalStringPtr(str->block.ptr());
		end   = builder.CreateGEP(begin,builder.getInt64(str->block.length()));
	}
	else {
		auto sequence = generatePointerExpression(node);
		begin = builder.CreateLoad(builder.CreateStructGEP(sequence,0));
		end   = builder.CreateLoad(builder.CreateStructGEP(sequence,1));
	}
	return std::make_pair(begin,end);
}
llvm::Value* generateLinearSequenceBegin(LLVMgenerator* generator,Node* node){
	auto sequence = generator->generatePointerExpression(node);
	auto begin = generator->builder.CreateLoad(generator->builder.CreateStructGEP(sequence,0));
	return begin;
}
bool optimizeLinearSequenceAssignment(LLVMgenerator* generator,Node* src,Node* dest){
	if(auto str = src->asStringLiteral()){
		auto begin = generator->builder.CreateGlobalStringPtr(str->block.ptr());
		auto ptr   = generator->generatePointerExpression(dest);
		generator->builder.CreateStore(begin,generator->builder.CreateStructGEP(ptr,0));
		auto end   = generator->builder.CreateGEP(begin,generator->builder.getInt64(str->block.length()));
		generator->builder.CreateStore(end,generator->builder.CreateStructGEP(ptr,1));
		return true;
	}
	return false;
}

llvm::Value* genLinearSequenceOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,llvm::Value* operand1,llvm::Value* operand2,llvm::Value* operand3){
	LinearSequenceValues sequence;

	using namespace data::ast::Operations;
	switch(op){
	// (natural) ( (end - begin) / sizeof(*begin) )
	case LENGTH:
		loadLinearSequence(generator,operand1,sequence,true,true);
		return generator->builder.CreateCast(llvm::Instruction::Trunc,generator->builder.CreatePtrDiff(sequence.end,sequence.begin),coreTypes[GEN_TYPE_SIZE_T]);
	
	// begin + i
	case ELEMENT_GET:
		loadLinearSequence(generator,operand1,sequence,true,false);
		if(!operand2) return sequence.begin;
		if(auto cnst = llvm::dyn_cast<llvm::ConstantInt>(operand2)){
			if(cnst->getValue() == 0) return sequence.begin;
		}
		return generator->builder.CreateGEP(sequence.begin,operand2);
	
	// *(begin + i) = v
	case ELEMENT_SET:
		loadLinearSequence(generator,operand1,sequence,true,false);
		generator->builder.CreateStore(operand3,generator->builder.CreateGEP(sequence.begin,operand2));
	
	// begin >= end
	case SEQUENCE_EMPTY:
		loadLinearSequence(generator,operand1,sequence,true,true);
		return generator->builder.CreateICmpUGE(
			generator->builder.CreatePtrToInt(sequence.begin,llvm::Type::getInt64Ty(generator->context)),
			generator->builder.CreatePtrToInt(sequence.end,llvm::Type::getInt64Ty(generator->context))
			);

	// begin++
	case SEQUENCE_MOVENEXT:
		loadLinearSequence(generator,operand1,sequence,true,false);
		return generator->builder.CreateStore(
			generator->builder.CreateGEP(sequence.begin,llvm::ConstantInt::get(llvm::Type::getInt64Ty(generator->context),1,false)),
			sequence.beginptr);

	}

	assert(false && "Invalid sequence operation");
}

/**
Scenarios:
  !(x == y) => x != y
*/
llvm::Value* optimize1ArgOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,Type* argType,Node* arg){
	using namespace data::ast::Operations;
	
	if(argType->isBool() && op == NEGATION){
		auto call = arg->asCallExpression();
		if(!call) return nullptr;
		auto func = call->object->asFunctionReference();
		if(!func) return nullptr;
		if(func->function->isIntrinsicOperation()){
			auto op2 = func->function->getOperation();
			if(op2 == EQUALITY_COMPARISON){
				//TODO
			}
		}
	}

	return nullptr;
}

// TODO: fix linear sequences
llvm::Value* genOperation(LLVMgenerator* generator,data::ast::Operations::Kind op,Node* arg){
	Type*  operand1Type;
	llvm::Value* values[3];
	if(auto tuple = arg->asTupleExpression()){
		assert(tuple->size() < 4);

		auto args = tuple->childrenPtr();
		operand1Type = args[0]->returnType();
		values[0] = generator->generateExpression(args[0]);
		values[1] = generator->generateExpression(args[1]);
		if(tuple->size() == 3) 
			values[2] = generator->generateExpression(args[2]);
		else values[2] = nullptr;
	}
	else {
		operand1Type = arg->returnType();
		values[0] = generator->generateExpression(arg);
		values[1] = nullptr;
		values[2] = nullptr;
	}

	if(operand1Type->isPointer() && operand1Type->next()->isLinearSequence()){
		return genLinearSequenceOperation(generator,op,values[0],values[1],values[2]);
	}
	else if(operand1Type->isInteger() || operand1Type->isPlatformInteger() || operand1Type->isUintptr()){
		return genIntegerOperation(generator,op,operand1Type,values[0],values[1]);
	}
	else if(operand1Type->isFloat()){
		return genRealOperation(generator,op,values[0],values[1]);
	}
	else if(operand1Type->isBool()){
		return genBoolOperation(generator,op,values[0],values[1]);
	}


	assert(false && "Invalid operation");
}

llvm::CallingConv::ID genCallingConvention(data::ast::Function::CallConvention cc){
	switch(cc){
	case data::ast::Function::ARPHA:   return llvm::CallingConv::Fast;
	case data::ast::Function::CCALL:   return llvm::CallingConv::C;
	case data::ast::Function::STDCALL: return llvm::CallingConv::X86_StdCall;
	}
}

Node* LLVMgenerator::visit(CallExpression* node){
	llvm::Value* callee;
	llvm::CallInst* instr;
	const char* reg;
	Function* function;

	if(auto fcall = node->object->asFunctionReference()){
		if(fcall->function->isIntrinsicOperation()){
			emit(genOperation(this,fcall->function->getOperation(),node->arg));
			return node;
		}
		function = fcall->function;
		reg = function->returns()->isVoid()? "" : "calltmp";
		callee = getFunctionDeclaration(function);
	}
	else {
		auto ret = node->object->returnType();
		reg = ret->asFunctionPointer()->returns()->isVoid()? "" : "calltmp";
		callee = generateExpression(node->object); //calling a function pointer
		function = nullptr;
	}
	
	llvm::Twine twine(reg);
	if(node->arg->asUnitExpression()){
		instr = builder.CreateCall(callee,twine);
	}
	else if(auto tuple = node->arg->asTupleExpression()) {
		auto argCount = tuple->size();
		if(argCount == 2){
			auto arg1 = *(tuple->begin());
			auto arg2 = *(tuple->begin() + 1);
			if(arg1->returnType()->isLinearSequence()){
				auto sequence1 = generateLinearSequencePair(arg1);
				if(arg2->returnType()->isLinearSequence()){
					auto sequence2 = generateLinearSequencePair(arg2);
					instr = builder.CreateCall4(callee,sequence1.first,sequence1.second,sequence2.first,sequence2.second,twine);
				}
				else instr = builder.CreateCall3(callee,sequence1.first,sequence1.second,generateExpression(arg2),twine);
			}
			else if(arg2->returnType()->isLinearSequence()){
				auto sequence2 = generateLinearSequencePair(arg2);
				instr = builder.CreateCall3(callee,generateExpression(arg1),sequence2.first,sequence2.second,twine);
			}
			else instr = builder.CreateCall2(callee,generateExpression(*(tuple->begin())),generateExpression(*(tuple->begin()+1)),twine);
		} else {
			std::vector<llvm::Value*> args;
			args.reserve(argCount);
			for(auto i = tuple->begin();i!=tuple->end();i++){
				args.push_back(generateExpression(*i));
			}	
			instr = builder.CreateCall(callee,args,twine);
		}
	}
	else {
		if(node->arg->returnType()->isLinearSequence()){
			auto sequence = generateLinearSequencePair(node->arg);
			instr = builder.CreateCall2(callee,sequence.first,sequence.second,twine);
		}
		else instr = builder.CreateCall(callee,generateExpression(node->arg),twine);
	}

	if(function){
		instr->setCallingConv(genCallingConvention(function->callingConvention()));
		if(function->isNonthrow()) instr->setDoesNotThrow(true);
	}
	else {
		auto fpType = node->object->returnType()->asFunctionPointer();
		instr->setCallingConv(genCallingConvention(fpType->callingConvention()));
		if(fpType->isNonthrow()) instr->setDoesNotThrow(true);
	}

	emit(instr);
	return node;
}

Node* LLVMgenerator::visit(PointerOperation* node){
	if(node->isAddress()){
		emit(generatePointerExpression(node->expression));
	} else {
		if(this->needsPointer) emit(generateExpression(node->expression));
		else emitLoad(generateExpression(node->expression));
	}
	return node;
}

Node* LLVMgenerator::visit(AssignmentExpression* node){
	auto objType= node->object->asVariable()? node->object->asVariable()->type.type() : node->object->returnType();
	if(objType->isLinearSequence() && optimizeLinearSequenceAssignment(this,node->value,node->object)){
		return node;
	}

	auto val = generateExpression(node->value);
	assert(val);
	auto ptr = generatePointerExpression(node->object);
	emitStore(ptr,val);
	return node;
}

static inline bool isIntLike(Type* t){
	return t->isInteger() || t->isPlatformInteger() || t->isUintptr() || t->isChar() || t->isBool();
}

Node* LLVMgenerator::visit(CastExpression* node){
	auto src = node->object->returnType();
	auto dest = node->type;
	int  cast = -1;

	if(isIntLike(src)){
		auto srcBits = std::abs(src->bits);
		if(isIntLike(dest)){
			auto destBits = std::abs(dest->bits);
			if(srcBits < destBits){
				if(src->bits < 0 && dest->bits < 0) cast = llvm::Instruction::SExt;
				else cast = llvm::Instruction::ZExt;
			}
			else cast = llvm::Instruction::Trunc;
		}
		else if(dest->isFloat()){
			cast = src->bits<0? llvm::Instruction::SIToFP : llvm::Instruction::UIToFP;
		}
	}
	else if(src->isFloat()){
		if(dest->isFloat())
			cast = src->bits < dest->bits? llvm::Instruction::FPExt : llvm::Instruction::FPTrunc;
		else if(isIntLike(dest))
			cast = dest->bits < 0 ? llvm::Instruction::FPToSI : llvm::Instruction::FPToUI;
	}
	else if(src->isPointer()){
		if(dest->isUintptr())      cast = llvm::Instruction::PtrToInt;
		else if(dest->isPointer()) cast = llvm::Instruction::BitCast;
	}
	else if(src->isLinearSequence()){
		if(dest->isLinearSequence()) cast = llvm::Instruction::BitCast;
		else if(dest->isUintptr() || dest->isPointer()){
			auto begin = generateLinearSequenceBegin(this,node->object);
			emit(dest->isPointer()? begin : builder.CreateCast(llvm::Instruction::PtrToInt,begin,llvm::Type::getInt64Ty(context)));
			return node;
		}
	}

	if(cast != -1){
		auto obj = generateExpression(node->object);
		auto t   = genType(node->type);
		emit(builder.CreateCast(llvm::Instruction::CastOps(cast),obj,t));
	}
	else assert(false && "Invalid cast!");
	return node;
}

Node* LLVMgenerator::visit(ReturnExpression* node){
	if(!functionOwner->_returnType.type()->isVoid()){
		builder.CreateRet(generateExpression(node->expression));
	}
	else {
		if(!node->expression->asUnitExpression()) generateExpression(node->expression);
		builder.CreateRetVoid();
	}
	return node;
}

ControlFlowExpression* isBreakContinue(Node* node){
	if(auto block = node->asBlockExpression()){
		if(block->size() == 1) node = *block->begin();
	}
	if(auto flow = node->asControlFlowExpression()){
		if(flow->isBreak() || flow->isContinue()) return flow;
	}
	return nullptr;
}

Node* LLVMgenerator::visit(IfExpression* node){
	bool returnsValue   = false;
	bool isSelect = returnsValue && !node->consequence->asBlockExpression() && !node->alternative->asBlockExpression();
	
	//condition
	auto cond = generateExpression(node->condition);

	//if(true) 1 else 2
	if(isSelect){
		emit(builder.CreateSelect(cond,generateExpression(node->consequence),generateExpression(node->alternative)));
		return node;
	}

	//More complex if
	llvm::BasicBlock* alternativeBlock = nullptr;
	llvm::Value* consequenceValue,*alternativeValue;
	auto f = builder.GetInsertBlock()->getParent();

	//branch
	auto consequenceBlock = llvm::BasicBlock::Create(context, "then", f);
	auto mergingBlock     = llvm::BasicBlock::Create(context,"ifcont");
	if(!node->alternative->asUnitExpression()){
		alternativeBlock = llvm::BasicBlock::Create(context,"else");
		builder.CreateCondBr(cond,consequenceBlock,alternativeBlock);
	}
	else {
		/**
		* Optimize : loop { if(condition) break | continue; ... }
		*/
		if(auto flow = isBreakContinue(node->consequence)){
			f->getBasicBlockList().pop_back();
			consequenceBlock = nullptr;
			builder.CreateCondBr(cond,flow->isBreak()? loopChain->after : loopChain->body,mergingBlock);
		}
		else builder.CreateCondBr(cond,consequenceBlock,mergingBlock);
	}

	ifFallthrough = false;

	//consequence
	if(consequenceBlock){
		builder.SetInsertPoint(consequenceBlock);
		if(returnsValue) consequenceValue = generateExpression(node->consequence);
		else generateExpression(node->consequence);
		consequenceBlock = builder.GetInsertBlock();//update consequence block
		if(ifFallthrough){
			assert(alternativeBlock);
			builder.CreateBr(alternativeBlock);
			ifFallthrough = false;
		} else {
			if(!llvm::isa<llvm::BranchInst>(consequenceBlock->back()))
				builder.CreateBr(mergingBlock);
		}
	}
	//alternative
	if(alternativeBlock){
		f->getBasicBlockList().push_back(alternativeBlock);
		builder.SetInsertPoint(alternativeBlock);
		
		if(returnsValue) alternativeValue = generateExpression(node->alternative);
		else generateExpression(node->alternative);
		alternativeBlock = builder.GetInsertBlock();//update alternative block

		if(!llvm::isa<llvm::BranchInst>(alternativeBlock->back()))
			builder.CreateBr(mergingBlock);	
	}
	//merge
	f->getBasicBlockList().push_back(mergingBlock);
	builder.SetInsertPoint(mergingBlock);
	if(returnsValue){
		auto phi = builder.CreatePHI(genType(node->returnType()),2,"iftmp");
		phi->addIncoming(consequenceValue,consequenceBlock);
		phi->addIncoming(alternativeValue,alternativeBlock);
		emit(phi);
	}
	return node;
}

//TODO

/**
Scenarios: 
  x && y => if(x) if(y) true else false else false
  x || y => if(x) true  else if(y) true else false
*/
Node* LLVMgenerator::visit(LogicalOperation* node){
	auto cond1 = generateExpression(node->parameters[0]);

	auto f = builder.GetInsertBlock()->getParent();
	auto consequenceBlock1 = llvm::BasicBlock::Create(context, "cond1true", f);
	auto consequenceBlock2 = llvm::BasicBlock::Create(context, "cond2true");
	auto alternativeBlock  = llvm::BasicBlock::Create(context,"condFalse");
	auto mergingBlock      = llvm::BasicBlock::Create(context,"cont");

	builder.CreateCondBr(cond1,consequenceBlock1,alternativeBlock);

	builder.SetInsertPoint(consequenceBlock1);
	auto cond2 = generateExpression(node->parameters[1]);
	builder.CreateCondBr(cond2,consequenceBlock2,alternativeBlock);

	f->getBasicBlockList().push_back(consequenceBlock2);
	builder.SetInsertPoint(consequenceBlock2);
	auto trueValue = builder.getTrue();
	builder.CreateBr(mergingBlock);

	f->getBasicBlockList().push_back(alternativeBlock);
	builder.SetInsertPoint(alternativeBlock);
	auto falseValue = builder.getFalse();
	builder.CreateBr(mergingBlock);

	f->getBasicBlockList().push_back(mergingBlock);
	builder.SetInsertPoint(mergingBlock);
	auto phi = builder.CreatePHI(builder.getInt1Ty(),2,"iftmp");
	phi->addIncoming(trueValue,consequenceBlock2);
	phi->addIncoming(falseValue,alternativeBlock);
	emit(phi);

	return node;
}

Node* LLVMgenerator::visit(ControlFlowExpression* node){
	if(node->isBreak()){
		builder.CreateBr(loopChain->after);
	} else if(node->isContinue()){
		builder.CreateBr(loopChain->body);
	} else if(node->isFallthrough()){
		ifFallthrough = true;
	}
	return node;
}
Node* LLVMgenerator::visit(LoopExpression* node){
	//body
	auto preBlock  = builder.GetInsertBlock();
	auto loopBlock = llvm::BasicBlock::Create(context,"loop",preBlock->getParent());
	builder.CreateBr(loopBlock);
	builder.SetInsertPoint(loopBlock);
	//after
	auto afterBlock = llvm::BasicBlock::Create(context,"loopcont");
	
	LoopChainNode loopNode = { node,loopBlock,afterBlock,loopChain };
	loopChain = &loopNode;
	if(auto block = node->body->asBlockExpression()){
		gen(block);
	} else generateExpression(node->body);
	loopChain = loopNode.prev;

	builder.CreateBr(loopBlock);
	//merge
	preBlock->getParent()->getBasicBlockList().push_back(afterBlock);
	builder.SetInsertPoint(afterBlock);
	
	return node;
}

void  LLVMgenerator::gen(BlockExpression* node){
	for(auto i = node->begin();i!=node->end();i++){
		generateExpression(*i);
	}
}

Node* LLVMgenerator::visit(BlockExpression* node){
	gen(node);
	return node;
}

llvm::GlobalVariable*  LLVMgenerator::getGlobalVariableDeclaration(Variable* variable){
	if(auto unmapped = unmap(variable)) return static_cast<llvm::GlobalVariable*>(unmapped);

	// mangle
	gen::Mangler::Element mangler(moduleMangler);
	mangler.mangle(variable);

	auto threadLocal = true;
	auto cnst = false;
	llvm::Constant* init = llvm::ConstantInt::get(coreTypes[GEN_TYPE_I8],0,false);
	auto var = new llvm::GlobalVariable(*module,genType(variable->type.type()),cnst,genLinkage(variable),nullptr,mangler.stream.str(),nullptr,threadLocal);
	//var->setAlignment(variable->type.type()->alignment());
	map(variable,var);

	return var;
}

Node* LLVMgenerator::visit(Variable* variable){
	if(!functionOwner){
		auto var = getGlobalVariableDeclaration(variable);

		if(needsPointer) emit(var);
	} else {
		auto block = builder.GetInsertBlock();
		llvm::IRBuilder<> _builder(block,block->begin());
		auto var = _builder.CreateAlloca(genType(variable->type.type()),nullptr,variable->label().ptr());
		map(variable,var);

		if(needsPointer) emit(var);
	}
	return variable;
}


llvm::Function* LLVMgenerator::getIntrinsicFunctionDeclaration(const char* name,llvm::ArrayRef<llvm::Type*> args,llvm::Type* ret){
	auto func = llvm::Function::Create(llvm::FunctionType::get(ret,args,false),
		llvm::GlobalValue::ExternalLinkage,name,module);
	return func;
}

llvm::Function* LLVMgenerator::getFunctionDeclaration(Function* function){
	if(auto unmapped = unmap(function)) return static_cast<llvm::Function*>(unmapped);

	// mangle
	gen::Mangler::Element mangler(moduleMangler);
	mangler.mangle(function);

	//create the actual declaration
	llvm::FunctionType* t;
	bool optimizeSequences = function->arguments.size() < 3;
	if(function->arguments.size() == 0){
		t = llvm::FunctionType::get(genType(function->_returnType.type()),false);
	} else {
		std::vector<llvm::Type*> args;
		args.reserve(function->arguments.size());
		for(auto i = function->arguments.begin();i!=function->arguments.end();i++){
			if(optimizeSequences && (*i)->type.type()->isLinearSequence()){
				auto pt = generatePointerType(this, (*i)->type.type()->next());
				args.push_back(pt);
				args.push_back(pt);
			}
			else args.push_back(genType((*i)->type.type()));
		}
		t = llvm::FunctionType::get(genType(function->_returnType.type()),args,false);
	}

	auto func = llvm::Function::Create(t,genLinkage(function),function->isExternal() || function->label() == "main" ? function->label().ptr() : mangler.stream.str(),module);
	func->setCallingConv(genCallingConvention(function->label() == "main"? data::ast::Function::CCALL : function->callingConvention()));
	if(function->isNonthrow()){
		func->setDoesNotThrow(true);
	}
	map(function,func);
	return func;
}

Node* LLVMgenerator::visit(Function* function){
	if(function->isExternal() || function->isFlagSet(Function::MACRO_FUNCTION) || function->isFieldAccessMacro() || 
		function->isTypeTemplate() || function->isFlagSet(Function::HAS_PATTERN_ARGUMENTS) || function->isFlagSet(Function::HAS_EXPENDABLE_ARGUMENTS)) return function;

	auto func = getFunctionDeclaration(function);

	//generate body
	llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", func);

	// Initialize arguments
	size_t i = 0;
	bool optimizeSequences = function->arguments.size() < 3;
	for (llvm::Function::arg_iterator arg = func->arg_begin(); i != function->arguments.size(); ++arg, ++i) {
		if(optimizeSequences && function->arguments[i]->type.type()->isLinearSequence()){
			std::string n = function->arguments[i]->label().ptr();

			arg->setName(n + "_begin");
			llvm::IRBuilder<> _builder(block,block->begin());
			auto var = _builder.CreateAlloca(genType(function->arguments[i]->type.type()),nullptr,function->arguments[i]->label().ptr());
			_builder.CreateStore(arg,_builder.CreateStructGEP(var,0));

			++arg;
			arg->setName(n + "_end");
			_builder.CreateStore(arg,_builder.CreateStructGEP(var,1));

			map(function->arguments[i],var);
		}
		else {
			arg->setName(function->arguments[i]->label().ptr());
		
			//arguments to allocas
			llvm::IRBuilder<> _builder(block,block->begin());
			auto var = _builder.CreateAlloca(genType(function->arguments[i]->type.type()),nullptr,function->arguments[i]->label().ptr());
			_builder.CreateStore(arg,var);
			map(function->arguments[i],var);
		}
	}

	builder.SetInsertPoint(block);

	auto oldFunction = functionOwner;
	functionOwner = function;
	gen(&function->body);
	if(!function->isFlagSet(Function::CONTAINS_RETURN)) builder.CreateRetVoid();
	if(llvm::verifyFunction(*func,llvm::PrintMessageAction)){
		//TODO
	}
	passManager->run(*func);
	functionOwner = oldFunction;

	return function;
}


/**
* LLVM backend implementation.
*/
namespace gen {

	static void createTargetTriple(LLVMBackend* backend,llvm::Triple *triple,data::gen::native::Target* target){
		using namespace data::gen::native;

		llvm::Triple::ArchType arch = llvm::Triple::UnknownArch;
		switch(target->cpuArchitecture){
		case Target::Arch::X86:
			arch = llvm::Triple::x86;
			break;
		case Target::Arch::ARM:
			arch = llvm::Triple::arm;
			break;
		}
		triple->setArch(arch);
		if(target->cpuMode == Target::Mode::M64){
			*triple = triple->get64BitArchVariant();
			if(triple->getArch() == llvm::Triple::UnknownArch) backend->onFatalError("The 64 bit mode isn't supported for the selected architecture!");
		}

		llvm::Triple::OSType os = llvm::Triple::UnknownOS;
		llvm::Triple::VendorType vendor = llvm::Triple::UnknownVendor;
		switch(target->platform){
		case Target::Platform::WINDOWS:
		case Target::Platform::WINDOWS_RT:
			os = llvm::Triple::Win32;
			vendor = llvm::Triple::PC;
			break;
		case Target::Platform::WINDOWS_MINGW:
			os = llvm::Triple::MinGW32;
			vendor = llvm::Triple::PC;
			break;
		case Target::Platform::LINUX:
			os = llvm::Triple::Linux;
			break;
		case Target::Platform::MACOSX:
			os = llvm::Triple::Darwin;
			vendor = llvm::Triple::Apple;
			break;
		}
		triple->setOS(os);
		triple->setVendor(vendor);

	}

	LLVMBackend::LLVMBackend(data::gen::native::Target* target,data::gen::Options* options){
		this->target  = target;
		this->options = options;

		llvm::LLVMContext& context = llvm::getGlobalContext();

		llvm::InitializeAllTargets();
		llvm::InitializeAllTargetMCs();
		llvm::InitializeAllAsmPrinters();
		llvm::InitializeAllAsmParsers();

		//todo target choosing
		llvm::Triple TheTriple;
		createTargetTriple(this,&TheTriple,target);

		const llvm::Target *TheTarget = 0;
		std::string Err;
		TheTarget = llvm::TargetRegistry::lookupTarget(TheTriple.getTriple(), Err);
		if (!TheTarget) {
			this->onFatalError(format("LLVM target: Couldn't find the target '%s'",TheTriple.str()));
		}

		auto MCPU = "";
		auto CMModel = llvm::CodeModel::Default;
		auto RelocModel = llvm::Reloc::Default;
		//features
		std::string FeaturesStr;

		//
		int optimizationLevel = options->optimizationLevel;
		llvm::CodeGenOpt::Level OLvl = optimizationLevel == -1 ? llvm::CodeGenOpt::None : 
			optimizationLevel == 0 ? llvm::CodeGenOpt::Less : optimizationLevel == 1 ? llvm::CodeGenOpt::Default : llvm::CodeGenOpt::Aggressive;

		//target options
		llvm::TargetOptions  Options;
		Options.LessPreciseFPMADOption = false;
		Options.PrintMachineCode = false;
		Options.NoFramePointerElim = false;
		Options.NoFramePointerElimNonLeaf = false;
		Options.NoExcessFPPrecision = false;
		Options.UnsafeFPMath = false;
		Options.NoInfsFPMath = false;
		Options.NoNaNsFPMath = false;
		Options.HonorSignDependentRoundingFPMathOption = 0;
		Options.UseSoftFloat = false;
		Options.FloatABIType = llvm::FloatABI::Default;
		Options.NoZerosInBSS = false;
		Options.GuaranteedTailCallOpt = false;
		Options.DisableTailCalls = false;
		Options.StackAlignmentOverride = false;
		Options.RealignStack = true;
		Options.DisableJumpTables = false;
		Options.TrapFuncName = "";
		Options.PositionIndependentExecutable = false;
		Options.EnableSegmentedStacks = false;

		targetMachine = TheTarget->createTargetMachine(TheTriple.getTriple(),MCPU,FeaturesStr.c_str(),Options,RelocModel,CMModel,OLvl);

		//typesystem
		if(target->cpuMode == data::gen::native::Target::Mode::M32){
			target->typeSystemState.pointerSizeof = 4;
			target->typeSystemState.functionPointerSizeof = 4;
			target->typeSystemState.sizetSizeof = 4;
		} else {
			target->typeSystemState.pointerSizeof = 8;
			target->typeSystemState.functionPointerSizeof = 8;
			target->typeSystemState.sizetSizeof = 8;
		}

		coreTypes[GEN_TYPE_I8]  = llvm::Type::getInt8Ty(context);
		coreTypes[GEN_TYPE_I16] = llvm::Type::getInt16Ty(context);
		coreTypes[GEN_TYPE_I32] = llvm::Type::getInt32Ty(context);
		coreTypes[GEN_TYPE_I64] = llvm::Type::getInt64Ty(context);
		coreTypes[GEN_TYPE_SIZE_T] = target->typeSystemState.sizetSizeof == 8 ? llvm::Type::getInt64Ty(context) : llvm::Type::getInt32Ty(context);
		coreTypes[GEN_TYPE_VOID]   = llvm::Type::getVoidTy(context);
		coreTypes[GEN_TYPE_FLOAT]  = llvm::Type::getFloatTy(context);
		coreTypes[GEN_TYPE_DOUBLE] = llvm::Type::getDoubleTy(context);
	}

	int round = 0;

	//Generates a binary module
	static void genModule(LLVMBackend* backend,int outputFormat,llvm::Module* module,std::string& dest){
		//Open file
		std::string err;
		llvm::raw_fd_ostream out(dest.c_str(), err, llvm::raw_fd_ostream::F_Binary);
		if (!err.empty()){
			backend->onError(format("Couldn't open a file '%s' for writing!",dest));
			return;
		}
		//write instructions to file
		if(outputFormat == data::gen::native::OBJECT || outputFormat == data::gen::native::ASSEMBLY){
			using namespace llvm;
			llvm::Module& mod = *module;
			llvm::TargetMachine& Target = *targetMachine;
			llvm::TargetMachine::CodeGenFileType FileType = 
				outputFormat == data::gen::native::OBJECT ? llvm::TargetMachine::CGFT_ObjectFile : llvm::TargetMachine::CGFT_AssemblyFile;
			llvm::raw_fd_ostream& Out = out;

			PassManager PM;
			
			// Add the target data from the target machine, if it exists, or the module.
			if (const TargetData *TD = Target.getTargetData())
				PM.add(new TargetData(*TD));
			else
				PM.add(new TargetData(&mod));

			Target.setAsmVerbosityDefault(true);
			Target.setMCRelaxAll(true);
			formatted_raw_ostream FOS(Out);

			// Ask the target to add backend passes as necessary.
			if (Target.addPassesToEmitFile(PM, FOS, FileType, true)) {
				backend->onFatalError("LLVM target doesn't support the generation of this file type!");
				return;
			}

			PM.run(mod);
		}
		else if(outputFormat == LLVMBackend::OUTPUT_BC ){
			llvm::WriteBitcodeToFile(module,out);
		}
	}
	
	std::string LLVMBackend::generateModule(Node* root,const char* outputDirectory,const char* moduleName,int outputFormat){
		using namespace llvm;
		auto module = new Module(StringRef(moduleName),getGlobalContext());
		auto passManager = new FunctionPassManager(module);
		// Add the target data from the target machine, if it exists, or the module.
		if (auto TD = targetMachine->getTargetData())
			passManager->add(new TargetData(*TD));
		else
			passManager->add(new TargetData(module));
		// Create various passes
		passManager->add(createPromoteMemoryToRegisterPass());
		int optimizationLevel = options->optimizationLevel;
		if(optimizationLevel >= 0){ 
			// Do simple "peephole" optimizations and bit-twiddling optzns.
			passManager->add(createInstructionCombiningPass());
			//Simplify the control flow graph (deleting unreachable blocks, etc).
			passManager->add(createCFGSimplificationPass());
		}
		if(optimizationLevel >= 1){
			// Provide basic AliasAnalysis support for GVN.
			passManager->add(createBasicAliasAnalysisPass());
			// Reassociate expressions.
			passManager->add(createReassociatePass());
			// Eliminate Common SubExpressions.
			passManager->add(createGVNPass());
		}
		passManager->doInitialization();
		
		LLVMgenerator generator(getGlobalContext(),root,module,passManager,round);
		round++;
		module->dump();
		//extension
		const char* extension;
		bool isWinMSVS = target->platform == data::gen::AbstractTarget::Platform::WINDOWS || target->platform == data::gen::AbstractTarget::Platform::WINDOWS_RT;
		if(outputFormat == data::gen::native::OBJECT)        extension =  isWinMSVS ? ".obj" : ".o";
		else if(outputFormat == data::gen::native::ASSEMBLY) extension =  isWinMSVS ? ".asm" : ".S";
		else extension = ".bc";
		//
		std::string path = std::string(outputDirectory) + "/" + moduleName + extension;
		genModule(this,outputFormat,module,path);
		delete passManager;
		delete module;
		return path;
	}

};

