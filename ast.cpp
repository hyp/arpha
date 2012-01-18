#include "common.h"
#include "scope.h"
#include "declarations.h"
#include "parser.h"

#include "ast.h"
#include "compiler.h"
#include "arpha.h"


void* ExpressionFactory::allocate(size_t size){
	return malloc(size);
}

ConstantExpression* ExpressionFactory::makeConstant(){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->type = compiler::Error;
	e->_isLiteral = false;
	return e;
}
ConstantExpression* ExpressionFactory::makeNothing(){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->type = arpha::Nothing;
	e->_isLiteral = false;//???
	return e;
}
ConstantExpression* ExpressionFactory::makeCompilerNothing(){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->type = compiler::Nothing;
	e->_isLiteral = false;
	return e;
}
ConstantExpression* ExpressionFactory::makeError(){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->type = compiler::Error;
	e->_isLiteral = false;
	return e;
}
ConstantExpression* ExpressionFactory::makeScopeReference(Scope* scope){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->refScope = scope;
	e->type = compiler::scopeRef;
	e->_isLiteral = false;
	return e;
}
ConstantExpression* ExpressionFactory::makeFunctionReference(FunctionDef* func){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->refFunction = func;
	e->type = compiler::function;
	e->_isLiteral = false;
	return e;
}
ConstantExpression* ExpressionFactory::makeTypeReference(Type* type){
	auto e = new(allocate(sizeof(ConstantExpression))) ConstantExpression;
	e->refType = type;
	e->type = compiler::type;
	e->_isLiteral = false;
	return e;
}


VariableExpression* ExpressionFactory::makeVariable(Variable* var){
	auto e =new(allocate(sizeof(VariableExpression))) VariableExpression;
	e->variable = var;
	return e;
}

OverloadSetExpression* ExpressionFactory::makeOverloadSet(SymbolID symbol,Scope* scope){
	auto e = new(allocate(sizeof(OverloadSetExpression))) OverloadSetExpression;
	e->scope = scope;
	e->symbol = symbol;
	return e;
}
CallExpression*   ExpressionFactory::makeCall(Node* object,Node* argument){
	auto e = new(allocate(sizeof(CallExpression))) CallExpression;
	e->object = object;
	e->arg = argument;
	return e;
}
AccessExpression* ExpressionFactory::makeAccess(Node* object,SymbolID symbol){
	auto e = new(allocate(sizeof(AccessExpression))) AccessExpression;
	e->object = object;
	e->symbol = symbol;
	e->field  = nullptr;
	return e;
}

ReturnExpression* ExpressionFactory::makeReturn(Node* expression){
	auto e = new(allocate(sizeof(ReturnExpression))) ReturnExpression;
	e->value = expression;
	return e;
}
TupleExpression* ExpressionFactory::makeTuple(){
	auto e =new(allocate(sizeof(TupleExpression))) TupleExpression;
	return e;
}
TupleExpression* ExpressionFactory::makeTuple(Node* a,Node* b){
	if( a->is<TupleExpression>() ){ //TODO, not 0 children?
		((TupleExpression*)a)->children.push_back(b);
		return (TupleExpression*)a;
	}
	auto e =new(allocate(sizeof(TupleExpression))) TupleExpression;
	e->children.push_back(a);
	e->children.push_back(b);
	return e;
}
BlockExpression* ExpressionFactory::makeBlock(){
	return new(allocate(sizeof(BlockExpression))) BlockExpression;
}

#define CASE(t) case t::__value__

Node* ExpressionFactory::duplicate(const Node* node){
	return (Node*)node;//TODO
}


static Type* literalConstantReturnType(const ConstantExpression* node){
		if(node->type == arpha::int64)
			return abs(node->i64) <= int64(std::numeric_limits<int>::max()) ? arpha::int32 : arpha::int64;
		else if(node->type == arpha::uint64){
			if(node->u64 <= uint64(std::numeric_limits<int>::max())) return arpha::int32;
			else if(node->u64 <= uint64(std::numeric_limits<uint32>::max())) return arpha::uint32;
			else if(node->u64 <= uint64(std::numeric_limits<int64>::max())) return arpha::int64;
			else return arpha::uint64;		
		}
		else if(node->type == arpha::float64) return arpha::float64;
		assert(false);
		return nullptr;
}

Type* returnType(const Node* node){

	switch(node->__type){
		CASE(ConstantExpression): 
			if( ((ConstantExpression*)node)->isLiteral() ) return literalConstantReturnType((ConstantExpression*)node);
			return ((ConstantExpression*)node)->type;
		CASE(VariableExpression): return ((VariableExpression*)node)->variable->type;
		CASE(CallExpression):	  
			if( ((CallExpression*)node)->object->is<ConstantExpression>() && ((ConstantExpression*) (((CallExpression*)node)->object) )->type == compiler::function ) 
				return ((ConstantExpression*) (((CallExpression*)node)->object) )->refFunction->returnType;
			else return compiler::Unresolved;
		//CASE(AccessExpression): return ((AccessExpression*)node)->
		CASE(AssignmentExpression):  return returnType( ((AssignmentExpression*)node)->object.v );
		CASE(TupleExpression): assert(((TupleExpression*) node)->type); return ((TupleExpression*) node)->type;
	}

	return compiler::Nothing;
}

std::ostream& operator<< (std::ostream& stream,const ConstantExpression* node){
	if(node->isLiteral()) stream<<'#';
	if(node->type == arpha::uint64)       stream<<node->u64;
	else if(node->type == arpha::int64) stream<<node->i64;
	else if(node->type == arpha::float64) stream<<node->f64;
	else if(node->type == compiler::type) stream<<"type "<<node->refType->id;
	else if(node->type == compiler::function) stream<<"func "<<node->refFunction->id;
	else if(node->type == compiler::scopeRef) stream<<"scope";
	else if(node->type == compiler::Error)   stream<<"error";
	else if(node->type == compiler::Nothing) stream<<"statement";
	else if(node->type == arpha::Nothing) stream<<"nothing";
	else assert(false);
	return stream;
}

std::ostream& operator<< (std::ostream& stream,const Node* node){
	stream<<'(';

	switch(node->__type){
		CASE(ConstantExpression): stream<<(ConstantExpression*)node; break;
		CASE(VariableExpression): stream<<"variable "<<((VariableExpression*)node)->variable->id; break;
		CASE(OverloadSetExpression): stream<<"overloadset "<<((OverloadSetExpression*)node)->symbol; break;
		CASE(CallExpression): stream<<"call "<<((CallExpression*)node)->object<<" with "<<((CallExpression*)node)->arg; break;
		CASE(AccessExpression): stream<<((AccessExpression*)node)->object<<" . "<<((AccessExpression*)node)->symbol; break;
		CASE(AssignmentExpression): stream<< ((AssignmentExpression*)node)->object.v <<" = "<<((AssignmentExpression*)node)->value; break;
		CASE(ReturnExpression): stream<<"return"<<((ReturnExpression*)node)->value; break;
		CASE(TupleExpression):
			for(auto i = ((TupleExpression*) node)->children.begin();i != ((TupleExpression*) node)->children.end();){
				stream<<(*i);
				++i;
				if( i == ((TupleExpression*) node)->children.end() ) break;
				stream<<',';
			}
			break;
		CASE(BlockExpression):
			stream<<"{\n  ";
			for(auto i = ((BlockExpression*) node)->children.begin();i != ((BlockExpression*) node)->children.end(); ++i) stream<<(*i)<<";\n  "; 
			stream<<"}";
			break;
	}

	stream<<"):"<<returnType(node)->id;
	return stream;
}

bool isEqual(const Node* a,const Node* b){
	if(a->__type != b->__type) return false;
	if(returnType(a) != returnType(b)) return false;

	switch(a->__type){
		CASE(TupleExpression):
			if( ((TupleExpression*) a)->children.size() != ((TupleExpression*) b)->children.size() ) return false;
			for(size_t i=0;i< ((TupleExpression*) a)->children.size() ;++i){
				if( !isEqual(((TupleExpression*) a)->children[0],((TupleExpression*) b)->children[0]) ) return false;
			}
			return true;
	}

	return false;
}

unittest(ast){
	auto vref = new VariableExpression;
	assert(vref->is<VariableExpression>());
	delete vref;
}

#undef CASE
