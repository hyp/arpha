#include "common.h"
#include "parser.h"
#include "ast.h"
#include "arpha.h"


#define CASE(t) case t::__value__

Type* returnType(const Node* node){

	switch(node->__type){
		CASE(TypeExpression):     return arpha::type;
		CASE(VariableExpression): return ((VariableExpression*)node)->variable->type;
		CASE(CallExpression):	  return ((CallExpression*)node)->object->is<FunctionExpression> () ? 
									( (FunctionExpression*)(((CallExpression*)node)->object) )->function->returnType : arpha::Unresolved;
		CASE(FieldAccessExpression): return ((FieldAccessExpression*)node)->field->type;
		CASE(AssignmentExpression):  return returnType( ((AssignmentExpression*)node)->object.v );
		CASE(TupleExpression): assert(((TupleExpression*) node)->type); return ((TupleExpression*) node)->type;
	}

	return arpha::Nothing;
}

std::ostream& operator<< (std::ostream& stream,const Node* node){
	stream<<'(';

	switch(node->__type){
		CASE(TypeExpression): stream<<"type "<<((TypeExpression*)node)->type->id; break;
		CASE(VariableExpression): stream<<"variable "<<((VariableExpression*)node)->variable->id; break;
		CASE(FunctionExpression): stream<<"func "<<((FunctionExpression*)node)->function->id; break;
		CASE(OverloadSetExpression): stream<<"overloadset "<<((OverloadSetExpression*)node)->name; break;
		CASE(CallExpression): stream<<"call "<<((CallExpression*)node)->object<<" with "<<((CallExpression*)node)->arg; break;
		CASE(FieldAccessExpression): stream<<((FieldAccessExpression*)node)->object.v<<" . "<<((FieldAccessExpression*)node)->field->name;
		CASE(AssignmentExpression): stream<< ((AssignmentExpression*)node)->object.v <<" = "<<((AssignmentExpression*)node)->value;
		CASE(ReturnExpression): stream<<"return"<<((ReturnExpression*)node)->value;
		CASE(TupleExpression):
			for(auto i = ((TupleExpression*) node)->children.begin();;){
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
		CASE(TypeExpression): return ((TypeExpression*)a)->type == ((TypeExpression*)b)->type;
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