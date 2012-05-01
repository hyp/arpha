#include "c.h"
#include "../ast/visitor.h"
#include "../compiler.h"
#include "../arpha.h"

struct TranslatorImplementation: NodeVisitor {
	std::ostream& stream;

	TranslatorImplementation(std::ostream& str) : stream(str) {}
	Node* visit(ConstantExpression* node){
		stream<<'(';
		if(node->type == arpha::uint64)       stream<<node->u64;
		else if(node->type == arpha::int64) stream<<node->i64;
		else if(node->type == arpha::float64) stream<<node->f64;
		else if(node->type == arpha::constantString) stream<<'"'<<node->string<<'"';
		else if(node->type == arpha::Nothing) stream<<"void";
		else if(node->type == arpha::boolean) stream<<(node->u64?"true":"false");
		else assert(false);
		stream<<')';
		return node;
	}
	Node* visit(VariableExpression* node){
		stream<<'('<<node->variable->id<<')';
		return node;
	}
	Node* visit(TupleExpression* node){
		auto i = node->children.begin();
		while(1){
			(*i)->accept(this);
			++i;
			if( i == node->children.end() ) break;
			stream<<" , ";
		}
		return node;
	}
	Node* visit(CallExpression* node){
		
		return node;
	}
	Node* visit(AssignmentExpression* node){
		node->object->accept(this);
		stream<<"=";
		node->value->accept(this);
		return node;
	}
	Node* visit(BlockExpression* node){
		stream<<"{\n\t";
		for(auto i =node->children.begin();i != node->children.end(); ++i){
			(*i)->accept(this);stream<<";\n\t"; 
		}
		stream<<"}";
		return node;
	}

};

void translate(std::ostream& output,Node* root){
	output<<"//This file was produced by arpha compiler"<<std::endl<<"#include \"arpha_c_stub.h\""<<std::endl<<std::endl;
	TranslatorImplementation translator(output);
	root->accept(&translator);
}
