#include "../base/base.h"
#include "../base/bigint.h"
#include "c.h"
#include "../ast/declarations.h"
#include "../ast/visitor.h"


struct TranslatorImplementation: NodeVisitor {
	std::ostream& stream;

	TranslatorImplementation(std::ostream& str) : stream(str) {}
	Node* visit(VariableReference* node){
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
