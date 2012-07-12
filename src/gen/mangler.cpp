#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/system.h"
#include "../ast/node.h"
#include "../ast/scope.h"
#include "../ast/declarations.h"
#include "../data/data.h"
#include "mangler.h"

using namespace gen;

//The number to go before _495849
int blockComponentLength(uintptr_t n){
	int count = 2;
	for(; n > 9;count++) n/=10;
	return count;
}

void Mangler::Element::mangleModule(BlockExpression* root){
	stream<<"A3src";
}

void Mangler::Element::mangleComponents(Node* component){
	if(auto block = component->asBlockExpression()){ 
		if(block->parentNode == nullptr) mangleModule(block);
		else {
			mangleComponents(block->parentNode);
			if(!block->label().isNull()) stream<<block->label().length()<<block->label();
			else stream<<blockComponentLength((uintptr_t)block)<<'_'<<((uintptr_t)block);
		}
	} else assert(false);
}

void mangleNode(std::stringstream& stream,Node* node){
	auto label = node->label();
	if(!label.isNull()) stream<<label.length()<<label;
	else stream<<blockComponentLength((uintptr_t)node)<<'_'<<((uintptr_t)node);
}

unittest(gg){

}

void Mangler::Element::mangle(Type* type){
	switch(type->type){
	case Type::VOID:     stream<<'n'; break;
	case Type::BOOL:     stream<<'b'; break;
	case Type::INTEGER:
		{
		auto isSigned = type->bits<0;
		auto bits = isSigned?-type->bits:type->bits;
		char c;
		if(bits == 32)      c = (isSigned?'g':'h');
		else if(bits == 64) c = (isSigned?'j':'k');
		else if(bits == 8)  c = (isSigned?'o':'p');
		else if(bits == 16) c = (isSigned?'q':'r');
		else {
			c = 0;
			stream<<(isSigned?'i':'u')<<bits;
		}
		if(c!=0) stream<<c;
		}
		break;
	case Type::FLOAT:    stream<<(type->bits == 32? 'f':'d'); break;
	case Type::CHAR:     stream<<(type->bits == 8? 'x': type->bits == 32? 'y' : 'z'); break;
	case Type::RECORD:   mangle(static_cast<Record*>(type)->declaration); break;
	case Type::VARIANT:  mangle(static_cast<Variant*>(type)->declaration); break;
	case Type::POINTER:  stream<<'P'; mangle(type->next()); break;
	case Type::FUNCTION: stream<<'C'; break;
	case Type::ANONYMOUS_RECORD:
	case Type::ANONYMOUS_VARIANT:
		{
		auto rec = static_cast<AnonymousAggregate*>(type);
		stream<<(type->type == Type::ANONYMOUS_RECORD?'T':'U')<<rec->numberOfFields<<'_';
		for(size_t i = 0;i < rec->numberOfFields;i++){
			if(rec->fields && !rec->fields[i].isNull())
				stream<<rec->fields[i].length()<<rec->fields[i];
			else stream<<'0';
			mangle(rec->types[i]);
		}
	}
	break;
	case Type::LINEAR_SEQUENCE: stream<<'S'; mangle(type->next()); break;
	case Type::LITERAL_INTEGER: stream<<"li"; break;
	case Type::LITERAL_FLOAT:   stream<<"lf"; break;
	case Type::LITERAL_CHAR:    stream<<"lc"; break;
	case Type::LITERAL_STRING:  stream<<"ls"; break;
	default:
		assert(false);
	}
}

char mangleCC(uint8 cc){
	switch(cc){
	case data::ast::Function::CCALL:   return 'C';
	case data::ast::Function::STDCALL: return 'S';
	}
}

void Mangler::Element::mangle(Function* function){
	mangleComponents(function->parentNode);
	stream<<'F';
	//properties
	bool streamedProperties = false;
	if(function->callingConvention() != data::ast::Function::ARPHA){
		if(!streamedProperties){ stream<<'_'; streamedProperties = true; }
		stream<<'c'<<mangleCC(function->callingConvention());
	}

	mangleNode(stream,function);
	stream<<function->arguments.size()<<'_';
	for(auto i = function->arguments.begin();i!=function->arguments.end();i++){
		stream<<(*i)->label().length()<<(*i)->label().ptr();
		mangle((*i)->type.type());
	}
	mangle(function->returns());
}

void Mangler::Element::mangle(Variable* variable){
	mangleComponents(variable->parentNode);
	stream<<'G';
	mangleNode(stream,variable);
}

char typePrefix(Type* type){
	switch(type->type){
	case Type::RECORD:   return 'R';
	case Type::VARIANT:  return 'V';
	}
}

void Mangler::Element::mangle(TypeDeclaration* type){
	mangleComponents(type->parentNode);
	stream<<typePrefix(type->type());
	mangleNode(stream,type);
}
