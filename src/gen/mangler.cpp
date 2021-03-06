#include "../base/base.h"
#include "../base/symbol.h"
#include "../base/system.h"
#include "../compiler.h"
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

void Mangler::Element::mangleComponents(Node* component){
	if(auto block = component->asBlockExpression()){ 
		if(block->parentNode != nullptr){
			if(auto func = block->parentNode->asFunction()){
				mangle(func);
				return;
			} else mangleComponents(block->parentNode);
		} else stream<<'A';

		//mangleComponents(block->parentNode);
		if(!block->label().isNull()) stream<<block->label().length()<<block->label();
		else stream<<blockComponentLength((uintptr_t)block)<<'_'<<((uintptr_t)block);
	} else if(auto typeDecl = component->asTypeDeclaration()){ 
		mangle(typeDecl);
	} else assert(false && "Invalid parent Node");
}

void mangleNode(std::stringstream& stream,Node* node){
	auto label = node->label();
	if(!label.isNull()) stream<<label.length()<<label;
	else stream<<blockComponentLength((uintptr_t)node)<<'_'<<((uintptr_t)node);
}

char mangleCC(uint8 cc){
	switch(cc){
	case data::ast::Function::CCALL:   return 'C';
	case data::ast::Function::STDCALL: return 'S';
	}
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
	case Type::NATURAL:  stream<<'s'; break;
	case Type::UINTPTRT: stream<<'l'; break;
	case Type::RECORD:   mangle(static_cast<Record*>(type)->declaration); break;
	case Type::VARIANT:  mangle(static_cast<Variant*>(type)->declaration); break;

	case Type::POINTER:  stream<<'P'; mangle(type->next()); break;
	case Type::REFERENCE: stream<<'Q'; mangle(type->next()); break;
	case Type::LINEAR_SEQUENCE: stream<<'S'; mangle(type->next()); break;
	case Type::STATIC_ARRAY: stream<<'W';stream<<static_cast<StaticArray*>(type)->length(); mangle(type->next());  break;

	case Type::FUNCTION_POINTER: 
		{
		auto fp = static_cast<FunctionPointer*>(type);
		stream<<'C';
		//properties
		bool streamedProperties = false;
		if(fp->callingConvention() != data::ast::Function::ARPHA){
			if(!streamedProperties){ stream<<'_'; streamedProperties = true; }
			stream<<'c'<<mangleCC(fp->callingConvention());
		}
		mangle(fp->parameter());mangle(fp->returns()); break;
		}

	case Type::ANONYMOUS_RECORD:
	case Type::ANONYMOUS_VARIANT:
		{
		auto rec = static_cast<AnonymousAggregate*>(type);
		if(rec->isFlagSet(AnonymousAggregate::GEN_REWRITE_AS_VECTOR)){
			stream<<'X'<<rec->numberOfFields;
			mangle(rec->types[0]);
			break;
		}
		stream<<(type->type == Type::ANONYMOUS_RECORD?'T':'U')<<rec->numberOfFields<<'_';
		for(size_t i = 0;i < rec->numberOfFields;i++){
			if(rec->fields && !rec->fields[i].isNull())
				stream<<rec->fields[i].length()<<rec->fields[i];
			else stream<<'0';
			mangle(rec->types[i]);
		}
	}
	break;

	case Type::QUALIFIER:
		if(type->hasConstQualifier()) stream<<"O";
		else assert(false && "Invalid qualifier");
		mangle(type->next()); break;
	default:
		assert(false);
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
	if(function->isNonthrow()){
		if(!streamedProperties){ stream<<'_'; streamedProperties = true; }
		stream<<'n';
	}

	mangleNode(stream,function);
	stream<<function->arguments.size()<<'_';
	for(auto i = function->arguments.begin();i!=function->arguments.end();i++){
		stream<<(*i)->label().length()<<(*i)->label().ptr();
		mangle((*i)->type.type());
	}
	//mangle(function->returns()); NB: can't do because function can return a type declared inside, which would cause infinite loop!
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
