#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../compiler.h"
#include "node.h"
#include "visitor.h"
#include "declarations.h"

/**
* Node tracer
*/
struct NodeToString: NodeVisitor {
	std::ostream& stream;
	NodeToString(std::ostream& ostream) : stream(ostream) {}

	Node* visit(NodeReference* node){
		stream<<"[> "<<node->node()<<" <]";
		return node;
	}
	Node* visit(IntegerLiteral* node){
		stream<<node->integer;
		return node;
	}
	Node* visit(FloatingPointLiteral* node){
		stream<<node->value;
		return node;
	}
	Node* visit(CharacterLiteral* node){
		if(node->value<=127) stream<<'\''<<(char)node->value<<'\'';
		else stream<<"'\\U"<<node->value<<'\'';
		return node;
	}
	Node* visit(BoolExpression* node){
		stream<<(node->value?"true":"false");
		return node;
	}
	Node* visit(ArrayLiteral* node){
		stream<<'[';
		for(auto i = node->begin();i!=node->end();i++){
			stream<<*i;
			if(i+1 != node->end()) stream<<" , ";
		}
		stream<<']';
		return node;
	}
	Node* visit(ErrorExpression* node){
		stream<<"error";
		return node;
	}
	Node* visit(StringLiteral* node){
		stream<<'"';
		for(size_t i = 0;i<node->block.length();i++){
			stream<<node->block[i];
		}
		stream<<'"';
		return node;
	}
	Node* visit(UnitExpression* node){
		stream<<"()";
		return node;
	}
	Node* visit(ImportedScopeReference* node){
		stream<<"scope-ref "<<node->scope->label();
		return node;
	}
	Node* visit(TypeReference* node){
		stream<<node->type;
		return node;
	}
	Node* visit(TraitReference* node){
		stream<<node->trait;
		return node;
	}
	Node* visit(VariableReference* node){
		stream<<"variable "<<node->variable->label()<<"["<<(void*)node->variable<<"]";
		return node;
	}
	Node* visit(FunctionReference* node){
		stream<<"ref func "<<node->function->label();
		return node;
	}

	Node* visit(CallExpression* node){
		stream<<"call "<<node->object<<" with "<<node->arg;
		return node;
	}
	Node* visit(LogicalOperation* node){
		stream<<node->parameters[0]<<(node->isOr()?" || ":" && ")<<node->parameters[1];
		return node;
	}
	Node* visit(FieldAccessExpression* node){
		stream<<node->object<<" f. "<<node->fieldsName();
		return node;
	}

	Node* visit(AssignmentExpression* node){
		stream<<node->object<<" = "<<node->value;
		return node;
	}
	Node* visit(ReturnExpression* node){
		stream<<"return"<<' '<<node->expression;
		return node;
	}
	Node* visit(ControlFlowExpression* node){
		stream<<(node->isContinue() ? "continue" : (node->isBreak() ? "break" : "fallthrough"));
		return node;
	}
	Node* visit(ThrowExpression* node){
		stream<<"throw "<<node->expression;
		return node;
	}
	Node* visit(PointerOperation* node){
		stream<<(node->isAddress()?'&':'*')<<node->expression;
		return node;
	}
	Node* visit(IfExpression* node){
		stream<<"if("<<node->condition<<") "<<node->consequence<<" else "<<node->alternative;
		return node;
	}
	Node* visit(TupleExpression* node){
		auto i = node->children.begin();
		if( i == node->children.end() ) return node;
		while(1){
			stream<<(*i);
			++i;
			if( i == node->children.end() ) break;
			stream<<',';
		}
		return node;
	}
	Node* visit(BlockExpression* node){
		stream<<"{\n  ";
		for(auto i =node->children.begin();i != node->children.end(); ++i) stream<<(*i)<<";\n  "; 
		stream<<"}";
		return node;
	}
	Node* visit(LoopExpression* node){
		stream<<"loop "<<node->body;
		return node;
	}
	Node* visit(CastExpression* node){
		stream<<node->object<<" as "<<node->type;
		return node;
	}

	Node* visit(ExpressionVerifier* node){
		stream<<node->expression<<" with expected type "<<node->expectedType;
		return node;
	}
	Node* visit(UnresolvedSymbol* node){
		stream<<node->symbol;
		return node;
	}

	Node* visit(AccessExpression* node){
		stream<<node->object<<" u. "<<node->symbol;
		return node;
	}

	Node* visit(MatchResolver* node){
		stream<<"match("<<node->object<<")";
		return node;
	}

	Node* visit(Variable* node){
		stream<<"variable "<<node->label()<<" as "<<node->type<<"["<<(void*)node<<"]";
		return node;
	}

	void newline(){
		stream<<std::endl<<"\t";
	}

	//TODO variant
	Node* visit(TypeDeclaration* decl){
		auto type = decl->type();
		if(auto record = type->asRecord()){
			stream<<"type "<<decl->label()<<" {";
			for(auto i = record->fields.begin();i!=record->fields.end();i++){
				newline();
				stream<<"var "<<(*i).name<<" "<<(*i).type;
			}
			newline();
			stream<<"}";
		}
		else if(auto variant = type->asVariant()){
			stream<<"variant "<<decl->label();
		}
		else {
			stream<<"trait "<<decl->label();
		}

		if(decl->optionalStaticBlock){
			stream<<decl->optionalStaticBlock;
		}
		//newline();
		return decl;
	}
	Node* visit(Function* node){
		stream<<"function "<<node->label()<<"(";
		for(auto i = node->arguments.begin();i!=node->arguments.end();i++) stream<<(*i);
		stream<<")";
		stream<<" -> "<<node->_returnType;
		newline();
		stream<<(&node->body);
		return node;
	}

	Node* visit(PrefixMacro* node){
		stream<<"prefix macro "<<node->label();
		newline();
		stream<<&node->func()->body;
		return node;
	}

	Node* visit(InfixMacro* node){
		stream<<"infix macro "<<node->label()<<" with precedence: "<<node->stickiness;
		newline();
		stream<<&node->func()->body;
		return node;
	}
};

std::ostream& operator<< (std::ostream& stream,TypePatternUnresolvedExpression& type){
	if(type.isResolved()) return stream<<type.type();
	else if(type.isPattern()){
		return type.pattern ? stream<<type.pattern : stream<<"_";
	} else return stream<<type.unresolvedExpression;
}

std::ostream& operator<< (std::ostream& stream,Type* type){
	if(type->hasConstSemantics()) stream<<"const ";
	switch(type->type){
		case Type::VOID: stream<<"void"; break;
		case Type::TYPE: stream<<"Type"; break;
		case Type::BOOL: stream<<"bool"; break;
		case Type::RECORD:  stream<<static_cast<Record*>(type)->declaration->label(); break;
		case Type::VARIANT: stream<<static_cast<Variant*>(type)->declaration->label(); break;
		case Type::INTEGER: stream<<(type->bits<0?"int":"uint")<<(type->bits<0?-type->bits:type->bits); break;
		case Type::FLOAT: stream<<(type->bits == 32? "float":"double"); break;
		case Type::CHAR:  stream<<"char"<<type->bits; break;
		case Type::NATURAL: stream<<"natural"; break;
		case Type::UINTPTRT: stream<<"uintptr"; break;

		case Type::POINTER: 
			stream<<"Pointer("<<type->next()<<')'; break;
		case Type::LINEAR_SEQUENCE:
			stream<<"LinearSequence("<<type->next()<<")"; break;
		case Type::STATIC_ARRAY:
			stream<<"Array("<<type->next()<<","<<type->N<<')'; break;

		case Type::FUNCTION_POINTER: 
			stream<<"Function("<<type->argument<<" -> "<<static_cast<FunctionPointer*>(type)->returns()<<')'; break;

		case Type::NODE:
			stream<<"ast.Expression";
			if(type->nodeSubtype != -1) stream<<"("<<type->nodeSubtype<<")"; 
			break;
		case Type::ANONYMOUS_RECORD:
		case Type::ANONYMOUS_VARIANT:
			{
			auto sep = type->type == Type::ANONYMOUS_RECORD? ", ":"| ";
			auto aggr = static_cast<AnonymousAggregate*>(type);
			stream<<"(";
			for(size_t i =0;i<aggr->numberOfFields;i++){
				if(i) stream<<sep;
				if(aggr->fields && !aggr->fields[i].isNull()) stream<<aggr->fields[i]<<": ";
				stream<<aggr->types[i];
			}
			stream<<")";
			}
			break;
		case Type::VARIANT_OPTION:
			stream<<(static_cast<VariantOption*>(type)->declaration->label()); break;

		case Type::TRAIT:
			stream<<(static_cast<Trait*>(type)->declaration->label()); break;

		case Type::LITERAL_INTEGER:
			stream<<"literal.integer"; break;
		case Type::LITERAL_FLOAT:
			stream<<"literal.real"; break;
		case Type::LITERAL_CHAR:
			stream<<"literal.char"; break;
		case Type::LITERAL_STRING:
			stream<<"literal.string"; break;
	}
	return stream;
}

std::ostream& operator<< (std::ostream& stream,Node* node){
	auto d = compiler::currentUnit()->printingDecorationLevel;
	if(d) stream<<'(';
	if(!node->label().isNull()) stream<<node->label()<<':';
	
	NodeToString visitor(stream);
	node->accept(&visitor);
	if(d) {
		stream<<')';
		stream<<"::";
		if(node->isResolved() && !node->asFunctionReference()) stream<<node->returnType();
		else stream<<"?";
	}
	return stream;
}
