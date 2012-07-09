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
	const char* unaryOp(int kind){
		switch(kind){
		case UnaryOperation::BOOL_NOT: return "!";
		case UnaryOperation::MINUS: return "-";
		}
		return "op";
	}
	Node* visit(UnaryOperation* node){
		if(node->kind() == UnaryOperation::BOUNDED_POINTER_LENGTH) stream<<node->expression<<".length";
		else stream<<' '<<unaryOp(node->kind())<<' '<<node->expression;
		return node;
	}
	const char* binaryOp(int kind){
		switch(kind){
		case BinaryOperation::BOOL_AND: return "&&";
		case BinaryOperation::BOOL_OR: return "||";
		case BinaryOperation::EQUALS: return "==";
		case BinaryOperation::LESS: return "<";
		case BinaryOperation::GREATER: return ">";
		case BinaryOperation::ADD: return "+";
		case BinaryOperation::SUBTRACT: return "-";
		case BinaryOperation::MULTIPLY: return "*";
		case BinaryOperation::DIVIDE: return "/";
		case BinaryOperation::MOD: return "%";
		}
		return "op";
	}
	Node* visit(BinaryOperation* node){
		if(node->kind() == BinaryOperation::BOUNDED_POINTER_ELEMENT) stream<<node->a<<"["<<node->b<<"]";
		else stream<<node->a<<' '<<binaryOp(node->kind())<<' '<<node->b;
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
	Node* visit(Record* record){
		stream<<"record "<<record->label()<<"{";
		newline();
		for(auto i = record->fields.begin();i!=record->fields.end();i++) stream<<"field "<<(*i).name<<" as "<<(*i).type;
		return record;
	}

	Node* visit(TypeDeclaration* decl){
		stream<<"DECL "<<decl->type();
		//newline();
		//for(auto i = record->fields.begin();i!=record->fields.end();i++) stream<<"field "<<(*i).name<<" as "<<(*i).type;
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
		case Type::TYPE: stream<<"type"; break;
		case Type::BOOL: stream<<"bool"; break;
		case Type::RECORD: stream<<type->record; break;
		case Type::INTEGER: stream<<type->integer->id; break;
		case Type::POINTER: 
			stream<<"Pointer("<<type->next()<<')'; break;
		case Type::POINTER_BOUNDED:
			stream<<"BoundedPointer("<<type->next()<<")"; break;
		case Type::FUNCTION: 
			stream<<"Function("<<type->argument<<"->"<<type->returns<<')'; break;
		case Type::STATIC_ARRAY:
			stream<<"Array("<<type->next()<<","<<type->N<<')'; break;
		case Type::POINTER_BOUNDED_CONSTANT:
			stream<<"BoundedPointer("<<type->next()<<","<<type->N<<")"; break;
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
		if(node->isResolved()) stream<<node->returnType();
		else stream<<"?";
	}
	return stream;
}
