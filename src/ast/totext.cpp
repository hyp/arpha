#include "../base/base.h"
#include "../base/bigint.h"
#include "../base/symbol.h"
#include "../compiler.h"
#include "node.h"
#include "declarations.h"

/**
* Node tracer
*/
void Node::dump(Dumper& dumper) const {
	if(dumper.isVerbose()) dumper.print("(");
	dumpImplementation(dumper);
	if(dumper.isVerbose()){
		dumper.print(")");
		dumper.print(" :: ");
		if(isResolved()) returnType()->dump(dumper);
		else dumper.print("?");
	}
}

void IntegerLiteral::dumpImplementation(Dumper& dumper) const {
	dumper.print(format("%s",integer));
	if(isFlagSet(LiteralNode::EXPLICIT_TYPE)){
		dumper.print(" as ");
		explicitType->dump(dumper);
	}
}
void FloatingPointLiteral::dumpImplementation(Dumper& dumper) const {
	dumper.print(format("%s",value));
	if(isFlagSet(LiteralNode::EXPLICIT_TYPE)){
		dumper.print(" as ");
		explicitType->dump(dumper);
	}
}
void CharacterLiteral::dumpImplementation(Dumper& dumper) const {
	dumper.print("'");
	dumper.print(format("%s",value));
	dumper.print("'");
	if(isFlagSet(LiteralNode::EXPLICIT_TYPE)){
		dumper.print(" as ");
		explicitType->dump(dumper);
	}
}
void BoolExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print(format("%s",value?"true":"false"));
}
void StringLiteral::dumpImplementation(Dumper& dumper) const {
	dumper.print("\"");
	dumper.print(block.ptr());
	dumper.print("\"");
	if(isFlagSet(LiteralNode::EXPLICIT_TYPE)){
		dumper.print(" as ");
		explicitType->dump(dumper);
	}
}
void NodeReference::dumpImplementation(Dumper& dumper) const {
	dumper.print("[> ");
	node()->dump(dumper);
	dumper.print(" <]");
}
void ArrayExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("[ ");
	for(auto i = begin();i!=end();++i){
		(*i)->dump(dumper);
		if(i+1 != end()) dumper.print(" , ");
	}
	dumper.print(" ]");
}
void TupleExpression::dumpImplementation(Dumper& dumper) const {
	for(auto i = begin();i!=end();++i){
		(*i)->dump(dumper);
		if(i+1 != end()) dumper.print(" , ");
	}
}
void ErrorExpression::dumpImplementation(Dumper& dumper) const { dumper.print("error"); }
void UnitExpression::dumpImplementation(Dumper& dumper) const { dumper.print("()"); }
void ImportedScopeReference::dumpImplementation(Dumper& dumper) const { dumper.print("scope-ref");dumper.print(scope->label()); }
void VariableReference::dumpImplementation(Dumper& dumper) const {
	if(dumper.refDeclPointers()) dumper.print(format("variable [%d]",(void*)variable));

	if(variable->label().isNull()) dumper.print(format("unnamed%d",(void*)variable));
	else dumper.print(variable->label());
}
void FunctionReference::dumpImplementation(Dumper& dumper) const {
	if(dumper.refDeclPointers()) dumper.print(format("function [%d]",(void*)function));

	if(function->label().isNull()) dumper.print(format("unnamed%d",(void*)function));
	else dumper.print(function->label());
}
void TypeReference::dumpImplementation(Dumper& dumper) const {
	if(dumper.refDeclPointers()) dumper.print(format("type [%d]",(void*)type));

	type->dump(dumper);
}
void TraitParameterReference::dumpImplementation(Dumper& dumper) const {
	dumper.print("concept arg");
}
void CallExpression::dumpImplementation(Dumper& dumper) const {
	object->dump(dumper);
	dumper.print("(");
	if(!arg->asUnitExpression()) arg->dump(dumper);
	dumper.print(")");
}
void LogicalOperation::dumpImplementation(Dumper& dumper) const {
	parameters[0]->dump(dumper);
	dumper.print(isOr()?" || ":" && ");
	parameters[1]->dump(dumper);
}
void FieldAccessExpression::dumpImplementation(Dumper& dumper) const {
	object->dump(dumper);
	auto name = fieldsName();
	if(name.isNull()){
		dumper.print(format("[%d]",field));
	}
	else {
		dumper.print(dumper.isVerbose()? ".[f]" : ".");
		dumper.print(name);
	}
}
void AssignmentExpression::dumpImplementation(Dumper& dumper) const {
	object->dump(dumper);
	dumper.print(" = ");
	value->dump(dumper);
}
void ReturnExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("return ");
	expression->dump(dumper);
}
void ControlFlowExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print((isContinue() ? "continue" : (isBreak() ? "break" : "fallthrough")));
}
void ThrowExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("throw ");
	expression->dump(dumper);
}
void PointerOperation::dumpImplementation(Dumper& dumper) const {
	dumper.print(isAddress()?"& (":"* (");
	expression->dump(dumper);
	dumper.print(")");
}
void IfExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("if(");
	condition->dump(dumper);
	dumper.print(") ");
	consequence->dump(dumper);
	dumper.print(" else ");
	alternative->dump(dumper);
}
void LoopExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("loop");
	body->dump(dumper);
}
void CastExpression::dumpImplementation(Dumper& dumper) const {
	object->dump(dumper);
	dumper.print(" as ");
	type->dump(dumper);
}
void BlockExpression::dumpImplementation(Dumper& dumper) const {
	dumper.print("{\n");
	dumper.incIndentation();
	dumper.printIndentation();
	for(auto i = begin();i!=end();++i){
		(*i)->dump(dumper);
		if(i+1 != end()){
			dumper.print("\n");
			dumper.printIndentation();
		}
	}
	dumper.print("\n");
	dumper.decIndentation();
	dumper.printIndentation();
	dumper.print("} ");
}
void ScopedCommand::dumpImplementation(Dumper& dumper) const {
	if(isWhere()){
		dumper.print("where ");
		for(auto i = parameters.begin();i!=parameters.end();++i){
			dumper.print(i->first);
			dumper.print(" is ");
			i->second->dump(dumper);
			if((i+1) != parameters.end()) dumper.print(", ");
		}
	}
	else if(isVisibilityMode(data::ast::PRIVATE)) dumper.print("private");
	else dumper.print("public");
	if(child){
		dumper.print(" ");child->dump(dumper);
	} 
	else dumper.print(":");
}
void ExpressionVerifier::dumpImplementation(Dumper& dumper) const {
	expression->dump(dumper);
}
void UnresolvedSymbol::dumpImplementation(Dumper& dumper) const {
	dumper.print(symbol);
	if(dumper.isVerbose() && explicitLookupScope) dumper.print("[scp]");
}
void AccessExpression::dumpImplementation(Dumper& dumper) const {
	object->dump(dumper);
	dumper.print(dumper.isVerbose()? ".[u]" : ".");
	dumper.print(symbol);
}
void MatchResolver::dumpImplementation(Dumper& dumper) const {
	dumper.print("match(");
	object->dump(dumper);
	dumper.print(")");
}

void Variable::dumpImplementation(Dumper& dumper) const {
	dumper.print(isFlagSet(IS_IMMUTABLE)? "def " : "var ");
	if(dumper.refDeclPointers()) dumper.print(format("[%d] ",(void*)this));
	if(label().isNull()) dumper.print(format("unnamed%d",(void*)this));
	else dumper.print(label());
	dumper.print(" ");
	if(!(type.isPattern() && type.pattern == nullptr)) type.dump(dumper);
}
void Function::dumpImplementation(Dumper& dumper) const {
	dumpDeclaration(dumper);
	dumper.print(" ");
	body.dump(dumper);
}
void PrefixMacro::dumpImplementation(Dumper& dumper) const {
	dumper.print("macro ");
	dumper.print(label());
	dumper.print(" ");
	function->body.dump(dumper);
}
void InfixMacro::dumpImplementation(Dumper& dumper) const {
	dumper.print(format("macro(infix %d) ",stickiness));
	dumper.print(label());
	dumper.print(" ");
	function->body.dump(dumper);
}
void TypeDeclaration::dumpImplementation(Dumper& dumper) const {
	auto type = this->type();
	if(auto record = type->asRecord()){
		dumper.print("type ");
		dumper.print(label());
		dumper.print(" {\n");
		dumper.incIndentation();
		for(auto i = record->fields.begin();i!=record->fields.end();i++){
			dumper.printIndentation();
			dumper.print("var ");
			dumper.print((*i).name);
			dumper.print(" ");
			(*i).type.dump(dumper);
			dumper.print("\n");
		}
		dumper.decIndentation();
		dumper.printIndentation();
		dumper.print("} ");
	}
	else if(auto variant = type->asVariant()){
		dumper.print("variant ");
		dumper.print(label());
	}
	else {
		dumper.print("concept ");
		dumper.print(label());
	}
}
void Function::dumpDeclaration(Dumper& dumper) const{
	dumper.print(isFlagSet(MACRO_FUNCTION)? "macro " : "def ");
	dumper.print(label());
	dumper.print("(");
	for(auto i = arguments.begin();i!= arguments.end(); ++i){
		if(i != arguments.begin()) dumper.print(", ");
		auto arg = *i;
		dumper.print(arg->label());
		if(arg->isVararg()) dumper.print("..");
		dumper.print(" ");
		if(!(arg->type.isPattern() && arg->type.pattern == nullptr)) arg->type.dump(dumper);
		if(auto defaultValue = arg->defaultValue()){
			dumper.print(" = ");
		}
	}
	dumper.print(") ");
	if(!(_returnType.isPattern() && _returnType.pattern == nullptr)) _returnType.dump(dumper);
}

void TypePatternUnresolvedExpression::dump(Dumper& dumper) const {
	if(isResolved()) type()->dump(dumper);
	else if(isPattern()){
		if(!pattern) dumper.print("_");
		else pattern->dump(dumper);
	} 
	else unresolvedExpression->dump(dumper);
}
void Type::dump(Dumper& dumper) const {
	const char* str = nullptr;
	switch(type){
		case Type::VOID: str = "Nothing"; break;
		case Type::TYPE: str = "Type"; break;
		case Type::BOOL: str = "bool"; break;
		case Type::RECORD:  str = static_cast<const Record*>(this)->declaration->label().ptr(); break;
		case Type::VARIANT: str = static_cast<const Variant*>(this)->declaration->label().ptr(); break;
		case Type::INTEGER: dumper.print(format("%s%d",(bits<0?"int":"uint"),bits<0? -bits:bits)); break;
		case Type::FLOAT: str = bits == 32? "float":"double"; break;
		case Type::CHAR:  dumper.print(format("char%d",bits)); break;
		case Type::NATURAL: str ="natural"; break;
		case Type::UINTPTRT: str ="uintptr"; break;

		case Type::POINTER: 
			dumper.print("*");
			next()->dump(dumper); 
			break;
		case Type::REFERENCE:
			dumper.print("Reference(");
			next()->dump(dumper); 
			dumper.print(")");
			break;
		case Type::LINEAR_SEQUENCE:
			dumper.print("LinearSequence(");
			next()->dump(dumper); 
			dumper.print(")");
			break;
		case Type::STATIC_ARRAY:
			dumper.print("Array(");
			next()->dump(dumper); 
			dumper.print(format(",%d)",static_cast<const StaticArray*>(this)->length()));
			break;
		case Type::FUNCTION_POINTER: 
			dumper.print("FunctionPointer(");
			next()->dump(dumper); 
			dumper.print(",");
			static_cast<const FunctionPointer*>(this)->returns()->dump(dumper);
			dumper.print(")");
			break;

		case Type::NODE:
			dumper.print("ast.Expression");
			if(nodeSubtype != -1) dumper.print(format("(%d)",nodeSubtype)); 
			break;
		case Type::ANONYMOUS_RECORD:
		case Type::ANONYMOUS_VARIANT:
			{
			auto sep = type == Type::ANONYMOUS_RECORD? ", ":"| ";
			auto aggr = static_cast<const AnonymousAggregate*>(this);
			if(aggr->isFlagSet(AnonymousAggregate::GEN_REWRITE_AS_VECTOR)){
				dumper.print("Vector(");
				aggr->types[0]->dump(dumper);
				dumper.print(format(",%d)",aggr->numberOfFields));
			} else {
				dumper.print("(");
				for(size_t i =0;i<aggr->numberOfFields;i++){
					if(i) dumper.print(sep);
					if(aggr->fields && !aggr->fields[i].isNull()){ dumper.print(aggr->fields[i]); dumper.print(": "); };
					aggr->types[i]->dump(dumper);
				}
				dumper.print(")");
			}
			}
			break;
		case Type::VARIANT_OPTION:
			str = static_cast<const VariantOption*>(this)->declaration->label().ptr(); break;

		case Type::TRAIT:
			str = static_cast<const Trait*>(this)->declaration->label().ptr(); break;

		case Type::QUALIFIER:
			if(hasConstQualifier()) dumper.print("const ");
			else assert(false && "Invalid type qualifier");
			next()->dump(dumper);
			break;

	}
	if(str) dumper.print(str);
}

std::ostream& operator<< (std::ostream& stream,TypePatternUnresolvedExpression& type){
	auto con = Dumper::console();
	type.dump(con);
	return stream;
}

std::ostream& operator<< (std::ostream& stream,Type* type){
	auto con = Dumper::console();
	type->dump(con);
	return stream;
}

std::ostream& operator<< (std::ostream& stream,Node* node){
	if(compiler::reportLevel >= compiler::ReportDebug){
		auto con = Dumper::console();
		con.flags |= Dumper::VERBOSE | Dumper::REF_DECL_POINTERS;
		node->dump(con);
	}
	return stream;
}
