#include "node.h"
#include "visitor.h"
#include "../compiler.h"
#include "../arpha.h"

struct NodeToString: NodeVisitor {
	std::ostream& stream;
	NodeToString(std::ostream& ostream) : stream(ostream) {}

	Node* visit(ConstantExpression* node){
		if(node->isLiteral()) stream<<'#';
		if(node->type == arpha::uint64)       stream<<node->u64;
		else if(node->type == arpha::int64) stream<<node->i64;
		else if(node->type == arpha::float64) stream<<node->f64;
		else if(node->type == arpha::constantString) stream<<'"'<<node->string<<'"';
		else if(node->type == compiler::type) stream<<"type "<<node->refType->id;
		else if(node->type == compiler::function) stream<<"func "<<node->refFunction->id;
		else if(node->type == compiler::scopeRef) stream<<"scope";
		else if(node->type == compiler::Error)   stream<<"error";
		else if(node->type == compiler::Nothing) stream<<"statement";
		else if(node->type == arpha::Nothing) stream<<"nothing";
		else if(node->type == arpha::boolean) stream<<(node->u64?"true":"false");
		else assert(false);
		return node;
	}
	Node* visit(VariableExpression* node){
		stream<<"variable "<<node->variable->id;
		if(node->variable->value) stream<<" with value "<<node->variable->value;
		return node;
	}
	Node* visit(OverloadSetExpression* node){
		stream<<"overloadset "<<node->symbol;
		return node;
	}
	Node* visit(CallExpression* node){
		stream<<"call "<<node->object<<" with "<<node->arg;
		return node;
	}
	Node* visit(AccessExpression* node){
		stream<<node->object<<" . "<<node->symbol;
		return node;
	}
	Node* visit(AssignmentExpression* node){
		stream<<node->object<<" = "<<node->value;
		return node;
	}
	Node* visit(ReturnExpression* node){
		stream<<"return "<<node->value;
		return node;
	}
	Node* visit(IfExpression* node){
		stream<<"if "<<node->condition<<" then "<<node->consequence;
		if(node->alternative) stream<<" else "<<node->alternative;
		return node;
	}
	Node* visit(TupleExpression* node){
		auto i = node->children.begin();
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
	Node* visit(WhileExpression* node){
		stream<<"while "<<node->condition<<" do "<<node->body;
		return node;
	}
};
std::ostream& operator<< (std::ostream& stream,Node* node){
	stream<<'(';
	NodeToString visitor(stream);
	node->accept(&visitor);
	stream<<"):"<<node->returnType()->id;
	return stream;
}

//Return type of nodes
Type* Node::returnType() const { return compiler::Nothing; }
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
		else if(node->type == arpha::constantString) return arpha::constantString;
		assert(false);
		return nullptr;
}
Type* ConstantExpression::returnType() const {
	return isLiteral() ? literalConstantReturnType(this) : type;
}
Type* VariableExpression::returnType() const {
	return variable->type;
}
Type* TupleExpression::returnType() const {
	assert(type);
	return type;
}
Type* CallExpression::returnType() const {
	if( auto cnst = object->asConstantExpression()){
		if(cnst->type == compiler::function ) return cnst->refFunction->returnType;
	}
	return compiler::Unresolved;
}
Type* AssignmentExpression::returnType() const {
	return object->returnType();
}
Type* IfExpression::returnType() const {
	if(!alternative) return compiler::Nothing;
	return consequence->returnType();
}

//Injects visitor callback and dynamic cast function into a node structure
//Note: only does the definitions, the appropriate implementations are done by traversing NODE_LIST
#define DECLARE_NODE_IMPLEMENTATION(T) \
	Node* T::accept(NodeVisitor* visitor) { \
		return visitor->visit(this);        \
	}										\
	T* T::as##T() { return this; }                        

NODE_LIST(DECLARE_NODE_IMPLEMENTATION)

#undef DECLARE_NODE_IMPLEMENTATION

//Constructors
ConstantExpression* ConstantExpression::create(Type* constantType){
	auto expression = new ConstantExpression;
	expression->type = constantType;
	expression->_isLiteral = false;
	return expression;
}

ConstantExpression* ConstantExpression::createScopeReference(Scope* scope){
	auto e = create(compiler::scopeRef);
	e->refScope = scope;
	return e;
}
ConstantExpression* ConstantExpression::createFunctionReference(Function* func){
	auto e = create(compiler::function);
	e->refFunction = func;
	return e;
}
ConstantExpression* ConstantExpression::createTypeReference(Type* type){
	auto e = create(compiler::type);
	e->refType = type;
	return e;
}
VariableExpression* VariableExpression::create(Variable* variable){
	auto e =new VariableExpression;
	e->variable = variable;
	return e;
}
TupleExpression* TupleExpression::create(){
	auto e = new TupleExpression;
	e->type = nullptr;
	return e;
}
TupleExpression* TupleExpression::create(Node* a,Node* b){
	if( auto aIsTuple = a->asTupleExpression() ){ //TODO, not 0 children?
		aIsTuple->children.push_back(b);
		return aIsTuple;
	}
	auto e = new TupleExpression;
	e->children.push_back(a);
	e->children.push_back(b);
	e->type = nullptr;
	return e;
}
OverloadSetExpression* OverloadSetExpression::create(SymbolID symbol,Scope* scope){
	auto e = new OverloadSetExpression;
	e->scope = scope;
	e->symbol = symbol;
	return e;
}
CallExpression* CallExpression::create(Node* object,Node* argument){
	auto e = new CallExpression;
	e->object = object;
	e->arg = argument;
	return e;
}
AccessExpression* AccessExpression::create(Node* object,SymbolID symbol,Scope* scope){
	auto e = new AccessExpression;
	e->object = object;
	e->symbol = symbol;
	e->scope = scope;
	e->passedFirstEval = false;
	return e;
}
AssignmentExpression* AssignmentExpression::create(Node* object,Node* value){
	auto e = new AssignmentExpression;
	e->object = object;
	e->value = value;
	return e;
}
ReturnExpression* ReturnExpression::create(Node* expression){
	auto e = new ReturnExpression;
	e->value = expression;
	return e;
}
IfExpression* IfExpression::create(Node* condition,Node* consequence,Node* alternative){
	auto e = new IfExpression;
	e->condition = condition;
	e->consequence = consequence;
	e->alternative = alternative;
	return e;
}
BlockExpression* BlockExpression::create(Scope* scope){
	auto e = new BlockExpression;
	e->scope = scope;
	return e;
}
WhileExpression* WhileExpression::create(Node* condition,Node* body){
	auto e = new WhileExpression;
	e->condition = condition;
	e->body = body;
	return e;
}
