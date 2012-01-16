#ifndef AST_H
#define AST_H

struct Node {
	int __type;
	Location location;

	template< typename T >
	inline const T* is(){
		return (T*) ( __type == T::__value__ ? this : nullptr );
	}

};

#define EXPR(name,id) struct name : public Node { enum { __value__ = id }; inline name() { __type = id; } public

//a constant value
EXPR(ConstantExpression,0):
	union {
		int64   i64;
		uint64  u64;
		double  f64;
	};
	Type* type;
	bool _isLiteral;

	inline const bool isLiteral() const { return _isLiteral; }
};

EXPR(TypeExpression,1):
	Type* type;
};

EXPR(VariableExpression,2):
	Variable* variable;
};

EXPR(FunctionExpression,3):
	Function* function;
};

EXPR(OverloadSetExpression,4):
	Scope* scope;
	SymbolID symbol;
};

EXPR(TupleExpression,5):
	std::vector<Node*> children;
	Type* type;

};

EXPR(CallExpression,6):
	Node* object;
	Node* arg;
};

struct AccessExpression;

struct VariableOrField : Node {
	union {
		VariableExpression* v;
		AccessExpression* f;
	};
	int type;
};

EXPR(AccessExpression,7):
	Node* object;
	SymbolID symbol;
	Variable* field;
};

EXPR(AssignmentExpression,8):
	VariableOrField object;
	Node* value;
};

EXPR(ReturnExpression,9):
	Node* value;
	Scope* currentBlock;
};

EXPR(BlockExpression,10):
	std::vector<Node*> children;
};

#undef EXPR

struct ExpressionFactory {
	void* allocate(size_t size);

	ConstantExpression* makeConstant();
	ConstantExpression* makeCompilerNothing();
	ConstantExpression* makeError();
	TupleExpression* makeUnit(); //unit is a tuple with 0 elements and type arpha::Nothing
	TupleExpression* makeTuple(Node* a,Node* b);

	VariableExpression* makeVariable(Variable* var);
	TypeExpression* makeType(Type* type);
	FunctionExpression* makeFunction(Function* func);
	AccessExpression* makeAccess(Node* object,SymbolID symbol);

	OverloadSetExpression* makeOverloadSet(Scope* scope,SymbolID symbol);
	CallExpression* makeCall(Node* object,Node* argument);
	BlockExpression* makeBlock();

	Node* duplicate(const Node* node);
};

//expression always returns something
Type* returnType(const Node* node);

//prints ast dump
std::ostream& operator<< (std::ostream& stream,const Node* node);

//compares 2 asts
bool isEqual(const Node* a,const Node* b);

#endif