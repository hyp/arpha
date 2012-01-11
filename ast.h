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

EXPR(ConstantExpression,0):
	uint64 uinteger;
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
	SymbolID name;
};

EXPR(TupleExpression,5):
	std::vector<Node*> children;
	Type* type;

};

EXPR(CallExpression,6):
	Node* object;
	Node* arg;
};

struct FieldAccessExpression;

struct VariableOrField : Node {
	union {
		VariableExpression* v;
		FieldAccessExpression* f;
	};
	int type;
};

EXPR(FieldAccessExpression,7):
	VariableOrField object;
	Type::Field* field;
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

#define EXPRESSION_TYPELIST(f) f(ConstantExpression) f(TypeExpression) f(VariableExpression) f(FunctionExpression) f(OverloadSetExpression) f(TupleExpression) f(CallExpression) \
	f(FieldAccessExpression) f(AssignmentExpression) f(ReturnExpression) f(BlockExpression)


#undef EXPR

//expression always returns something
Type* returnType(const Node* node);

//prints ast dump
std::ostream& operator<< (std::ostream& stream,const Node* node);

//compares 2 asts
bool isEqual(const Node* a,const Node* b);

#endif