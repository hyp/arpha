/**
* This module implements intrinsic functionality for package arpha/ast
*/
#ifndef APRHA_INTRINSICS_AST_H
#define APRHA_INTRINSICS_AST_H

namespace intrinsics {
	namespace ast {

		extern TypeExpression* ExprPtr;

		void startup();
		void onMacroScope(Scope* scope);

		void init(Scope* moduleScope);

		//parse.loop(until,separator,handler)
		Node* loop(Node* arg);
		Node* loopFull(Node* arg);
	};
};

#endif