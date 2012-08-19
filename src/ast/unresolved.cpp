/**
* This module performs error reporting for nodes which weren't resolved.
*/
#include "../compiler.h"
#include "../base/system.h"
#include "scope.h"
#include "node.h"
#include "declarations.h"
#include "resolve.h"

void Resolver::reportUnresolvedNodes(Node* root){
	_reportUnresolved = true;

	compiler::headError(root->location(),format("Can't resolve %s expressions and definitions:",unresolvedExpressions));

	//Perform an extra pass collecting unresolved AST nodes.
	auto reportLevel = compiler::reportLevel;
	compiler::reportLevel = compiler::Silent;
	resolve(root);
	compiler::reportLevel = reportLevel;

	_reportUnresolved = false;
}

bool isTypeError(TypePatternUnresolvedExpression& type){
	if(type.kind == TypePatternUnresolvedExpression::UNRESOLVED){
		return true;
	}
	return false;
}

#ifdef  ERROR
	#undef  ERROR
#endif
#define ERROR(node,...)         compiler::subError(node->location(),format(__VA_ARGS__))
#define ERROR_FIX(node,fix,...) compiler::subError(node->location(),format(__VA_ARGS__))

void Resolver::reportUnresolved(UnresolvedSymbol* unr){
	auto scope = (unr->explicitLookupScope ? unr->explicitLookupScope : currentScope());
	auto correction = scope->lookupBestSimilar(unr->symbol,20);
	if(correction){
		ERROR(unr,"The symbol '%s' is undefined. Perphaps you mean '%s'?",unr->symbol,correction->label());
	} else {
		ERROR(unr,"The symbol '%s' is undefined",unr->symbol);
	}
}
void Resolver::reportUnresolved(CallExpression* call){
	bool argResolved = call->arg->isResolved();

	auto console = Dumper::console();
	if(!call->object){
		if(argResolved && call->isFlagSet(CallExpression::CALL_TO_DESTRUCTOR)){
			ERROR(call,"Can't resolve call '(%s).destroy()'",call->arg);
		}
	}
	else if(auto unr = call->object->asUnresolvedSymbol()) {
		if(argResolved){
			if(call->isFlagSet(CallExpression::DOT_SYNTAX)){
				if(auto tuple = call->arg->asTupleExpression()){
					ERROR(call,"Can't find the matching overload for the call %s.%s(%s)",*tuple->begin(),unr->symbol,call->arg);
				}
				else {
					ERROR(call,"Can't find the matching overload for the call %s.%s()",call->arg,unr->symbol);
				}
			}
			else ERROR(call,"Can't find the matching overload for the call %s(%s)",unr->symbol,call->arg);
		}

		auto scope = (unr->explicitLookupScope ? unr->explicitLookupScope : currentScope());
		
		bool header = false;
		for(overloads::OverloadRange overloads(scope,unr->symbol,call->isFlagSet(CallExpression::DOT_SYNTAX));!overloads.isEmpty();overloads.advance()){
			if(!header){
				if(argResolved) console.print(format("\tThe available overloads for the function '%s' are:",unr->symbol));
				header = true;
			}
			if(argResolved && overloads.currentFunction()->isResolved()){
				console.print("\n\t\t");
				overloads.currentFunction()->dumpDeclaration(console);
			}
		}
		if(!header){
			if(!argResolved){
				ERROR(call,"Invalid call '%s'",unr->symbol);
			}
			console.print(format("\tThere are no available overloads for the function '%s'! Perphaps you've misspelled the function?\n",unr->symbol));
		}
		else console.print("\n\n");
	}
}
void Resolver::reportUnresolved(Function* function){
	if(function->isFieldAccessMacro()) return;

	bool allArgsResolved = true;
	for(auto i = function->arguments.begin();i!=function->arguments.end();i++){
		auto arg = *i;
		if(!arg->isResolved()){
			allArgsResolved = false;
		}
	}

	if(allArgsResolved){
		if(!isTypeError(function->_returnType)){
			if(function->body.isResolved()) ERROR(function,"Can't resolve the function '%s'",function->label());
		}
	}
}

Node* Resolver::reportUnresolvedNode(Node* node){
	auto console = Dumper::console();

	if(auto unr = node->asUnresolvedSymbol()) reportUnresolved(unr);
	else if(auto call = node->asCallExpression()) reportUnresolved(call);
	else if(auto var = node->asVariable()){
		if(!isTypeError(var->type)){
			if(var->type.isEmptyPattern()) ERROR_FIX(node,"Please specify the variable's type","Can't infer a type for the variable '%s'.",var->label());
			else ERROR(node,"Can't resolve the variable '%s'.",var->label());
		}
	}
	else if(auto func = node->asFunction()) reportUnresolved(func);
	else if(auto mt = node->asMatchResolver()){
		if(mt->object->isResolved()) ERROR(node,"Can't resolve 'match(%s)'",mt->object);
	}
	return ErrorExpression::getInstance();
}

#undef  ERROR
