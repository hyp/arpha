#ifndef DECLARATIONS_H
#define DECLARATIONS_H

struct Node;
struct Parser;

struct Substitute : PrefixDefinition {
	Substitute(SymbolID name,Location& location) : PrefixDefinition(name,location) {}
	
	Node* parse(Parser* parser);

	Node* expression;
};


#endif