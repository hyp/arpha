/**
* A translator to c.
*/
#ifndef ARPHA_C_BACKEND_H
#define ARPHA_C_BACKEND_H

#include "../base/base.h"
#include "../ast/node.h"


void translate(std::ostream& output,Node* root);


#endif