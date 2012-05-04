#ifndef ARPHA_H
#define ARPHA_H

namespace arpha {

	namespace Precedence {
		enum {
			Assignment = 10, // =
			Tuple = 15, // ,
			Call = 90, //()
			Access = 100, //.
		};
	}

	void test();
	void defineCoreSyntax(Scope* scope);

	//core functions

	//core types


};

#endif