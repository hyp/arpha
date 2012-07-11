/**
* This module implements name mangling.
*/
#ifndef ARPHA_GEN_MANGLER_H
#define ARPHA_GEN_MANGLER_H

namespace gen {

	struct Mangler {

		struct Element {
			Element(Mangler* mangler) { this->mangler = mangler; }
		
			void mangle(Function* function);
			void mangle(Variable* variable);
			void mangle(TypeDeclaration* type);

			std::stringstream stream;
		private:
			void mangle(Type* type);
			void mangleModule(BlockExpression* root);
			void mangleComponents(Node* node);
			Mangler* mangler;
		};
	};
}

#endif
