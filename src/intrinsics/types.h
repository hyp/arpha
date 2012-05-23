//Intrinsic type expressions
#ifndef APRHA_INTRINSICS_TYPES_H
#define APRHA_INTRINSICS_TYPES_H

namespace intrinsics {
	namespace types {

		extern TypeExpression *Void;

		extern TypeExpression *Type,*Expression;

		//integers
		extern TypeExpression* boolean;
		extern TypeExpression* int8;
		extern TypeExpression* int16;
		extern TypeExpression* int32;
		extern TypeExpression* int64;
		extern TypeExpression* uint8;
		extern TypeExpression* uint16;
		extern TypeExpression* uint32;
		extern TypeExpression* uint64;

		void init(Scope* moduleScope);
	};
};

#endif
