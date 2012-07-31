//Intrinsic type expressions
#ifndef APRHA_INTRINSICS_TYPES_H
#define APRHA_INTRINSICS_TYPES_H

namespace intrinsics {
	namespace types {

		extern ::Type *Void,*Type;

		extern ::Type *NodePointer;

		extern Function *StaticArrayTypeGenerator;

		//Boots up the type system by defining the void,type and literal types
		void startup();


		//integers
		extern ::Type* boolean;
		extern ::Type* int8;
		extern ::Type* int16;
		extern ::Type* int32;
		extern ::Type* int64;
		extern ::Type* uint8;
		extern ::Type* uint16;
		extern ::Type* uint32;
		extern ::Type* uint64;
		extern ::Type* natural;



		//
		void preinit(Scope* moduleScope);
	};
};

#endif
