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
	extern Type* type;
	extern Type* expression;
	extern Type* Nothing,*Unresolved,*inferred;
	extern Type *constant;
	extern Type *int8,*uint8,*int16,*uint16,*int32,*uint32,*int64,*uint64;
	extern Type *float64,*float32;
	//String literal - constant array of 8 bit charcaters utf8 encoded
	extern Type *constantString; 

	extern Type *boolean;	

	inline bool isReal(Type* type){
		return type == float32 || type == float64;
	}
	inline bool isInteger(Type* type){
		return type==int32 || type==uint32 || type == int64|| type == uint64 || type==int16 || type==uint16 || type==int8 || type==uint8;
	}
	inline bool isSignedInteger(Type* type){
		return type==int32 || type == int64 ||  type==int16 ||  type==int8;
	}
	inline bool isAssignableFromConstant(Type* type){
		return type==int32 || type==uint32 || type == int64 || type == uint64 || type==int16 || type==uint16 || type==int8 || type==uint8 || type == float32 || type ==float64 || type==boolean;
	}
};

#endif