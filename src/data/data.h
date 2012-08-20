/**
* This module abstracts data for various subsystems
*/
#ifndef ARPHA_DATA_H
#define ARPHA_DATA_H

#define DATA_STAT_COLLECT_STATISTICS 1

namespace data {

	struct Options {
		const char** packagesPaths;
		size_t       packagesPathsCount;
	};

	namespace ast {

		enum VisibilityMode {
			PUBLIC,PRIVATE
		};

		namespace Function {
			enum CallConvention {
				ARPHA,COLD,CCALL,STDCALL
			};
			enum Flags {
				NONTHROW = 0x1,
			};

			//internal flags used only for function declarations
			namespace Internal {
				enum  Flags {
					UNITTEST = 0x100,
					EXTERNAL = 0x200,
					INTRINSIC = 0x400,
					INTRINSIC_RETURNS_PATTERN = 0x800,
					INTRINSIC_OPERATION = 0x1000,
					//Type's fields are hidden behind macro getters.
					MACRO_FIELD_ACCESS = 0x2000,
					//This function generates a type as a result of it's invocation. It allows it to act as a type in type patterns.
					TYPE_TEMPLATE = 0x4000,

					EXTERNAL_DLLIMPORT = 0x8000,
					EXTERNAL_DLLEXPORT = 0x10000,
				};
			};
		}

		namespace Search {
			enum Result {
				NotFound,
				Found,
				FoundMultiple,
				NotAllElementsResolved
			};
		}

		//intrinsic operations
		namespace Operations {
			enum Kind {
				NEGATION,//inverse on booleans

				ADDITION,
				SUBTRACTION,
				MULTIPLICATION,
				DIVISION,
				REMAINDER,

				BIT_NOT,
				BIT_AND,
				BIT_OR,
				BIT_XOR,

				LEFT_SHIFT,
				RIGHT_SHIFT,//Do we need seperate logical right shift?


				EQUALITY_COMPARISON,
				LESS_COMPARISON,
				GREATER_COMPARISON,
				LESS_EQUALS_COMPARISON,
				GREATER_EQUALS_COMPARISON,

				LOGIC_AND,
				LOGIC_OR,

				//math(fp only now)
				MATH_ABS,
				MATH_POW,
				MATH_SQRT,
				MATH_EXP,
				MATH_LOG,

				//trig(fp only)
				TRIG_SIN,
				TRIG_COS,
				TRIG_TAN,
				TRIG_ASIN,
				TRIG_ACOS,
				TRIG_ATAN,
				TRIG_ATAN2,

				//sequences
				LENGTH,
				ELEMENT_GET,
				ELEMENT_SET,
				SEQUENCE_EMPTY,
				SEQUENCE_MOVENEXT,
				SLICE,

				MEMCPY,
				MEMMOVE,
				MEMSET,

				//vectors
				VECTOR_SHUFFLE,

				TYPE_IS,
			};
		}
	}

	//This data represents varios compilation statistics
	namespace stat {
		struct Statistics {
			int functionCalls;
			int externFunctionCalls;
			struct Optimizations {
				int functionCallsInlined;
			};
			Optimizations optimizations;
		};
	};

	namespace gen {

		struct Options {
			int  optimizationLevel;//-1,0,1,2,3
			bool generate;
			bool generateDebugInfo;  
			bool generateRuntimeBoundsChecks;
		};

		struct AbstractTarget {
			enum Platform {
				WINDOWS,
				WINDOWS_RT,
				WINDOWS_MINGW,
				LINUX,
				MACOSX,
				POSIX,
				OTHER,
			};
			Platform  platform;
			int       platformVersion; //TODO int/2 int/3 int???
			struct TypeSystemState  {
				uint32 pointerSizeof, functionPointerSizeof, sizetSizeof;
			};
			TypeSystemState typeSystemState;
		};

		namespace native {
			struct Target: AbstractTarget {
				enum Arch {
					X86,ARM,
				};
				enum Mode {
					M32,
					M64
				};
				Arch        cpuArchitecture;
				Mode        cpuMode;
				const char* cpuCapabilities; //architecture specific capabilities
			};

			enum ModuleOutputFormat {
				OBJECT   = 0x1, //An .o or .obj file
				ASSEMBLY = 0x2, //An .S file
			};

			enum PackageLinkingFormat {
				EXECUTABLE,    //A .exe file
				LIBRARY,       //A .lib/.a  file
				SHARED_LIBRARY //A .dll/.so file
			};
		}

		namespace toC {
			struct Target: AbstractTarget {
			};
		}
	}


}

#endif
