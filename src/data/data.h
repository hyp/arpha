/**
* This module abstracts data for various subsystems
*/
#ifndef ARPHA_DATA_H
#define ARPHA_DATA_H

namespace data {

	struct Options {
		const char** packagesPaths;
		size_t       packagesPathsCount;
	};

	namespace ast {
		enum FunctionCallConvention {
			ARPHA,CCALL,STDCALL
		};
	}

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
				OBJECT,        //An .o or .obj file
				ASSEMBLY,      //An .S file
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
