
FILE(GLOB tmplibs  ../../ build/lib/Debug/LLVM*.lib)
foreach(lib ${tmplibs})
	list(APPEND LLVM_LIBRARIES ${lib})
endforeach()
