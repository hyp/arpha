cmake_minimum_required (VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
find_package(LLVM REQUIRED)

include_directories("include")

set(BASE_FILES src/base/base.cpp src/base/bigint.cpp src/base/format.cpp src/base/symbol.cpp src/base/memory.cpp src/base/system.cpp src/base/utf.cpp)
set(LANG_FILES src/syntax/token.cpp src/syntax/lexer.cpp src/syntax/parser.cpp src/syntax/arpha.cpp src/intrinsics/types.cpp src/ast/node.cpp src/ast/declarations.cpp src/ast/resolve.cpp src/ast/analyze.cpp src/ast/interpret.cpp src/ast/scope.cpp src/ast/totext.cpp src/ast/intrinsic_bindings.cpp src/ast/type.cpp)
set(GEN_FILES  src/gen/gen.cpp src/gen/linker.cpp src/gen/mangler.cpp src/gen/llvm/gen.cpp)
set(TEST_FILES src/testing/tests.cpp)

project (arpha)
add_executable(arpha src/main.cpp ${BASE_FILES} ${LANG_FILES} ${TEST_FILES})

# add_executable(arpha src/main.cpp ${BASE_FILES} ${LANG_FILES} ${TEST_FILES} ${GEN_FILES})
# target_link_libraries(arpha ${LLVM_LIBRARIES})
