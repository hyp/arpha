cmake_minimum_required (VERSION 2.6)
project (arpha)
add_executable(arpha src/base/base.cpp src/base/format.cpp src/base/symbol.cpp src/base/location.cpp src/base/memory.cpp src/base/system.cpp src/base/utf.cpp src/syntax/token.cpp src/syntax/lexer.cpp src/syntax/ast.cpp src/main.cpp src/scope.cpp src/declarations.cpp src/parser.cpp src/evaluate.cpp src/interpreter.cpp src/backend/c.cpp)