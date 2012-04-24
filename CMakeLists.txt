cmake_minimum_required (VERSION 2.6)
project (arpha)
add_executable(arpha src/base/base.cpp src/base/format.cpp src/base/symbol.cpp src/main.cpp src/scope.cpp src/declarations.cpp src/ast.cpp src/parser.cpp src/evaluate.cpp src/interpreter.cpp)