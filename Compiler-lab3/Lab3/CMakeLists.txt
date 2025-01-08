include(CMakePrintHelpers)

CMAKE_MINIMUM_REQUIRED(VERSION 3.7 FATAL_ERROR)
project(irbuilder)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_compile_options(-g)

# compiler must be 11 or 14
set(CMAKE_CXX_STANDARD 14)

# required if linking to static library
add_definitions(-DANTLR4CPP_STATIC)
add_definitions(-DFULL_IMPL)
# using /MD flag for antlr4_runtime (for Visual C++ compilers only)
set(ANTLR4_WITH_STATIC_CRT OFF)
# add external build for antlrcpp
include(ExternalAntlr4Cpp)
# add antrl4cpp artifacts to project environment
include_directories(${ANTLR4_INCLUDE_DIRS})

# set variable pointing to the antlr tool that supports C++
# this is not required if the jar file can be found under PATH environment
set(ANTLR_EXECUTABLE /usr/local/lib/antlr-4.9.3-complete.jar)
# add macros to generate ANTLR Cpp code from grammar
find_package(ANTLR REQUIRED)
find_package(RapidJSON)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
llvm_map_components_to_libnames(llvm_libs core mcjit native)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

#Call macro to add lexer and grammar to your build dependencies.
antlr_target(SafeCGrammarLexer SafeCLexer.g4 LEXER
             PACKAGE antlrcpp)
antlr_target(SafeCGrammarParser SafeCParser.g4 PARSER
             PACKAGE antlrcpp
             DEPENDS_ANTLR SafeCGrammarLexer
             COMPILE_FLAGS -visitor -no-listener -lib ${ANTLR_SafeCGrammarLexer_OUTPUT_DIR})

# include generated files in project environment
include_directories(${ANTLR_SafeCGrammarLexer_OUTPUT_DIR})
include_directories(${ANTLR_SafeCGrammarParser_OUTPUT_DIR})

# include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include/")
include_directories(include)
include_directories(runtime)

# get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
# foreach(dir ${dirs})
#   message(STATUS "dir='${dir}'")
# endforeach()

# cmake_print_variables(ANTLR_SafeCGrammarLexer_OUTPUT_DIR)
# cmake_print_variables(ANTLR_SafeCGrammarParser_OUTPUT_DIR)
# cmake_print_variables(ANTLR_SafeCGrammarLexer_CXX_OUTPUTS)
# cmake_print_variables(ANTLR_SafeCGrammarParser_CXX_OUTPUTS)

add_executable(optirbuilder main.cpp
            src/AstBuilder.cpp
            src/SafeCIRBuilder.cpp
            src/AstNode_Visitor.cpp
            runtime/runtime.cpp
            runtime/io.cpp
            runtime/runtime.h
            runtime/io.h
            src/Dataflow.cpp
            src/ConstantPropagator.cpp
            ${ANTLR_SafeCGrammarLexer_CXX_OUTPUTS}
            ${ANTLR_SafeCGrammarParser_CXX_OUTPUTS}
               )
target_link_libraries(optirbuilder antlr4_static ${llvm_libs})