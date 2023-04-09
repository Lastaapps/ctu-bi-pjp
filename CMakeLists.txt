cmake_minimum_required(VERSION 3.4.3)
project(mila)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# You can always add more flags without hardcoding them here using the following pattern
# $ CXXFLAGS='-g -fsanitize=address -fsanitize=undefined' cmake <source_dir>
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic ${CMAKE_CXX_FLAGS}")

# Change this to force specific version of LLVM
# find_package(LLVM "${MAJOR}.${MINOR}" REQUIRED CONFIG)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src/)
add_executable(mila src/main.cpp src/Lexer.hpp src/Lexer.cpp src/Parser.hpp src/Parser.cpp)

target_include_directories(mila PRIVATE ${LLVM_INCLUDE_DIRS})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_options(mila PRIVATE ${LLVM_DEFINITIONS_LIST})

# Find the libraries that correspond to the LLVM components that we wish to use and link against them
# https://github.com/llvm/llvm-project/issues/34593
# llvm_map_components_to_libnames(llvm_libs support core irreader)
# target_link_libraries(mila ${llvm_libs})

llvm_config(mila USE_SHARED support core irreader)
