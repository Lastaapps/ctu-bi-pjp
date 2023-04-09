cmake_minimum_required(VERSION 3.4.3)
project(mila)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Change this to force specific version of LLVM
# find_package(LLVM "${MAJOR}.${MINOR}" REQUIRED CONFIG)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_executable(mila src/main.cpp src/Lexer.hpp src/Lexer.cpp src/Parser.hpp src/Parser.cpp)

target_include_directories(mila PRIVATE ${LLVM_INCLUDE_DIRS})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_options(mila PRIVATE ${LLVM_DEFINITIONS_LIST})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

# Link against LLVM libraries
target_link_libraries(mila ${llvm_libs})
