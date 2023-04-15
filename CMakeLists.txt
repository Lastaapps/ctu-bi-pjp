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


include(CTest)
if (BUILD_TESTING)
    file(GLOB_RECURSE MILA_SOURCES LIST_DIRECTORIES false CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/samples/*.mila")
    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/tests")

    # compile tests
    foreach(src ${MILA_SOURCES})
        get_filename_component(basename ${src} NAME_WE)
        add_test(NAME "compiler:${basename}" COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/mila" "${src}" "-o" "${CMAKE_CURRENT_BINARY_DIR}/tests/${basename}")
        set_tests_properties("compiler:${basename}" PROPERTIES FIXTURES_SETUP "${basename}")
    endforeach()

    # run tests
    file(GLOB MILA_OUTPUTS LIST_DIRECTORIES false CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/tests/run/*.run[0-9]*.out")
    foreach(out ${MILA_OUTPUTS})
        get_filename_component(outname ${out} NAME)
        get_filename_component(extensionOut ${out} EXT)
        get_filename_component(basename ${out} NAME_WE)
        string(REPLACE "out" "in" extensionIn "${extensionOut}")
        set(inname "${basename}${extensionIn}")

        set(executable ${CMAKE_CURRENT_BINARY_DIR}/tests/${basename})
        set(outfile ${CMAKE_CURRENT_SOURCE_DIR}/tests/run/${outname})
        set(infile  ${CMAKE_CURRENT_SOURCE_DIR}/tests/run/${inname})

        if(EXISTS "${infile}")
            add_test(NAME "run:${outname}" COMMAND
                ${CMAKE_COMMAND}
                -D executable=${executable}
                -D expected=${outfile}
                -D input=${infile}
                -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_test.cmake)
        else()
            add_test(NAME "run:${outname}" COMMAND
                ${CMAKE_COMMAND}
                -D executable=${executable}
                -D expected=${outfile}
                -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/run_test.cmake)
        endif()
        set_tests_properties("run:${outname}" PROPERTIES FIXTURES_REQUIRED "${basename}")
    endforeach()
endif()

