# Semestral Work

- CMakeLists.txt - CMake source file
- main.hpp - main function definition
- Lexan.hpp, Lexan.cpp - Lexan related sources
- Parser.hpp, Parser.cpp - Parser related sources
- fce.c - grue for write, writeln, read function, it is compliled together with the program
- samples - directory with samples desribing syntax
- mila - wrapper script for your compiler
- test - test script with comiples all samples

## Literature

LLVM supplies a set of tutorials which is available here: https://llvm.org/docs/tutorial/

## Dependencies

LLVM including headers. Bases on your OS distribution, it would be usually packages like: `llvm`, `llvm-dev`. For downloading this repository and building it: `git`, `cmake`, `clang` a `zlib1g-dev`.
For Ubuntu or Debian based OS use:
```
sudo apt install llvm llvm-dev clang git cmake zlib1g-dev
```

### LLVM version

Recommended version is version 9 or 10 (currently latest). Older version may require changes. 

## Build

```
mkdir build &&
cd build &&
cmake ..
make
```

**To rebuild:**
```
cd build &&
make
```
Builded compiler outputs intermediate code from which llvm can generate a binary.

## OS Speficic problems:

### Linux
**NOTE:** If you by any change encounter **yaml-bench** error with llvm-9, follow this guide: https://weliveindetail.github.io/blog/post/2019/12/02/apt-llvm-9-dev-yaml-bench.html .
Sufficient workaround should be ``touch /usr/lib/llvm-9/bin/yaml-bench``

### Windows - WSL

WSL containts only version 6 by default, you need to download newer version from: https://apt.llvm.org/

### Mac

Some standard utilities may be required like `getopts`, `realpah`.

## Test samples
Run from project root. Compiles binary for all example source codes in ``sources/`` directory
```
./test
```

## Compile a program
Use supplied script to compile source code into binary.
```
./mila test.mila -o test
```

**How does mila wrapper script works?**

It runs `build/sfe` on the source code, then `llc` and `clang` (with the fce.c file added):

```
rm -f "$OutputFileBaseName.ir"
> "$OutputFileBaseName.ir" < "$InputFileName" ${DIR}/build/sfe &&
rm -f "$OutputFileBaseName.s"
llc "$OutputFileBaseName.ir" -o "$OutputFileBaseName.s" &&
clang "$OutputFileBaseName.s" "${DIR}/fce.c" -o "$OutputFileName"
```

## Compiler requirements
Compiler processes source code supplied on the stdin and produces LLVM ir on its stdout.
All errors should be written to the stderr, non zero return code should be return in case of error.
No arguments are required, but the mila wrapper is prepared for -v/--verbose, -d/--debug options which can be passed to the compiler.
Other arguments can be also added for various purposes.

## Template status
Regardless of the source code supplied, all produced binaries gives "Answer to the Ultimate Question of Life, the Universe, and Everything":
```
42
```

Generated intermediate code looks like this:
```
; ModuleID = 'mila'
source_filename = "mila"

declare i32 @writeln(i32)

define i32 @main() {
entry:
  %0 = call i32 @writeln(i32 42)
  ret i32 0
}
```
