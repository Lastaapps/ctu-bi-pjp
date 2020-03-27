# Semestral Work

- CMakeLists.txt - CMake source file
- main.hpp - main function definition
- Lexan.hpp, Lexan.cpp - Lexan related sources
- Parser.hpp, Parser.cpp - Parser related sources
- fce.c - grue for write, writeln, read function, it is compliled together with the program
- samples - directory with samples desribing syntax
- mila - wrapper script for your compiler
- test - test script with comiples all samples

## Literature ##

LLVM supplies a set of tutorials which is available here: https://llvm.org/docs/tutorial/

## Dependencies ##

LLVM including headers. Bases on your OS distribution, it would be usually packages like: `llvm`, `llvm-dev`. For downloading this repository and building it: `git`, `cmake`, `clang`.
For Ubuntu or Debian based OS use:
```
sudo apt install llvm llvm-dev clang cmake git
```

## Build ##

```
mkdir build &&
cd build &&
cmake ..
make
```
**NOTE:** If you by any change encounter **yaml-bench** error with llvm-9, follow this guide: https://weliveindetail.github.io/blog/post/2019/12/02/apt-llvm-9-dev-yaml-bench.html .
Sufficient workaround should be ``touch /usr/lib/llvm-9/bin/yaml-bench``

**To rebuild:**
```
cd build &&
make
```

## Test samples ##

```
./test
```

## Compile a program ##
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

## Compiler requirements ##
Compiler process source code supplied on the stdin and produces LLVM ir on its stdout.
All errors are should be written to the stdin, non zero return code should be return in case of error.
No arguments are required, but the mila wrapper is prepared for -v/--verbose, -d/--debug options which can be passed to the compiler.
Other arguments can be also added for various purposes.

## Template status ##
Regardless of the source code supplied, all produced binaries gives "Answer to the Ultimate Question of Life, the Universe, and Everything":
```
42
```

Generated intermediate code looks like this:
```
; ModuleID = 'sfe'
source_filename = "sfe"

declare i32 @writeln(i32)

define i32 @main() {
entry:
  %0 = call i32 @writeln(i32 42)
  ret i32 0
}
```
