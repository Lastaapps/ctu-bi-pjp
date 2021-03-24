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

A nice explanation of LLVM IR can be found here: https://mukulrathi.co.uk/create-your-own-programming-language/llvm-ir-cpp-api-tutorial/

## Dependencies

LLVM including headers. Bases on your OS distribution, it would be usually packages like:
`llvm`, `llvm-dev`. For downloading this repository and building it: `git`, `cmake`, `clang` and `zlib1g-dev`.
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

## OS speficic problems:

### Linux

#### Known issues

If you by any change encounter **yaml-bench** error with llvm-9, follow this guide: https://weliveindetail.github.io/blog/post/2019/12/02/apt-llvm-9-dev-yaml-bench.html .
Sufficient workaround should be ``touch /usr/lib/llvm-9/bin/yaml-bench``

### Mac OS

Some standard utilities may be required: `getopts` and `realpath`.

#### Detailed guide

Via brew:
```sh
brew install git
brew install cmake
brew install llvm
brew install gnu-getopt
```

Then you will need to update *rc file of your terminal (.bashrc, .zshrc, ...) with following lines:

```sh
# Verify that the version of LLVM is correct!
export LLVM_DIR="/usr/local/Cellar/llvm/10.0.0_1/lib/cmake"
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/Cellar/llvm/10.0.0_1/bin:$PATH"
```


If you dont have utility called __realpath__ you will also need to create it, because it is not in brew:

```sh
sudo echo '#!/bin/sh                                                                              
realpath() {
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
}' > /usr/local/bin/realpath
sudo chmod +x /usr/local/bin/realpath
```

Or:

```
sudo echo '#!/bin/sh    
OURPWD="$PWD"
cd "$(dirname "$1")"
LINK=$(readlink "$(basename "$1")")
while [ "$LINK" ]; do
cd "$(dirname "$LINK")"
LINK=$(readlink "$(basename "$1")")
done
REALPATH="$PWD/$(basename "$1")"
cd "$OURPWD"
echo "$REALPATH"
' > /usr/local/bin/realpath
sudo chmod +x /usr/local/bin/realpath
```


### Windows - WSL 2 guide

WSL containts only version 6 by default, you need to download newer version from: https://apt.llvm.org/

Start with basic dependencies:
```
sudo apt install clang cmake git llvm-10 llvm-10-dev
```

With that everything should be ready for compilation.

#### Known issues

If you get issue during linking:

```
[ 25%] Linking CXX executable mila
/usr/bin/ld: cannot find -lz
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```

You can solve it by installing __zlib1-dev__:

```
sh
sudo apt-get install zlib1g-dev
```

## Build

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
Builded compiler outputs intermediate code from which llvm can generate a binary.

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

It runs `build/mila` on the source code, then `llc` and `clang` (with the fce.c file added):

```
rm -f "$OutputFileBaseName.ir"
> "$OutputFileBaseName.ir" < "$InputFileName" ${DIR}/build/mila &&
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

## Adding your own files example

You want to add `Tree.hpp` and `Tree.cpp`, change `CMakeLists.txt` by adding into `add_executable`:
```
add_executable(mila main.cpp Lexer.hpp Lexer.cpp Parser.hpp Parser.cpp)
```
Result:
```
add_executable(mila main.cpp Lexer.hpp Lexer.cpp Parser.hpp Parser.cpp Tree.hpp Tree.cpp)
```

## Processing input example
You want to print number from the source file, change `Lexan.hpp`:

```
int Lexer::gettok() {
    m_NumVal = 42;
    return tok_number;
}
```
Result:
```
int Lexer::gettok() {
    std::cin >> m_NumVal;
    return tok_number;
}
```
