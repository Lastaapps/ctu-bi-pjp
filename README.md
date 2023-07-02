# BI-PJP

This is my solution of a semester project
in subject BI-PJP - Programming languages and compilers
at FIT CTU in Prague in 2022/2023.

The compiler written in Rust compiles the language
Mila (almost Pascal) into llvm bytecode.

Types supported are Integer, Float and String.
All the basic syntactical constructs are supported:
branching, loops, functions, recursion, (global) variables, scopes,
expressions both numerical and logical, I/O using the linked C lib.

## Libraries, tools
- llvm
- Rust
- inkwell

## Building
`cargo build`

## Running
To get llvm bytecode run:

`cargo run [filename]`

`cargo run < filename`

To compile the code to an executable binary, run

`./mila filename`

