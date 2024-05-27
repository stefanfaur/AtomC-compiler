# AtomC Compiler

The AtomC Compiler is a work-in-progress compiler that I am building for my Compilation Techniques course. 
It currently implements some compiler features for the AtomC programming language, a small subset of C.

## Features so far

- **Lexical Analyzer:** Tokenizes the input source code into a series of tokens.
- **Syntactical Analyzer:** Parses tokens to ensure they follow the AtomC language syntax. Absence of output signifies the source file is free of syntactical errors.
- **Domain Analyzer:** Checks for domain errors in the source code, such as undeclared variables/functions, or redeclaration of variables.
- **Type Analyzer:** Analyzes and verifies the types of variables and expressions to ensure they are used correctly according to the language rules.
- **Stack-Based Virtual Machine:** A stack-based virtual machine to execute the generated code. You can test this feature using `./acc --vmtest`.
- **Code Generation:** Transpiles an AtomC file to virtual machine code and runs it.

## Example

<img width="501" alt="Screenshot 2024-03-31 at 14 47 15" src="https://github.com/stefanfaur/AtomC-compiler/assets/45326397/f620626e-5a0a-4ccc-9e64-b944cbd7f575">

## Compilation

The *compiler can be compiled* using make with the following command:

```bash
make
```

## Usage

If you don't want to compile it yourself, you can also get the compiler executable for Linux, macOS, and Windows from the [releases page](https://github.com/stefanfaur/AtomC-compiler/releases). 

To compile an AtomC source file, use:

```bash
./acc <filename>
```

To run the stack-based virtual machine test, use:

```bash
./acc --vmtest
```

To test the compiler, you can run the test suite with:

```bash
make test
```

This runs all files in the `tests/` directory through the compiler, you can add whatever tests you like in there.

Introduce syntax errors, for example, into `tests/testparser.c` to see how the compiler responds.
If you find that I'm missing an error, or something could be clearer, feel free to raise an issue!

You can do cleanup with:

```bash
make clean
```
