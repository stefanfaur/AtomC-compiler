# AtomC Compiler

The AtomC Compiler is a work-in-progress compiler designed for educational purposes in the Compilation Techniques course. It currently supports lexical and syntactical analysis of the AtomC programming language.

## Features so far

- **Lexical Analyzer:** Tokenizes the input source code into a series of tokens.
- **Syntactical Analyzer:** Parses tokens to ensure they follow the AtomC language syntax. Absence of output signifies the source file is free of syntactical errors.

## Compilation

The *compiler can be compiled* using make with the following command:

```bash
make
```
## Usage

To compile an AtomC source file, use:

```bash
./acc <filename>
```

To test the parser, you can run:

```bash
make test
```

Introduce syntax errors into `tests/testparser.c` to see how the compiler responds.
If you find that I'm missing an error, or something could be clearer, feel free to raise an issue!

You can do cleanup with:

```bash
make clean
```