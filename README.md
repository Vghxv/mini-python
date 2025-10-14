# Mini-Python Compiler (OCaml)

This project implements a compiler for a small fragment of the Python language, called **Mini-Python**, targeting **x86-64 assembly**.

-----

## Project Goal

The primary objective of this project is to build a complete compiler pipeline for the Mini-Python language. This involves implementing:

  * **Lexing and Parsing** (Parser provided)
  * **Static Type Checking**
  * **Code Generation** (to x86-64 assembly)

-----

## Supported Language Features

Mini-Python is largely compatible with Python 3 and supports the following features:

  * **Data Types**: Booleans (`True`, `False`), signed 64-bit integers, strings, lists, and `None`.
  * **Statements**: Function definitions (`def`), conditional statements (`if/else`), variable assignment, list element modification (`x[e1] = e2`), printing (`print`), and control flow (`return`, `for...in...`).
  * **Operators**: Arithmetic (`+`, `-` (unary/binary), `*`, `//`, `%`), logical (`and`, `or`, `not`), and comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`).
      * **Operator Overloading**: The `+` operator is overloaded for **int addition**, **string concatenation**, and **list concatenation**.
      * **Boolean Interpretation**: `None`, `False`, `0`, empty strings, and empty lists are interpreted as `false` in Boolean contexts; all other values are `true`.
  * **Built-in Functions**: `len`, `list`, and `range`. The most common use is `list(range(e))`.

### Important Differences from Standard Python

The Mini-Python implementation has a few key differences from full Python 3 semantics:

  * **Arithmetic**: Uses **signed 64-bit** machine arithmetic (unlike Python's unbounded integers).
  * **List Display**: Strings within lists are printed **without** quotes (e.g., `[abc]`).
  * **List Indexing**: Accessing elements with a negative index is **undefined**.

-----

## Prerequisites

To build and run this compiler, you need:

  * **OCaml** development environment (version 2.0 or higher of Dune).
  * **Dune** build system.
  * **GCC** (or a compatible C compiler) to assemble and link the generated x86-64 code.

-----

## Building and Running

The project uses `dune` and a simple `Makefile` for compilation.

### Building the Compiler

Build the executable named `minipython.exe`:

```bash
make
# OR
dune build minipython.exe
```

### Command Line Options

The `minipython.exe` compiler accepts the following options:

| Option | Description | Exit Code on Success |
| :--- | :--- | :--- |
| `--parse-only` | Stops after successfully parsing the input file. | `0` |
| `--type-only` | Stops after successfully performing static type checking. | `0` |
| (None) | Compiles and generates x86-64 assembly code. | `0` |

If no options are passed, the compiler produces an assembly file (`<file.s>`) from the input Python file (`<file.py>`).

### Running Generated Code

1.  **Generate Assembly:**

    ```bash
    ./minipython.exe your_program.py
    ```

    This creates `your_program.s`.

2.  **Compile and Execute:**

    ```bash
    gcc your_program.s -o your_program
    ./your_program
    ```

### Error Reporting

  * **Static Errors** (Lexical, Syntax, or Type Errors) result in exit code **1**. The output should report the location of the error in the format: `file.py:line:column: error message`.
  * **Runtime Errors** result in exit code **1**. The output typically displays a runtime error message (e.g., `Runtime error\n`).

-----

## Testing

The provided test suite can be run using the `tests-mini-python/test` script.

### Running Automated Tests

To run the automated tests, navigate to the `tests-mini-python` directory and use your compiled executable (`minipython.exe`) as an argument.

| Test Category | Command |
| :--- | :--- |
| **Type Checking Tests** | `cd tests-mini-python && ./test -2 ../minipython.exe` |
| **Code Generation Tests** | `cd tests-mini-python && ./test -3 ../minipython.exe` |

The `test` script checks your compiler's exit code for proper static/type error handling and compares the output of the compiled binary against expected `.out` files for execution tests.

Test results are organized by category:

  * `syntax/bad/`: Lexing or parsing must fail.
  * `typing/bad/`: Type checking must fail.
  * `typing/good/`: Type checking must pass.
  * `exec-fail/`: Compiles successfully but fails at runtime.
  * `exec/`: Compiles and executes successfully, and output must match the `.out` file.
