# `runtime/vm`

A lightweight bytecode virtual machine used for executing a small subset of Mochi programs.  It includes a compiler that converts parsed Mochi ASTs into a sequence of instructions which are then executed by the VM.

## Architecture

* **Compiler** – walks the parsed program and emits instructions.  Each top level function becomes a `Function` containing bytecode instructions and register count.  A `Program` is a collection of these functions.
* **VM** – executes a `Program` using a simple stack of call frames.  Each frame has its own register slice.  Instructions operate on registers and support calls between compiled functions.
* **Disassembler** – converts bytecode back into a human readable listing, used by tests.

## Supported features

The VM supports a small but useful subset of Mochi:

* Integer, float, boolean and string constants
* Arithmetic operations `+`, `-`, `*`, `/`, `%`
* Comparison operators `==`, `!=`, `<`, `>`, `<=`, `>=`
* Variable definitions and reassignment
* `if`, `while` and `for` loops over ranges and lists
* Function definitions and calls with two arguments
* Built‑ins `len` and `print`
* List indexing and construction

## Unsupported features (partial list)

Many of Mochi's features are not yet implemented:

* Short circuit boolean operators `&&` and `||`
* `break` and `continue` within loops
* Structs, pattern matching and user defined types
* Closures or nested function values
* External package imports or FFI calls

This VM is intentionally simple and primarily used for experimentation and testing.

## Running tests

Golden tests ensure the VM stays in sync with the main interpreter. Execute:

```
go test ./tests/vm -run .
```

Use `-update` to refresh the expected output files when modifying the VM.
