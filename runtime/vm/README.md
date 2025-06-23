# `runtime/vm`

A lightweight bytecode virtual machine used for executing a small subset of Mochi programs.  It includes a compiler that converts parsed Mochi ASTs into a sequence of instructions which are then executed by the VM.

## Architecture

* **Compiler** – walks the parsed program and emits instructions.  Each top level function becomes a `Function` containing bytecode instructions and register count.  A `Program` is a collection of these functions.
* **VM** – executes a `Program` using a simple stack of call frames.  Each frame has its own register slice.  Instructions operate on registers and support calls between compiled functions.
* **Disassembler** – converts bytecode back into a human readable listing, used by tests.

## Supported features

The VM currently handles enough language features to run basic numeric programs:

* Integer constants and lists
* Addition and subtraction
* Comparison using `==`, `<` and `>`
* `if` statements and `for` loops over numeric ranges
* Function definitions and calls with two arguments
* Built‑ins `len` and `print`
* List indexing and list construction

## Unsupported features (partial list)

Many of Mochi's features are not yet implemented:

* Floating point numbers and string operations
* Variable reassignment and mutable variables
* While loops, break/continue statements
* Structs, pattern matching and user defined types
* Closures or nested function values
* External package imports or FFI calls

This VM is intentionally simple and primarily used for experimentation and testing.
