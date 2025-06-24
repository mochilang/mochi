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
* String concatenation using `+`
* Comparison operators `==`, `!=`, `<`, `>`, `<=`, `>=`
* Membership tests using `in`
* Short circuit boolean operators `&&` and `||`
* Variable definitions using `let` (immutable) and `var` (mutable)
  with optional type annotations and reassignment
* `if`, `while` and `for` loops with `break` and `continue`
* Function definitions
* Function calls with any number of arguments
* Anonymous function expressions
* Built‑ins `len`, `print` (up to two arguments), `append`, `str`, `json`, `now`, `input`, `count`, `avg`, `min` and `max`
* List, map and struct construction
* String indexing to access individual characters
* Field access using the `.` operator
* List and string slicing with `[start:end]` syntax (supports negative indices)
* Pattern matching

## Unsupported features (partial list)

Many of Mochi's features are not yet implemented:

* External package imports or FFI calls

This VM is intentionally simple and primarily used for experimentation and testing.

## Running tests

Golden tests ensure the VM stays in sync with the main interpreter. Execute:

```
go test ./tests/vm -run .
```

Use `-update` to refresh the expected output files when modifying the VM.
