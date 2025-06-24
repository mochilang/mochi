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
* Dataset queries (`from`, `join`, `group`, `sort`, `take`, etc.) and
  loading or saving datasets in CSV, JSON, JSONL or YAML formats

## Unsupported features

Many language capabilities are intentionally left out of this
experimental VM.  Unsupported areas include:

* Error handling with `try`/`catch`
* Logic programming constructs (`fact`, `rule`, `query`)
* Set collections (`set<T>`) and related operations
* Generic type parameters for functions and user-defined types
* Reflection and macro facilities
* Concurrency primitives such as `spawn` and channels
* Package declarations and `export` statements
* Asynchronous functions (`async`/`await`)
* Agents, streams and intent blocks with persistent state
* Agent initialization with field values
* Right and outer joins or pagination when joins are used
* Generative AI blocks, model declarations and other LLM helpers
* HTTP `fetch` expressions
* Foreign imports and FFI calls
* The `eval` builtin function
* Test blocks
* Generic methods inside `type` blocks
* Pattern matching on union variants or enums
* Nested recursive functions inside other functions
* Advanced string or list slicing beyond `[start:end]`
* Functions with multiple return values or variadic parameters
* Methods declared inside `type` blocks
* Enum and additional union type declarations
* Extern type declarations
* Increment/decrement operators (`++`, `--`) and compound
  assignments like `+=`
* Python style `range()` loops

This VM is intentionally simple and primarily used for experimentation and testing.

## Running tests

Golden tests ensure the VM stays in sync with the main interpreter. Execute:

```
go test ./tests/vm -run .
```

Use `-update` to refresh the expected output files when modifying the VM.
