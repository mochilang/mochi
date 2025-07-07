# MLIR Backend

The MLIR backend converts Mochi programs into [MLIR](https://mlir.llvm.org/) by
reusing the existing C backend. The generated C source is compiled to LLVM IR
using `clang`, and the IR is imported into MLIR with `mlir-translate`. The
resulting `.mlir` file can be lowered to LLVM IR again and linked into a native
binary. `clang` and `mlir-translate` from LLVM must be installed.

## Design

1. `compile/c` is used to emit C code for the input program.
2. `clang -S -emit-llvm` translates the C code to textual LLVM IR (`.ll`).
3. `mlir-translate --import-llvm` converts that LLVM IR into MLIR.
4. Optional: `mlir-translate --mlir-to-llvmir` can lower the MLIR back to LLVM
   IR which is then compiled to a binary with `clang`.

This backend therefore supports the same subset of Mochi that the C backend
implements.

## Supported Features

The following language features are available through the MLIR backend:

- Variable declarations, conditionals, `for` and `while` loops
- Function definitions and calls
- Lists of `int`, `float`, `string` and `list<int>` with slicing support
- Built‑in helpers such as `print`, `len`, `count`, `avg`, `input`, `str`, `now`
  and `json`
- Membership checks and `union`, `except` and `intersect` on lists
- Test blocks and `expect` statements
- `if` expressions for conditional values
- Anonymous `fun` expressions without captured variables
- `extern` variable and function declarations

## Unsupported Features

All limitations of the C backend apply. Notable missing features include:

- `map` types
- Query expressions (`from`/`select`/`sort by`)
- Enum definitions
- Agents, streams and `intent` handlers
- Generative `generate` blocks and model declarations
- Data helpers like `fetch`, `load` and `save`
- Full pattern matching on union variants
- Foreign imports and package modules
- Concurrency primitives such as `spawn` and channels
- Generic types and functions
- Logic programming (`fact`, `rule`, `query`)
- Reflection or macro facilities
- Package exports and extern objects
- Nested list types beyond `list<list<int>>`
- Set literals and set operations
- Methods in `type` blocks
- Multiple return values from functions
- Map membership and iteration
- Variadic and closure‑capturing functions
- Destructuring `let`/`var` bindings
- YAML dataset loading and saving
- Right and outer joins in queries
- Agent initialization with field values
- Nested recursive functions
- `export` statements
- Constructing or matching union values
- Automatic language imports

## Building

```bash
mochi build --target mlir examples/v0.1/hello.mochi -o hello.mlir
```

See the `Makefile` in this directory for a complete example that lowers the
MLIR to LLVM IR, builds the binary and runs it.
