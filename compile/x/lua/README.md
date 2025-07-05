# Lua Backend

This backend compiles Mochi programs to plain Lua source so that examples can run with a stock Lua interpreter.

## Architecture

- `compiler.go` walks the AST twice: first to emit function and test declarations and then the main body.
- `infer.go` performs simple type inference so loops and built-ins map to idiomatic Lua.
- Helper routines live in `runtime.go` and are injected only when referenced.
- Test blocks become Lua functions and are invoked after the main program.
- `tools.go` ensures a Lua interpreter is available when running the test suite.

## Supported Features

- Variable declarations, assignments and expressions
- Conditionals (`if`/`else`) and loops (`for`/`while`)
- Local and global function definitions plus anonymous closures
- Lists and maps with indexing and slicing
- Query expressions with cross joins and basic clauses (`where`, `sort`, `skip`, `take`)
- Common built-in functions like `print`, `input`, `count`, `avg` and `str`
- Dataset `load` and `save` for JSON, JSONL, YAML and CSV
- Test blocks compiled and executed automatically

## Unsupported Features

- Regular expression helpers beyond `match`
- Mutating lists while iterating (`insert`, `remove`)
- Joins with `left`, `right` or `outer` sides
- Logic programming constructs and logic queries
- Foreign function interface and interaction with external objects
- `try`/`catch` error handling
- Asynchronous functions (`async`/`await`)
- Set collections (`set<T>`) aside from list-based operations
- Model or stream declarations
- Methods inside `type` blocks
- Reflection, macros and generic type parameters
- Concurrency primitives such as `spawn`, `stream`, `agent`, `on`/`emit` and `intent`
- Package declarations are ignored; module imports use `require`
- Some LeetCode solutions (`6`, `10`, `23`, `27`) currently fail when compiled to Lua

## Lua to Mochi Conversion

The `tools/any2mochi` package can also convert Lua source files into minimal
Mochi stubs using the Lua language server.

### Supported

- Top-level functions and methods with parameter and return types
- Top-level variables with types inferred from hover information
- Simple `---@class` tables emitted as `type` declarations
- Basic function bodies including assignments, conditionals and loops
- Local function declarations
- Table literals and indexing expressions
- `repeat` loops and `break` statements

### Unsupported

- Advanced metatable or module constructs
- Projects spanning multiple files
