# Swift Backend

The Swift backend compiles Mochi programs to Swift source code. It covers only a subset of the language and is mainly used for verifying behaviour across targets.

## Building

Generate Swift code and compile it with `swiftc`:

```bash
mochi build --target swift main.mochi -o main.swift
swiftc main.swift -o main
./main
```

`tools.go` provides `EnsureSwift` which installs the Swift toolchain on macOS or Linux when running tests.

## Tests

Golden tests under `tests/compiler/swift` are tagged `slow` because they invoke the Swift toolchain:

```bash
go test ./compile/swift -tags slow
```

`compiler_test.go` compiles the example programs and checks their output.

## Supported Features

- Basic statements: `let`, `var`, assignments and expression statements
- Arithmetic, comparison and boolean operators
- Control flow: `if`, `for`, `while`, `break`, `continue`, `return`
- Functions, closures and recursion
- Struct and union type declarations
- Pattern matching with `match`
- Lists and maps with indexing, slicing, concatenation and membership checks
- List set operations: `union`, `except` and `intersect`
- String concatenation, indexing (including negative indices) and slicing
- Built-in functions: `len`, `count`, `str`, `avg`, `input`
- Basic dataset operations: `load` and `save`

## Unsupported Features

- Advanced dataset queries with grouping, joins, sorting or pagination
- Type inference for empty collections
- Streams, agents and intent handlers
- The `generate` and `fetch` expressions for LLM and HTTP integration
- Package declarations and the foreign function interface
- Native set collections
- Model declarations using `model` blocks
- `fact` and `rule` statements for logic programming
- Import statements
- Concurrency primitives like `spawn` and channels
- The `eval` builtin function
- Error handling with `try`/`catch`
- Reflection and macro facilities
- Extern object declarations and package exports
- Event emission and `on` handlers
- Generic functions
