# C# Backend

The C# backend translates Mochi programs to plain C# source files. It aims to remain lightweight while covering the core language. Generated files can be compiled or executed with any .NET toolchain.

## Files
- `compiler.go` – code generator
- `runtime.go` – helper functions injected as needed
- `compiler_test.go` – golden tests
- `tools.go` – installs the `dotnet` CLI for benchmarks

## Supported Features
- Functions, control flow and pattern matching
- Struct, union and model declarations
- Package imports and tests
- Dataset queries compiled to LINQ with filtering, sorting, grouping and basic joins
- Built-in helpers: print, len, now, json, set operations, HTTP fetch, dataset load/save (CSV, JSON, JSONL, TSV, YAML) and simple LLM stubs
- Stream blocks with `on`/`emit`

## Unsupported Features
- Left/right/outer joins
- Combining `group by` with joins, sorting or pagination
- Agent declarations and intent blocks
- Logic programming (`fact`, `rule`, `query`)
- Extern objects and FFI
- Full LLM integration for `_genText` and `_genStruct`
- Concurrency primitives like `spawn` and channels
- `try`/`catch` error handling
- Agent initialization with field values
- The `eval` builtin

## Building
Run `mochi build --target cs main.mochi -o main.cs` and compile with `dotnet run` or your preferred C# compiler.

## Tests
The suite under `tests/compiler/cs` verifies generated code. Execute with `go test ./compile/cs -tags slow`.

## Dotnet Installation
`tools.go` includes an installer invoked by tests and benchmarks to ensure the `dotnet` command is available.
