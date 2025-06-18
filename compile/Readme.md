# Compiler Backends

Mochi can translate programs to multiple targets. Each backend lives in `compile/<lang>` and exposes a `Compiler` type with a `Compile(*parser.Program)` method.

Current directories:

- `c`       – ANSI C code generation
- `cs`      – C# source emitter
- `dart`    – Dart source emitter
- `elixir`  – Elixir helpers
- `erlang`  – Erlang source emitter
- `ex`      – Elixir source emitter
- `go`      – native Go code generation
- `jvm`     – JVM bytecode output
- `kt`      – Kotlin source emitter
- `lua`     – Lua source emitter
- `py`      – Python source emitter
- `rb`      – Ruby source emitter
- `rust`    – Rust source emitter
- `st`      – GNU Smalltalk output
- `swift`   – minimal Swift output
- `ts`      – TypeScript/Deno output
- `wasm`    – WebAssembly using the Go backend

This guide shows how to implement another backend.

## Structure

A backend typically contains these files:

- `compiler.go` – main code generator
- `compiler_test.go` – golden tests
- `helpers.go` – utility helpers
- `infer.go` – optional type inference helpers
- `runtime.go` – injected runtime support

## Steps

1. Create a new folder under `compile/` named for your target.
2. Add a `Compiler` struct and `New` constructor similar to the existing ones.
3. Implement `Compile(*parser.Program)` to walk the AST and emit code.
4. Put any runtime helpers in `runtime.go` so they are included in generated programs.
5. Add tests in `compiler_test.go` that run Mochi sources from `tests/compiler/<lang>` and verify the output using the `golden` package.
6. Update build scripts if external tools are required for running your generated code.

### Tips

- Keep helper functions in `helpers.go` to keep `compiler.go` readable.
- Ensure output is deterministic so tests remain stable.
- Reuse `mochi/types` for type information when generating code.
- Look at the Go, Python and TypeScript backends for patterns you can copy.

A minimal backend only needs `compiler.go` and tests, but splitting out helpers and runtime code makes maintenance easier.
