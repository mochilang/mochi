# Compiler Backends

Mochi can translate programs to multiple targets. Stable backends live under
`compile/` while experimental ones reside in `compile/x/<lang>`. Each backend
exposes a `Compiler` type with a `Compile(*parser.Program)` method.

Stable backends:

- `go` – native Go code generation
- `py` – Python source emitter
- `ts` – TypeScript/Deno output

Experimental backends (under `compile/x/`) cover many other languages including
C, Java, Rust and more. Check the directories in `compile/x` for details.

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
