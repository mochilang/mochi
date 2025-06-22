# Assembly Backend

The assembly backend emits raw x86‑64 or ARM64 instructions instead of relying on a C compiler. It currently supports amd64 on Linux, macOS and Windows, and arm64 on Linux and macOS. The backend is intended mainly for experiments and diagnostics.

## Architecture

`compiler.go` contains a small `Compiler` type that picks an OS/architecture specific routine based on `runtime.GOOS` and `runtime.GOARCH`:

```go
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
    switch runtime.GOARCH {
    case "amd64":
        return c.compileAMD64(prog)
    case "arm64":
        return c.compileARM64(prog)
    default:
        return nil, fmt.Errorf("assembly backend unsupported on %s/%s", runtime.GOOS, runtime.GOARCH)
    }
}
```

Each routine emits AT&T style assembly that calls `printf` from libc. The tests assemble and link the generated file with the system C toolchain.

## Supported features

- `print` statements with a single integer expression
- Integer literals combined with `+`, `-`, `*`, `/` and `%`
- Generation for `amd64` (Linux, macOS, Windows) and `arm64` (Linux, macOS)

## Unsupported features

The assembly backend does not implement the rest of the Mochi language. Missing functionality includes:

- Variables, assignments and function definitions
- Control flow (`if`, `for`, `while`, `break`, `continue`)
- Data structures such as lists, maps, structs and unions
- Pattern matching with `match`
- Dataset queries (`from`, `select`, `join`, `group by`, `sort`, `skip`, `take`)
- Built-in helpers beyond `print`
- Generative AI helpers (`generate`, `model`, `generate embedding`)
- Concurrency primitives (`spawn`, channels, agents and streams)
- Foreign function interface (`import`, `extern`) and package declarations
- Error handling with `try`/`catch`
- Asynchronous functions (`async`/`await`)
- Generic type parameters or higher‑order functions
- Reflection or macro facilities

Due to these limitations the backend is only suitable for very small programs consisting solely of integer print statements.
