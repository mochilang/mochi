# TinyCC Embedding in Go

This folder demonstrates how to use [TinyCC](https://bellard.org/tcc/) from Go
so that C snippets can be compiled and executed at runtime.

## 1. Install TinyCC

The helper `tools.go` installs TinyCC through the platform package manager. From
the repository root run:

```bash
go run ./tools/tcc/tools.go
```

This attempts to install TinyCC via **apt**, **brew** or **choco/scoop**
depending on the host OS. Once installed `make run` executes the example and
`make test` runs the tests with TinyCC enabled.

## 2. Cross‑platform notes

TinyCC works on a number of platforms but requires separate builds for each
architecture. The table below summarises the current status:

| Platform           | Status | Notes                                             |
|--------------------|-------|----------------------------------------------------|
| Linux x86_64       | ✅    | Solid                                             |
| Linux ARM64        | ✅    | Needs a few patches                                |
| macOS              | ⚠️    | Works but Apple toolchain changes may cause issues |
| Windows            | ⚠️    | MinGW/MSVC builds supported but fragile            |
| FreeBSD/OpenBSD    | 🚫    | Not officially supported                           |
| WASM/Android       | 🚫    | Not supported                                      |

If you need TinyCC for another platform you can cross‑compile it from source. A
static `libtcc.a` for the target platform is usually produced by running:

```bash
# Linux build
./configure --cc=gcc --enable-static
make libtcc.a

# Windows build using MinGW
./configure --cc=x86_64-w64-mingw32-gcc --cpu=x86-64
make libtcc.a
```

## 3. Bundle into one Go binary

Use `go:embed` to ship `libtcc1.a` and any additional runtime files.
Link the static library using cgo directives:

```go
// #cgo LDFLAGS: -ltcc -lm -ldl
```

Building with `-ldflags="-linkmode=external -extldflags=-static"` produces a
fully static binary on Linux.

## 4. Runtime usage

The Go program can compile and execute C on the fly:

```go
code := `
int square(int x) { return x * x; }
`
res, err := CompileAndRun(code, "square", 5) // returns 25
```

See `main.go` for a minimal example that exposes this flow.

## 5. `mochi-tcc`

The command `mochi-tcc` compiles a `.mochi` source file to C using the built‑in
C backend and then uses the embedded TinyCC library to produce a native executable. It
is built from `cmd/mochi-tcc`:

```bash
make mochi-tcc
./mochi-tcc hello.mochi hello
```

## 6. Cross‑compilation

TinyCC itself supports cross‑compiling when configured with a cross compiler
like `x86_64-w64-mingw32-gcc`. On macOS you can build `libtcc.a` and a matching
`tcc` binary for Linux or Windows and use them with `mochi-tcc` to produce
cross‑platform binaries:

```bash
# Build TinyCC targeting Linux
./configure --cc=x86_64-linux-gnu-gcc --cpu=x86-64 --enable-static
make libtcc.a

# Build TinyCC targeting Windows
./configure --cc=x86_64-w64-mingw32-gcc --cpu=x86-64
make libtcc.a
```

The resulting `tcc` binary and `libtcc.a` can be used on macOS to generate
Linux or Windows executables.

## 7. Tools

The tests rely on `EnsureTCC` to check for TinyCC. The helper first tries common
package managers and, if that fails, downloads the TinyCC sources and builds
`libtcc.a` locally:

```go
func EnsureTCC() error {
        if _, err := exec.LookPath("tcc"); err == nil {
                if _, err := findLib(); err == nil {
                        return nil
                }
        }
        // attempt package managers …
        // ...
        if _, err := exec.LookPath("tcc"); err == nil {
                if _, err := findLib(); err == nil {
                        return nil
                }
        }
        fmt.Println("🔧 Building TinyCC from source…")
        return buildFromSource()
}
```
【F:tools/tcc/ensure.go†L17-L167】

## 8. Tests

Run `go test -tags "tcc libtcc"` in this directory once TinyCC is installed to verify the TinyCC integration.
