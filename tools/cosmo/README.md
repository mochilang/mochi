# Cosmopolitan Libc Integration

This package demonstrates how to use [Cosmopolitan Libc](https://justine.lol/cosmopolitan/) from Go.
It mirrors the TinyCC example under `tools/tcc` but links against the `cosmocc` static library to produce
portable binaries without needing an external compiler.

## Installing the library

`cosmocc` and `libcosmo.a` are needed to build programs. They are installed to
`tools/cosmo/cosmo` by the `EnsureCosmo` helper or when running `make ensure`.
You can override the location by setting the `COSMO_DIR` environment variable.

```go
// EnsureCosmo verifies that the static library is available at build time.
func EnsureCosmo() error { /* ... */ }
```

Once installed you can run the example with:

```bash
make run
```

Build the `mochi-cosmo` command with:

```bash
make build
```

which outputs `bin/mochi-cosmo` in the repository root.

and execute the tests with:

```bash
make test
```

## `mochi-cosmo`

The command `mochi-cosmo` works like `mochi-tcc`: it compiles a `.mochi` program to
C and then links it with Cosmopolitan to produce a native executable.

For example:

```bash
make build
./bin/mochi-cosmo ../../examples/v0.1/hello.mochi hello
./hello
# prints "Hello, world"
```
