# Cosmopolitan Libc Integration

This package demonstrates how to use [Cosmopolitan Libc](https://justine.lol/cosmopolitan/) from Go.
It mirrors the TinyCC example under `tools/tcc` but links against the `cosmocc` static library to produce
portable binaries without needing an external compiler.

## Installing the library

`libcosmo.a` is required at build time. Place it in `tools/cosmo/cosmo` or set
`COSMO_LIB` to its path. The helper `EnsureCosmo` checks this location and will
download the latest [Cosmopolitan](https://github.com/jart/cosmopolitan)
`cosmocc` archive from GitHub if the library is missing.

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
