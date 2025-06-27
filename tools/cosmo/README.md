# Cosmopolitan Libc Integration

This package demonstrates how to use [Cosmopolitan Libc](https://justine.lol/cosmopolitan/) from Go.
It mirrors the TinyCC example under `tools/tcc` but relies on the `cosmocc` compiler to produce
portable binaries.

## Installing cosmocc

`cosmocc` is a standalone cross compiler distributed as a prebuilt archive.
The helper `EnsureCosmo` will download the latest release for Linux,
Windows and macOS automatically when needed.
Alternatively you can download it manually from
[the official website](https://justine.lol/cosmopolitan/download.html)
and place the `cosmocc` binary somewhere in your `PATH`.

```go
// EnsureCosmo verifies that the cosmocc compiler is installed.
// It downloads the official archive and extracts the binary when missing.
func EnsureCosmo() error { /* ... */ }
```

Once installed you can run the example with:

```bash
make run
```

and execute the tests with:

```bash
make test
```

## `mochi-cosmo`

The command `mochi-cosmo` works like `mochi-tcc`: it compiles a `.mochi` program to
C and then uses `cosmocc` to build a native executable.
