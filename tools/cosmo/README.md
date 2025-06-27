# Cosmopolitan Libc Integration

This folder demonstrates using [Cosmopolitan Libc](https://justine.lol/cosmopolitan/) from Go. It mirrors `tools/tcc` but relies on the `cosmocc` compiler provided by the Cosmo toolchain.

## 1. Install Cosmo

`tools.go` provides a helper that downloads `cosmocc` when missing. From the repository root run:

```bash
go run ./tools/cosmo/tools.go
```

This attempts to fetch `cosmocc` with `curl` or `wget`. Once available, `make run` executes the example and `make test` runs the tests with Cosmo enabled.

## 2. Compile and run C snippets

The library exposes helpers to compile C code using `cosmocc`. `CompileAndRun` writes the program to a temporary file, builds a static binary and executes it.

## 3. `mochi-cosmo`

The command `mochi-cosmo` compiles a `.mochi` source file to C using the built‑in C backend and then uses `cosmocc` to produce a static executable:

```bash
go build -tags cosmo -o mochi-cosmo ./cmd/mochi-cosmo
./mochi-cosmo hello.mochi hello
```

## 4. Tests

Run `go test -tags cosmo` in this directory once `cosmocc` is installed to verify the Cosmo integration.
