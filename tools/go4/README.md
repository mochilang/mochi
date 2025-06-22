# go4

A minimal self-interpreter inspired by the [c4](https://github.com/rswier/c4) compiler. The `go4` program can tokenize, parse and execute a very small subset of Go.

## Building

```bash
go build -o go4 go4.go
```

## Usage

Run a Go source file (limited to the supported subset):

```bash
./go4 hello.go
```

Show tokens instead of executing:

```bash
./go4 -s hello.go
```

`go4` can interpret its own source incrementally:

```bash
./go4 go4.go hello.go
./go4 go4.go go4.go hello.go
```

`hello.go` is a small example program that prints a greeting.
