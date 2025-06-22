# go4

This directory contains a tiny Go interpreter inspired by the
[c4](https://github.com/rswier/c4) project. The `go4.go` program parses a
very small subset of Go using only the standard library and without relying on
the `go/ast` packages. In addition to `fmt.Println` calls it supports
simple functions, `if` statements and `return` expressions allowing basic
control flow and integer arithmetic.

To run a program:

```bash
go run ./go4.go -- path/to/file.go
```

Running `go4.go` on itself succeeds but prints nothing due to a recursion guard.

A simplified C compiler based on `c4` lives in `go4_c.go`. It is disabled by
default and requires the `ccompiler` build tag:

```bash
go build -tags ccompiler -o go4-c ./go4_c.go
```

## Building

You can build the interpreter binary with the provided `Makefile`:

```bash
make
```

This produces a `go4` executable in the current directory.
