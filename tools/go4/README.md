# go4

`go4` is a minimal interpreter for a tiny subset of Go inspired by the [c4](https://github.com/rswier/c4) self compiler. It tokenizes and compiles programs into a small SSA form executed by a register based virtual machine. The interpreter is capable of running its own source code.

## Building

Run `go build` from this directory to produce a standalone binary:

```bash
go build -o go4 go4.go
```

## Usage

Execute a Go file directly with the interpreter:

```bash
./go4 hello.go
```

The `-s` flag prints the generated SSA instructions instead of running the program. Instructions are formatted in a small assembly-like style:

```
main:
    MOV R0, "hello"
    PRINT R0
```

```bash
./go4 -s hello.go
```

When multiple files are supplied the output of each stage becomes the input
for the next. This allows building new programs with earlier ones. For example,
running the interpreter with itself and then `hello.go` first interprets
`go4.go` and then feeds its output into interpreting `hello.go`:

```bash
./go4 go4.go hello.go
```

The `hello.go` program prints a greeting using the built-in `println` function.
