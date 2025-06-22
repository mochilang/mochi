# go4

go4 is a tiny interpreter for a very small subset of Go inspired by [c4](https://github.com/rswier/c4).
It can evaluate simple programs written in this subset and is capable of running
its own source file.

## Usage

```bash
# build the interpreter
go build -o go4 go4.go

# run a Go file
./go4 hello.go

# print the generated instructions
./go4 -s hello.go

# self-host experiment
# The first argument may be `go4.go` which is skipped so the next files
# are executed just like running the compiled interpreter.
./go4 go4.go hello.go
./go4 go4.go go4.go hello.go

# compile go4 itself and write the VM instructions to a file
./go4 -s go4.go > go4.s
```

The repository includes a copy of `go4.s` generated from the current source so
the interpreter can demonstrate self-hosting.

The supported syntax covers basic integer expressions, assignments and
`fmt.Println` statements. It compiles this subset to a tiny stack based VM.
The project is intentionally minimal and not a full implementation of the Go
language.
