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
./go4 go4.go hello.go
./go4 go4.go go4.go hello.go
```

The supported syntax covers basic integer expressions, assignments and
`fmt.Println` statements. It compiles this subset to a tiny stack based VM.
The project is intentionally minimal and not a full implementation of the Go
language.
