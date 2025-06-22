# Go4 Interpreter

This is a tiny experiment inspired by the `c4` self–interpreting C compiler. The
`go4` program tokenizes a tiny subset of Go and executes simple examples on a
minimal VM.  The code avoids structs and keeps state in slices so the
interpreter itself can be parsed.  It is not a full Go implementation, but it
can parse its own source and dump the VM instructions.

## Building

```
go build -o go4 go4.go
```

## Example

The provided `hello.go` demonstrates variables and prints `hello` followed by `3`:

```
./go4 hello.go
```

You can dump the VM instruction operators using the `-s` flag:

```
./go4 -s hello.go
./go4 -s hello.go > hello.s
./go4 hello.s
```

Multiple files can be specified. They will be executed sequentially. This even
works when the interpreter is asked to run its own source:

```
./go4 go4.go hello.go
./go4 go4.go go4.go hello.go
```

You can also dump the VM instructions of the interpreter itself:
```
./go4 -s go4.go > go4.s
```
The resulting `go4.s` lists all ops from `go4.go`. You can run this file
directly:
```
./go4 go4.s
```
This demonstrates that the interpreter can read its own instruction dump. The
VM understands simple `if` and `for` statements as well as comparisons, but it
still cannot fully compile the interpreter.

Keep in mind that `go4` only understands a very small language subset, so running
it on real Go source will likely fail.

