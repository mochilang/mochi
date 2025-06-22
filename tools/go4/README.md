# go4

`go4` is a small wrapper inspired by the original [c4](https://github.com/rswier/c4)
compiler.  It can compile either C or Go sources depending on the file
extension of the first argument.  C files are compiled with `gcc` while Go
files are built using the `go` tool.  Any additional files are executed in
sequence by the program produced from the previous compilation, allowing simple
self‑hosting demonstrations similar to the original project.

Examples:

```bash
# compile and run hello.c
./go4 hello.c

# compile and run hello.go
./go4 hello.go

# print assembly for hello.go
./go4 -s hello.go

# compile go4.go then use the resulting program to build hello.c
./go4 go4.go hello.c

# build two generations of the C compiler before running hello.c
./go4 go4.c go4.c hello.c
```

Use the provided `Makefile` to build the `go4` executable:

```bash
make
```
