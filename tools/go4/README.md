# go4

`go4` is a small wrapper mimicking the command behaviour of the original [c4](https://github.com/rswier/c4) compiler.
It compiles the first provided `.c` file using the system `gcc` compiler and
executes the resulting binary on any remaining arguments. This allows simple
self-hosting demonstrations similar to the original project.

Examples:

```bash
# compile and run hello.c
./go4 hello.c

# print assembly for hello.c
./go4 -s hello.c

# compile c4.c and use the resulting compiler to run hello.c
./go4 c4.c hello.c

# build two generations of the compiler before running hello.c
./go4 c4.c c4.c hello.c
```

Use the provided `Makefile` to build the `go4` executable:

```bash
make
```
