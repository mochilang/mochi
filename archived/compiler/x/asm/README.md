# Assembly Backend

The assembly backend uses the existing C backend to generate C source code and
then invokes the system C compiler with the `-S` flag. The produced assembly is
whatever syntax the host toolchain emits (typically AT&T style for GCC/Clang).

## Architecture

`compiler.go` wraps the C backend. During compilation the program is first
translated to C, written to a temporary file and compiled to assembly using the
host C compiler. The resulting `.s` file is returned to the caller.

Because the heavy lifting is delegated to the C backend, this assembly backend
supports the full Mochi language on any platform where a C compiler is
available.
