# Fortran Backend

This backend emits straightforward Fortran 90 code for a very small subset of Mochi. It was originally implemented to build the LeetCode "two sum" example and remains intentionally minimal.

## Files

- `compiler.go` – code generator
- `compiler_test.go` – golden tests
- `tools.go` – helper for locating `gfortran`

## Building

Use `mochi build` with the `fortran` target to produce a `.f90` file and then compile it with `gfortran`:

```bash
mochi build --target fortran program.mochi -o program.f90
gfortran program.f90 -o program
./program
```

`tools.go` provides `EnsureFortran` which tries to locate `gfortran` (and install it with `apt-get` or Homebrew when available).

## Supported features

- `let` and `var` declarations for integers, floats, strings and lists
- `if` / `else` statements and simple `if` expressions
- `for` and `while` loops
- basic function definitions and calls
- indexing and slicing (no step parameter)
- arithmetic and comparison operators (`+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `in`)
- logical operators (`&&`, `||`, `!`)
- list operations `union`, `except`, `intersect` for `int`, `float` and `string` lists
- built‑ins: `len`, `count`, `avg`, `append`, `str`, `now`
- `print()` statements

## Unsupported features

The Fortran backend omits many parts of the language:

- map types and membership tests for maps
- map indexing and assignment
- query expressions (`from`, `sort by`, `select`)
- nested function definitions and struct literals
- `union`, `except` and `intersect` on maps and sets
- slice expressions with a step value
- pattern matching with `match`
- agents, streams and logic programming (`fact`, `rule`, `query`)
- foreign imports and helpers like `fetch`, `load` and `save`
- anonymous functions
- built‑ins beyond those listed above (`json`, `input`, `eval`, ...)
- concurrency primitives (`spawn`, channels)
- generative `generate` blocks and model declarations

## Tests

Run the slow golden tests to compile and execute example programs:

```bash
go test ./compile/fortran -tags slow
```
