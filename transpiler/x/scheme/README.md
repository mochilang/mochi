# Scheme Transpiler

This package implements a very small transpiler that converts a subset of Mochi
into Scheme code run by `chibi-scheme`.

## Supported golden tests (4/100)

- [x] `print_hello.mochi`
- [x] `let_and_print.mochi`
- [x] `binary_precedence.mochi`
- [x] `typed_let.mochi`

The totals correspond to the number of `.mochi` files under `tests/vm/valid`.
