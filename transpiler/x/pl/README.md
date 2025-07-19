# Prolog Transpiler

This directory contains a tiny transpiler that converts a restricted subset of Mochi
programs to SWI-Prolog. It is mainly used for experimentation and golden tests.

## Golden tests

Programs under `tests/vm/valid` that have generated Prolog code:

- [x] `print_hello`
- [x] `basic_compare`
- [x] `binary_precedence`
- [x] `cast_string_to_int`

Run `go test ./transpiler/x/pl -tags slow` to regenerate the `.pl` files and
verify their output.
