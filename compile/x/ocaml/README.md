# OCaml Backend

The OCaml backend translates a subset of Mochi into plain OCaml code.  It is
mainly intended for algorithmic examples focusing on lists, loops and basic IO.

## Files

- `compiler.go` – walks the Mochi AST and emits OCaml code
- `compiler_test.go` – golden tests that compile and run the generated program
- `tools.go` – helper for tests that ensures `ocamlc` is available

## Compilation

The compiler walks the Mochi AST and produces OCaml source using simple helper routines. It keeps track of indentation and emits loops, conditionals and function definitions in a straightforward manner.

## Tools

`EnsureOCaml` ensures that the OCaml compiler is available when tests run.

## Building

Generate OCaml source using `mochi build`:

```bash
mochi build --target ocaml main.mochi -o main.ml
```

## Supported features

- `let` and `var` declarations
- arithmetic and boolean expressions
- `if`, `for` and `while` statements
- `break` and `continue`
- list and map literals
- list indexing, assignment and slicing
- map access and membership checks
- functions with single return value
- function expressions (`fun`)
- built-ins `len`, `print`, `str`, `input`
- simple `match` expressions with constant patterns
- dataset queries with `from`/`where`/`select` and optional `skip`, `take` and `sort by`
- `fetch`, `load` and `save` expressions
- test blocks and `expect` statements
- struct and enum type declarations


The output can be compiled with `ocamlc`:

```bash
ocamlc main.ml -o main
./main
```

## Tests

The golden tests compile programs under `tests/compiler/ocaml` and a curated
subset in `tests/compiler/valid_ocaml`, then run them with `ocamlc`:

The tests check the generated program output using `ocamlc`.

Run the tests with:

```bash
go test ./compile/ocaml -tags slow
```

These tests verify both the generated program output and the emitted `.ml` code.

## Unsupported features

- The OCaml backend covers only a small slice of Mochi. Missing pieces include:

 - Partial dataset queries (joins and grouping not implemented)
- Comprehensive pattern matching and union types
- Modules and `import` declarations
- `generate` expressions
- Agent and model blocks
- Concurrency primitives like `spawn` and channels
 - Streams, LLM helpers and the foreign function interface
- Set literals and set operations
- Functions with multiple return values
- List membership operations and list unions
- Built-in functions `count` and `avg`
- Logic programming constructs like `fact`, `rule` and `query`
- Extern type or object declarations and package exports

## OCaml to Mochi Conversion

`tools/any2mochi` ships with a small OCaml converter. It primarily relies on
`ocamllsp` for symbol information but falls back to regex parsing for standalone
statements when no symbols are reported.

### Supported Features

- Top-level function definitions with parameter and return types
- Variable bindings defined with `let`
- Simple bodies containing assignments and `print_endline` calls

### Unsupported Features

- Pattern matching clauses and complex control flow
- Module declarations and `open` statements
- Advanced data structures or imperative features
