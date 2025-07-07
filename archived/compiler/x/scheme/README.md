# Scheme Backend

The Scheme backend converts a limited subset of Mochi programs into Scheme source code that runs on the [chibi-scheme](https://github.com/ashinn/chibi-scheme) interpreter.  It is primarily intended for experimentation rather than full production use.

## Architecture

- `compiler.go` walks the Mochi AST and emits Scheme code.  Small runtime helpers for datasets, list set operations and slicing are inserted only when the compiled program uses them.  Early returns and loop control are implemented using `call/cc` and recursive `let loop` forms.
- `tools.go` locates `chibi-scheme` and attempts a best-effort installation when tests require it.
- `compiler_test.go` contains golden tests that compile example programs and execute them with `chibi-scheme`.

## Supported Features

- Function definitions, calls and anonymous functions
- Variable declarations and assignments
- Basic expressions with arithmetic, comparison and logical operators
- `if`, `for` and `while` statements
- Lists and maps with literals, indexing, membership checks and mutation
- String and list slicing
- Built‑ins: `len`, `count`, `avg`, `sum`, `max`, `min`, `str`, `push`, `keys`, `print`, `input`, `_fetch`, `_load`, `_save`
- Simple `test` blocks with `expect` assertions
- List set operators `union`, `union_all`, `except` and `intersect`
- Struct type declarations and methods
- Basic dataset queries with filtering, cross joins, simple joins, sorting,
  pagination and selection

## Unsupported Features

The backend leaves many Mochi constructs unimplemented, including:

- Grouping or outer joins in dataset queries
- Generative AI blocks and LLM helpers
- Error handling with `try`/`catch`
- Union types and `match` expressions
- Foreign imports and the package system
 - Agents, streams and concurrency primitives such as `spawn`
- Set collections
- Export statements
- Generic type parameters
- Reflection or macro facilities
- Asynchronous functions (`async`/`await`)
- Outer joins or complex aggregation
- Pattern matching on union variants
- Destructuring bindings
- Nested recursive functions inside other functions
- Advanced slicing and collection mutators
- Logic programming predicates
- `load` and `save` only support JSON and JSONL formats

## Building

Generate Scheme from a Mochi program and execute it with chibi-scheme:

```bash
mochi build --target scheme main.mochi -o main.scm
chibi-scheme -m chibi main.scm
```

`EnsureScheme` tries to install chibi-scheme automatically when it is missing.

## Tests

Golden tests under `compile/x/scheme` compile and run example programs. They are tagged `slow` because they invoke an external interpreter:

```bash
go test ./compile/x/scheme -tags slow
```

The tests are skipped if `chibi-scheme` is unavailable.

## Scheme to Mochi Converter

`tools/any2mochi` includes a lightweight converter for Scheme source. It relies
on `scheme-langserver` when available and falls back to a simple parser.

### Supported Features

- Extraction of top‑level `define` forms
- Parameter names and return types from language server hover text when present
- Basic translation of function bodies with `display`, `set!` and arithmetic

### Unsupported Features

- Macros and advanced control flow
- Continuations and nested `let` expressions
- Comprehensive type information or module imports
