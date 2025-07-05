# Racket Backend

The Racket backend emits plain Racket code from Mochi programs. It covers a
small subset of the language and is mainly used for testing and experiments.

## Files

- `compiler.go` – converts the AST into Racket forms
- `compiler_test.go` – golden tests executed with `racket`
- `helpers.go` – utilities such as identifier sanitising
- `tools.go` – installs `racket` for the tests if needed

## Building

Compile a Mochi source file to Racket and execute it with the `racket` command:

```bash
mochi build --target rkt main.mochi -o main.rkt
racket main.rkt
```

## Tests

Tests are tagged `slow` because they invoke the external `racket` tool:

```bash
go test ./compile/rkt -tags slow
```

## Supported Features

- Basic control flow including `if`, `else if` and loops with `break` and `continue`
- Binary operators such as `+`, `-`, `*`, `/`, `==`, `!=` plus list set operators `union`, `union_all`, `except` and `intersect`
- `match` expressions
- Dataset helpers `_fetch`, `_load` and `_save` for JSON data
- Generate expressions via `_genText`, `_genEmbed` and `_genStruct`
- `in` operator for lists and strings
- Slice assignments, including multi-dimensional slices like `xs[0:1][1:2] = sub`
- Negative indexing and slicing for strings and lists
- Dataset queries with filtering via `where`, sorting, pagination with
  `skip`/`take`, `distinct` results and simple `group by` clauses
- Simple `struct` type declarations
- Iteration over map keys
- Function definitions and calls
- Test blocks with `expect` assertions
- Anonymous function expressions

## Unsupported Features

- Model definitions (`model` blocks)
- Error handling with `try`/`catch`
- Agents, streams and intents
- Logic programming constructs (`fact`, `rule`, `query`)
- Concurrency primitives such as `spawn` and channels
- Package management and `package` declarations
- Union type declarations
- Python interop via `_pyAttr`
- Methods defined inside `type` blocks
- Extern declarations (`extern var`, `extern fun`, `extern type`, `extern object`)
- Destructuring bindings in `let` and `var` statements
