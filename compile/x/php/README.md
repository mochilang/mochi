# PHP Backend

The PHP backend translates Mochi programs into plain PHP so they can run anywhere a `php` interpreter is available. It is a lightweight code generator geared towards scripting and small utilities.

## Building

Use the `mochi` CLI to compile a source file to PHP and execute it:

```bash
mochi build --target php main.mochi -o main.php
php main.php
```

## Supported Features

The PHP compiler implements a subset of Mochi including:

- variable declarations and assignments
- top-level variables and constants
- `if`, `for`, `while` and range loops
- functions and closures
- lists and maps with indexing and updates
- simple struct type declarations
- built-in helpers: `print`, `len`, `str`, `input`, `count`, `avg`, `sum`, `json`, `fetch`, `load`, `save`
- generative helpers: `_gen_text`, `_gen_embed`, `_gen_struct`
- dataset queries with `from`/`where`/`select`, sorting, pagination and joins
- grouping operations in dataset queries
- set operations: `union`, `union all`, `except`, and `intersect`
- simple test blocks (`test`)

## Unsupported Features

Several advanced language features are not yet available:

- modules or imports
- concurrency primitives
- generics and complex type inference
- pattern matching with `match`
- agent/stream declarations (`agent`, `on`, `emit`)
- foreign function imports
- union types or advanced generics

## Tests

Golden tests in `tests/compiler/php` compile and run programs using the system `php` interpreter. Run them with:

```bash
go test ./compile/php -tags slow
```

`tools.go` attempts to install `php` automatically when required.
