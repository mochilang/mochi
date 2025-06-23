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
- `if`, `for`, `while` and range loops
- functions and closures
- lists and maps with indexing and updates
- built-in helpers: `print`, `len`, `str`, `input`, `count`, `avg`
- set operations: `union`, `union all`, `except`, and `intersect`
- simple test blocks (`test`)

## Unsupported Features

Several advanced language features are not yet available:

- modules or imports
- concurrency primitives
- generics and complex type inference
- advanced dataset queries such as joins or grouping
- dataset helpers like `fetch`, `load`, `save` and `generate`
- pattern matching with `match`
- agent/stream declarations (`agent`, `on`, `emit`)
- foreign function imports
- type declarations such as `struct` or `union`

## Tests

Golden tests in `tests/compiler/php` compile and run programs using the system `php` interpreter. Run them with:

```bash
go test ./compile/php -tags slow
```

`tools.go` attempts to install `php` automatically when required.
