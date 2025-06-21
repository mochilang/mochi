# Fortran Backend

The Fortran backend generates basic Fortran 90 code from a very small subset of Mochi. It was created as a demonstration and primarily targets algorithmic examples such as the LeetCode "two sum" problem. The emitted code avoids dependencies and uses simple array allocations.

## Usage

```bash
mochi build --target fortran source.mochi -o program.f90
gfortran program.f90 -o program
./program
```

Run `go test ./compile/fortran -tags slow` to execute the golden tests. They will attempt to compile the generated code with `gfortran`.

## Supported features

- `let` bindings for integers, floats, strings and lists
- `if` statements and expressions
- `for` loops over numeric ranges
- `while` loops
- `break` and `continue`
- `test` blocks and `expect` statements
- function definitions returning integers, floats, strings or lists
- list operations: `union`, `except` and `intersect` on integer, float and string lists
- built-ins: `len`, `append`, `count`, `avg`, `str`, `now`
- printing via `print()`
- `package` and `export` declarations (ignored during code generation)
- `import` statements emit Fortran `include` lines

## Unsupported features

- map types and membership tests for maps
- map indexing and assignment
- query expressions (`from`/`sort by`/`select`)
- nested functions and struct literals
- slice expressions with a step
- pattern matching with `match`
- agents, streams and logic programming constructs (`fact`, `rule`, `query`)
- foreign imports and dataset helpers (`fetch`, `load`, `save`)
- anonymous functions
- extern declarations
- slice assignments like `xs[0:1] = sub`
- range loops with step values other than `1`
- event handlers (`on`) and `emit` statements
- type declarations using `type` blocks
- generative blocks and model declarations
- any other built-ins not listed above

While limited, this backend shows how Mochi's AST can target another language and may serve as a basis for more comprehensive Fortran support.
