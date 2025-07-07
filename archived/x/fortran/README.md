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
- list operations: `union`, `union all`, `except` and `intersect` on integer, float and string lists
- basic dataset queries with `from`/`where`/`select` and optional `skip`/`take`
- struct types declared with `type` blocks and simple struct literals
 - built-ins: `len`, `append`, `count`, `avg`, `str`, `abs`, `now`
- printing via `print()`
- `package` and `export` declarations (ignored during code generation)
- `import` statements emit Fortran `include` lines
- slice assignments like `xs[0:1] = sub`
- negative list and string indices wrap around using modulo

## Unsupported features

- map types and membership tests for maps
- map indexing and assignment
- nested functions
- slice expressions with a step
- pattern matching with `match`
- agents, streams and logic programming constructs (`fact`, `rule`, `query`)
 - foreign imports
- anonymous functions
- extern declarations
- range loops with step values other than `1`
- event handlers (`on`) and `emit` statements
- generative blocks and model declarations
- asynchronous functions (`async`/`await`)
- set collections (`set<T>`) and related operations
- concurrency primitives like `spawn` and channels
- any other built-ins not listed above

### Limitations for TPC-H queries

The experimental Fortran backend lacks support for map-based records and
`group by` dataset queries with aggregates. Queries such as `tpc-h/q1.mochi`
use those features and therefore cannot currently be compiled to Fortran.

While limited, this backend shows how Mochi's AST can target another language and may serve as a basis for more comprehensive Fortran support.
