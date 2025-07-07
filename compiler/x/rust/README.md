# Rust Backend

The Rust backend compiles Mochi programs to plain Rust source code. It is a minimal and experimental implementation used for validating the language and running solutions anywhere `rustc` is available. Generated code is formatted with `rustfmt` when the tool is present so the output is easy to read.

## Supported features

- Variable declarations with `let` and `var`
- `if`/`else` expressions and `for`/`while` loops
- User defined functions, simple `fun` expressions, test blocks and `expect` statements
- Lists and maps including indexing, slicing, membership checks and iteration over map keys in `for` loops
- List concatenation and set operations (`union_all`, `union`, `except`, `intersect`)
- Builtins like `print`, `len`, `count`, `avg`, `input` and `str`
- Struct type declarations, literals and basic method definitions
- Dataset queries with grouping, sorting, pagination and inner/left joins


## Unsupported features

The current implementation lacks support for:

- Outer and right joins in dataset queries.
 - Agent and stream declarations (`agent`, `on`, `emit`).
 - Intent handlers within agents (`intent` blocks).
- Logic programming constructs (`fact`, `rule`, `query`).
 - Data fetching and persistence expressions (`fetch`, `load`, `save`, `generate`, `generate embedding`).
- Package imports, extern objects and the foreign function interface (`import`, `extern`).
- Model declarations (`model`) and related LLM helpers.
- Error handling with `try`/`catch`.
- Asynchronous functions (`async`/`await`).
- Concurrency primitives like `spawn` and channels.
- Generic type parameters and higher‑order functions.
- Reflection or macro facilities.

These limitations cause some programs to fail to compile with the Rust backend.
