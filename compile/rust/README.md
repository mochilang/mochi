# Rust Backend

The Rust backend compiles Mochi programs to plain Rust source code. It is a minimal and experimental implementation used for validating the language and running solutions anywhere `rustc` is available.

## Supported features

- Variable declarations with `let` and `var`
- `if`/`else` expressions and `for`/`while` loops
- User defined functions, simple `fun` expressions and test blocks
- Lists and maps including indexing, slicing and membership checks
- List concatenation and set operations (`union_all`, `union`, `except`, `intersect`)
- Builtins like `print`, `len`, `count`, `avg`, `input` and `str`


## Unsupported features

The current implementation lacks support for:

- Advanced dataset queries such as grouping and left/right joins. Outer joins are also not implemented.
- Agent and stream declarations (`agent`, `on`, `emit`).
- Logic programming constructs (`fact`, `rule`, `query`).
- Data fetching and persistence expressions (`fetch`, `load`, `save`, `generate`).
- Package imports, extern objects and the foreign function interface (`import`, `extern`).
- Model declarations (`model`) and related LLM helpers.
- Error handling with `try`/`catch`.
- Asynchronous functions (`async`/`await`).
- Concurrency primitives like `spawn` and channels.
- Generic type parameters and higher‑order functions.
- Reflection or macro facilities.

These limitations cause some programs to fail to compile with the Rust backend.
