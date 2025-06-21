# Kotlin Backend

The Kotlin backend converts Mochi programs into Kotlin source files so they can run on the JVM or be embedded in Kotlin projects.

## Supported Features

- Struct and union declarations map to Kotlin `data class` and `sealed interface` types
- `match` expressions compile to Kotlin `when` blocks
- Dataset queries use collection operations like `filter`, `sortedBy`, `drop`, `take` and `map`
- Basic loops (`for`, `while`), conditionals and arithmetic expressions
- Built‑in helpers including `print`, `len`, `count`, `avg`, `str`, `input`, `json`, and `now`
- LLM and runtime helpers such as `_genText`, `_genEmbed`, `_genStruct`, `_fetch` and `_eval`
- List set operators `union`, `union_all`, `except` and `intersect`

## Unsupported Features

The Kotlin backend still lacks several features available in other compilers:

- Advanced dataset queries (joins, grouping and additional set operations)
- Streams, agents and intent handlers
- Logic programming (`fact`, `rule`, `query`)
- Foreign function interface and cross-language imports
- Event emission with `emit` statements
- Extern declarations
- Concurrency primitives such as `spawn` and channels
- Error handling with `try`/`catch` blocks
- Generic types and functions
- Set collections remain unsupported
- Reflection or macro facilities
- YAML dataset loading and saving
- Full LLM integration for `_genText`, `_genEmbed` and `_genStruct`
- Asynchronous functions (`async`/`await`)

## Building

Generate Kotlin code from a Mochi program:

```bash
mochi build --target kt main.mochi -o Main.kt
```

Compile the result with `kotlinc` and run it using standard JVM tools.

## Tests

Golden tests under `tests/compiler/kt` compile and execute each program. They are tagged `slow` because they invoke the Kotlin toolchain:

```bash
go test ./compile/kt -tags slow
```

The tests automatically skip when `kotlinc` is unavailable.
