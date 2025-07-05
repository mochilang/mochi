# Java Backend

The Java backend generates plain Java source from Mochi programs. The emitted
code contains a simple `Main` class with a `main` method when top-level
statements are present.

## Files

- `compiler.go` – code generator walking the Mochi AST
- `compiler_test.go` – golden tests that build and run the output
- `helpers.go` – identifier sanitisation helpers
- `tools.go` – utility that ensures `javac` is installed

## Supported Features

- Function declarations, calls and top-level statements
- Control flow with `if`, `for` and `while`
- Primitive types (`int`, `float`, `bool`, `string`)
- Lists and maps
- Basic expressions and assignments
- Variable declarations with `let` and `var`
- Map updates using `put`
- Anonymous `fun` expressions
- Built-ins `print`, `len`, `str`, `input`, `count`, `avg`, `now`, `json` and `eval`
- List operations `concat`, `union`, `union_all`, `except`, `intersect` and `slice`
- HTTP requests via `fetch`
- Dataset helpers `load` and `save`
- Generative `generate` blocks for text, embeddings and structs
- Test blocks with `expect`
- Pattern matching expressions with `match`
- Dataset queries with joins, sorting, pagination and basic grouping

## Building

```bash
mochi build --target java main.mochi -o Main.java
javac Main.java
java Main
```

## Tests

```bash
go test ./compile/java -tags slow
```
The tests skip automatically if `javac` is not available and attempt to compile
a selection of LeetCode solutions.

## Unsupported Features

- The Java backend currently lacks many Mochi features supported by other
compilers:
- Union types and tagged unions
- Concurrency primitives like streams, agents, `spawn` and channels
- Foreign imports and extern functions
- Logic programming constructs (`fact`, `rule`, `query`)
- Import statements
- Model declarations
- Reflection or macro facilities
- Extern variable, type and object declarations
- Event emission and intent handlers (`emit`, `on`)
- Methods declared inside `type` blocks
- Agent initialization with field values
- Set collections (`set<T>`) and related operations
- Error handling with `try`/`catch` blocks
- YAML dataset loading/saving
- Destructuring bindings in `let` and `var` statements
- Conditional expressions using `if`/`else`
