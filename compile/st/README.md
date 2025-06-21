# Smalltalk Backend

This backend converts Mochi programs to GNU Smalltalk. The tests under `compile/st` use the `gst` interpreter.

## Installation

The helper `EnsureSmalltalk` installs the `gst` interpreter when needed. It relies on common package managers or builds GNU Smalltalk from source as a fall back.

## Usage

Compile a program with `mochi build --target st` and run it using `gst`.
Run the Smalltalk tests with:

```bash
go test ./compile/st -tags slow
```

## Supported features

The backend implements a minimal subset of Mochi:

- Variable declarations and assignments
- Arithmetic and boolean expressions
- `if`/`else` conditionals
- `for` and `while` loops with `break` and `continue`
- Functions with a single return value
- Lists and maps with indexing and concatenation
- Basic `type` declarations compiled to dictionaries
- Built-ins `print`, `len`, `str`, `count`, `avg`, `input`, `now`, `json`,
  `append`, `eval`, and `reduce`
- List set operations using `union`, `union_all`, `except` and `intersect`

### Unsupported features

The following language constructs are not yet handled:

- Agents and stream handlers
- Dataset queries and data operations (`fetch`, `load`, `save`)
- Generative AI helpers such as `generate`
- Logic programming (`fact`, `rule`, `query`)
- Foreign function interface declarations (`extern`)
- Import and package statements
- Model declarations (`model`) and related LLM helpers
- Pattern matching with `match`
- Union type declarations
- Set literals
- Concurrency primitives like `spawn` and channels
- Event emission with `emit`
- Intent declarations with `intent`
- Methods declared inside `type` blocks
- Functions with multiple return values
- Variadic functions and destructuring bindings
- List and string slicing
- Iterating over maps with `for` loops
- YAML dataset loading and saving
- Right and outer joins in dataset queries
- Agent initialization with field values
- Nested recursive functions inside other functions
- `export` statements and constructing union variant values

