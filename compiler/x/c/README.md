# C Backend

The C backend generates portable ANSI C source code from Mochi programs. It is primarily used for benchmarks and as a reference implementation for other backends.

## Files

- `compiler.go` – main code generator walking the AST
- `compiler_test.go` – golden tests that compile and run generated code
- `tools.go` – helpers for the system C compiler and clang-format

## Runtime helpers

During code generation `compiler.go` injects minimal runtime support when required. The beginning of the output defines list structures and helper functions such as `list_int_create` and `concat_list_int`:

```c
#include <stdio.h>
#include <stdlib.h>
typedef struct { int len; int *data; } list_int;
```

Further helpers like `_count`, `_avg`, `_input`, `_str` and `_index_string` are emitted on demand. The compiler now records required helpers in a map instead of dozens of boolean fields for cleaner code.【F:compile/c/compiler.go†L140-L260】

## Features

The backend supports basic language features including variable declarations, `for`/`while` loops, conditionals and function definitions. Lists of integers, floats, strings and nested integer lists are handled using the `list_int`, `list_float`, `list_string` and `list_list_int` types. When compiling `for` loops over ranges or collections the generator emits plain C loops.【F:compile/c/compiler.go†L452-L527】

Builtin functions such as `print`, `len`, `count`, `avg`, `input`, `str`, `now` and `json` are translated to the appropriate helper calls or C library functions.【F:compile/c/compiler.go†L629-L757】
Extern variable and function declarations are emitted verbatim as C `extern` statements and registered in the type environment.

Additional features include:

- list slicing for strings, integer, float and string lists
- membership checks and `union`, `except` and `intersect` operations on lists
- `in` operator tests substring membership in strings
- builtin `min` and `max` functions for integer, float and string lists
- test blocks and `expect` statements compiled as helper functions
- `if` expressions for conditional values
- anonymous `fun` expressions (without captured variables) compiled to static functions
- methods declared inside `type` blocks
- variables without an explicit type use GCC `__auto_type` for inference
- union types are translated to tagged C unions

## Building

Compile a Mochi program to C using the build command:

```bash
mochi build --target c main.mochi -o main.c
```

Then use your system compiler to build the executable:

```bash
cc main.c -o main
./main
```

`tools.go` provides `EnsureCC` to locate `cc`, `gcc` or `clang` and attempt installation on Linux or macOS if missing. Set the `CC` environment variable to override the compiler used.【F:compile/c/tools.go†L10-L56】
`FormatC` prettifies generated code using `clang-format` when available. `EnsureClangFormat` can install the formatter if missing.【F:compile/x/c/tools.go†L78-L132】

## Tests

The golden tests in `compiler_test.go` verify that generated C matches expected output and executes correctly. They are tagged `slow` as they invoke the C toolchain:

```bash
go test ./compile/c -tags slow
```

They compile example programs from `tests/compiler/c` and compare the results.【F:compile/c/compiler_test.go†L71-L108】

The separate `leetcode_test.go` file compiles and executes the first ten
LeetCode solutions under `examples/leetcode/1` through `examples/leetcode/10`
using the C backend.

## Unsupported features

The current C backend only implements a small subset of Mochi. Several
constructs required by later LeetCode problems are not yet supported. Missing
features include:

- `map` types
- basic `from`/`where`/`select` queries are supported. Sorting is implemented,
  but joins and grouping remain unimplemented
- enum definitions
- agent-related constructs (`agent`, `stream`, `intent`)
- generative `generate` blocks and model definitions
- dataset helpers such as `fetch`, `load`, `save` and SQL-style `from ...` queries
- full pattern matching with `match` (simple constant matches are supported)
- foreign function interface via `import` and package declarations
- concurrency primitives like `spawn` and channels
- generics for user-defined types
- logic programming constructs (`fact`, `rule`, `query`)
- reflection or macro facilities
- package exports (extern objects are now supported)
- nested list types other than `list<list<int>>`
- set literals and set operations
- functions with multiple return values
- map membership operations
- iterating over maps with `for` loops
- variadic functions
- closures that capture surrounding variables
- destructuring bindings in `let` and `var` statements
- YAML dataset loading and saving
- right and outer joins in dataset queries
- agent initialization with field values
- nested recursive functions inside other functions
- `export` statements
- constructing union variant values or matching on them
- automatic language imports (`import python "..." auto`)
- extern object declarations

The backend now supports membership checks and `union`/`union all` operations
for integer, float, and string lists, along with slicing and printing of string
lists, but map membership and other advanced features
remain unimplemented.

Test blocks and `expect` statements are now compiled to C functions and
executed from `main`.

Additional language features like `try`/`catch` error handling and the planned
`async` keyword are also unimplemented. Problems relying on these features
(for example LeetCode problems 13–25 and 30) fail to compile at the moment.
