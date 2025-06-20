# C Backend

The C backend generates portable ANSI C source code from Mochi programs. It is primarily used for benchmarks and as a reference implementation for other backends.

## Files

- `compiler.go` – main code generator walking the AST
- `compiler_test.go` – golden tests that compile and run generated code
- `tools.go` – helper to locate or install a system C compiler

## Runtime helpers

During code generation `compiler.go` injects minimal runtime support when required. The beginning of the output defines list structures and helper functions such as `list_int_create` and `concat_list_int`:

```c
#include <stdio.h>
#include <stdlib.h>
typedef struct { int len; int *data; } list_int;
```

Further helpers like `_count`, `_avg`, `_input`, `_str` and `_index_string` are emitted on demand.【F:compile/c/compiler.go†L140-L260】

## Features

The backend supports basic language features including variable declarations, `for`/`while` loops, conditionals and function definitions. Lists of integers and nested lists are handled using the `list_int` and `list_list_int` types. When compiling `for` loops over ranges or collections the generator emits plain C loops.【F:compile/c/compiler.go†L452-L527】

Builtin functions such as `print`, `len`, `count`, `avg`, `input` and `str` are translated to the appropriate helper calls or C library functions.【F:compile/c/compiler.go†L629-L757】

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

- `map` types and the `in` membership operator
- query expressions such as `from`/`sort by`/`select`
- detection of `list<string>` parameters (the runtime type is only emitted when
  used inside a function)

Problems relying on these features (for example LeetCode problems 13–25 and
30) fail to compile at the moment.
