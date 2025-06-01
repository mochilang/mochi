# 📦 CHANGELOG.md

All notable changes to the Mochi programming language are documented in this file.

## [0.2.6] – 2025-06-01

### Added

* Support for `for-in` loops over lists and maps, enabling iteration with the following pattern:

  ```mochi
  let items = [1, 2, 3]
  for item in items {
    print(item)
  }

  let users = {"alice": 1, "bob": 2}
  for user in users {
    print(user)
  }
  ```

* Bytecode compilation and VM execution support for both:

  * Range-based `for x in start..end {}` loops.
  * Collection-based `for x in expr {}` loops (lists and maps).

* New compiler helper to detect and compile index expressions (e.g. `scores["alice"]`).

* Enhanced runtime handling of `OpIndex` to support both list and map lookups with bounds checking.

* Internal helper function `extractIndexExpr` for simplifying indexed expression detection in the compiler.

### Changed

* Rewrote `compiler.compileStmt` logic to explicitly detect and handle `AssignStmt` with indexed expressions.
* Updated `parser.ForStmt` grammar to clearly distinguish range loops from collection loops.
* Improved debug logging and error messages for out-of-bounds index access in the VM.

### Fixed

* Correct binding of loop variables and temporary state tracking (`__start`, `__end`, `__i`, `__coll`) in compiled bytecode.
* Resolved edge case where invalid index expressions would silently compile without error.
* Ensured that `for-in` statements over maps produce deterministic iteration in compiled programs.

## [0.2.5] – 2025-06-01

### Added

* Full implementation of the Mochi compiler and virtual machine (VM), providing end-to-end execution from parsed AST to runtime evaluation.
* Bytecode instruction set and stack-based VM engine supporting arithmetic, comparison, control flow, function calls, list/map literals, and built-in functions.
* Constant folding for binary expressions with literal operands, including arithmetic (`+`, `*`, etc.), comparison (`==`, `<`, etc.), and list concatenation.
* Compile-time evaluation of pure user-defined functions when invoked with constant arguments (e.g., `sum(10)` → `55`), enabling early computation and performance gains.
* `OpDefineFun` and `OpCall` instructions with support for lexical closures and recursive function execution.
* Debug logging for both compiler and VM execution, including stack inspection and instruction tracing.
* Built-in functions: `print`, `len`, `now`, and `json`, accessible via `OpConst` and `OpCall`.
* Bytecode-level `OpMakeList`, `OpMakeMap`, `OpIndex`, and `OpGetAttr` for working with structured literals and dynamic data.

### Changed

* Replaced early interpreter-based execution with VM-based bytecode model.
* Simplified function body representation using linear `[]Instruction` chunks.
* Updated compiler stack simulation (`stackDepth`) for validation of balanced push/pop semantics during code generation.

### Fixed

* Corrected handling of operator precedence in binary expressions.
* Addressed stack underflow in `OpCall` and `OpReturn` for deeply nested calls.

### Notes

* Function call folding is currently limited to pure functions with constant arguments and basic operations; runtime evaluation is used to simulate execution during compile time.
* List folding is supported for literal concatenation, but runtime list operations remain dynamic.
* This version forms the foundation for further optimization, including static typing, closure capture analysis, and performance tuning of the VM stack model.

## [0.2.4] – 2025-05-31

### Added

* Introduced initial [benchmark suite](https://github.com/mochi-lang/mochi/tree/main/bench) comparing Mochi against Python, TypeScript, and Go templates.
* Added `math` benchmark templates covering:

  * Iterative and recursive factorial
  * Fibonacci (iterative and recursive)
  * Prime counting
  * Matrix multiplication
  * Sum and multiplication loops
* Included `bench/runner.go` for consistent execution and output aggregation.

### Changed

* Refactored `evalBinaryExpr` with a fast path for `int + int` specialization, reducing arithmetic overhead in tight loops.
* Improved arithmetic expression evaluation order by explicitly applying operator precedence.
* Enhanced performance logging in benchmark harness with timing normalization across languages.

### Fixed

* Corrected `now()` implementation to return nanoseconds (int64) for accurate duration measurement.
* Fixed logic in benchmark templates where loop upper bounds were off-by-one in `range` expressions.
* Addressed incorrect duration reporting in `prime_count` and `fib_rec` benchmarks due to warm-up effects.

### Performance Notes

While Mochi remains significantly slower than Go and Python due to AST-walking evaluation, this release reduces arithmetic overhead in many microbenchmarks. Benchmarks now provide a clear baseline to track performance regressions and future interpreter optimizations.

## [0.2.3] - 2025-05-21

### ✨ Added

* **Support for nested unary expressions**, including `!!a`, `--x`, `-!x`, etc.
* **Support for infix binary expressions** with proper precedence and associativity, replacing older nested `Expr → Term → Factor` form with `BinaryExpr` chain.
* **`math.mochi`** example to test operator precedence and grouping (e.g. `1 + 2 * 3` vs `(1 + 2) * 3`).
* `Subst` type and `unifyWith` mechanism for unification with substitution of `TypeVar`s.
* Type inference for inline anonymous functions (`fun() => 42`) with return type variable `'R` properly resolved through substitution.

### 🛠️ Changed

* `checkFunExpr` now supports return type inference by unifying declared `TypeVar` return with actual return.
* Function literal type checking flow is now correct even when no type annotation is given on return.
* `unify` now supports a one-directional fallback to handle inferred return types in a predictable and sound way.

### 🐛 Fixed

* Fixed parser bug where closing parentheses after binary expressions (e.g. `("hello" + "!")`) triggered `unexpected token ')'` errors.
* Fixed incorrect parse failure on expressions like `-1` due to overly strict `Expr` grammar root.
* Fixed type inference failure in:

  ```mochi
  fun always42(): fun(): int {
    return fun() => 42
  }
  ```

  which now correctly infers `fun(): int`.


## [0.2.2] - 2025-05-20

### Added

* Support for parsing `PostfixExpr` with selectors and index operations (e.g., `scores["Alice"]`, `obj.field.subfield`).
* Support for `map<string, T>` and `list<T>` as first-class data structures.
* Type checker and runtime support for accessing elements in maps and lists.
* `len(...)` function now works on maps, lists, and strings.
* `test "..." { ... }` blocks are now supported for assertions.
* New runtime error diagnostics for invalid indexing and type mismatches (e.g., `I016`, `T018`).
* Support for `expect` statements within test blocks.
* Cleaner error messages with help text for invalid indexing and type usage.

### Changed

* Replaced simple identifier assignment (`x = ...`) with structured assignment using `PostfixExpr` (supports future `obj.field`, `arr[i]`).
* `AssignStmt` now uses `Target *PostfixExpr` instead of `Name string`.
* Parser and interpreter refactored to support structural assignment targets.

### Removed

* Mutability features such as `scores["Alice"] = 10` and `del scores["Bob"]` have been disabled for now to enforce immutability by design.
* Fallback expressions (`scores["Zoe"] or 0`) and inline defaulting are removed until language-level support is finalized.
* Redundant parser grammar strings such as `Expect string "expect"` and similar unused tags.

## [0.2.1] – 2025-05-19

### Added

* ✅ **Indexing and slicing** support for lists and strings:

  ```mochi
  let fruits = ["🍎", "🍌", "🍇", "🍑"]
  print(fruits[0])     //> 🍎
  print(fruits[1:3])   //> ["🍌", "🍇"]
  ```
* ✅ **Negative indices** (e.g., `fruits[-1]` for last element)
* ✅ `len(...)` built-in function:

  ```mochi
  let x = [1, 2, 3]
  print(len(x)) //> 3
  ```

### Changed

* 🔄 Refactored interpreter to support **builtin function registry** (`map[string]BuiltinFunc`)
* 🧠 Improved error messages for:

  * Invalid index target (e.g. indexing into unsupported types)
  * Slice/index bounds violations
  * Undefined built-in functions
  * Mismatched argument types (e.g. `len(42)`)

### Fixed

* 🐞 Parser ambiguity between `ListLiteral` and `IndexExpr` (trailing comma parsing)
* 🐞 Indexing/slicing properly returns value types at runtime and passes through typechecker
* 🐞 `len(...)` now works with any list (type `[T]`, including `[any]`)

## [0.2.0] - 2025-05-19

### ✨ Added

* `make release` command for one-command version tagging, commit, and publishing via GoReleaser.
* Support for `mochi run` and `mochi repl` subcommands using `go-arg`.
* `--version` flag with detailed output (version, commit, build time, OS/Arch).
* AST printing via `--ast` flag: shows Lisp-style syntax tree after successful type checking.
* GoReleaser config for cross-platform binary builds (macOS, Linux, Windows; amd64/arm64).
* GitHub Actions-based release workflow support.
* `VERSION` file to decouple versioning from Git tags.

### 💎 Improved

* Build metadata is now embedded via `-ldflags`: version, commit hash, and ISO 8601 build time.
* Build time is reformatted to a readable string (`Tue May 21 08:45:00 2025`) at runtime.
* Simplified `printVersion()` to display concise, informative one-liner with version, commit, build time, and platform.
* Output format and error messages across CLI commands are more user-friendly and consistent.

### 🧪 Changed

* Default behavior for `make release` is now a dry run; use `RELEASE=true` for actual release.
* Output binaries go to `$(HOME)/bin`, and `dist/` is automatically cleaned by `make clean`.
* Build reproducibility improved by enforcing ISO 8601 UTC in `BUILD_TIME`.

## [0.1.12] – 2025-05-18

### 🎉 Initial Developer Preview

Mochi v0.1.12 is the first developer preview release. It’s a small, statically typed language designed for clarity,
composability, and testability — ideal for intelligent tools, AI agents, and educational compilers.

### ✨ Language Features

- `let` bindings with static types (with type inference)
- Arithmetic, comparison, boolean logic
- `if` / `else` control flow
- Named functions and anonymous closures
- Currying and higher-order functions
- `for ... in ...` loops with integer ranges
- Expression-first syntax with return values
- Built-in `print(...)`

### 🧪 Testing

- `test "..." { ... }` blocks for inline testing
- `expect expr` assertions for golden tests
- Golden test runner with:
    - `make test` – run tests
    - `make update-golden` – update expected outputs

### ⚙️ Tooling and Developer Experience

- `mochi` CLI built with `go-arg`, supports:
    - `--run` to execute
    - `--ast` to print AST
    - `--version` to print build metadata
- `mochi-run` tool to generate:
    - `examples.md` – with source, AST, output
    - `llm.md` – source code listing for ingestion
    - `grammar.ebnf` – exported EBNF from parser
- Multiline comment support (`/* ... */`)
- Rich diagnostic engine with source position and suggestions
- Lisp-style AST formatting
- Colorful, emoji-enhanced Makefile DX:
    - `make build`, `make fmt`, `make lint`, `make clean`, etc.
- Auto-detect `golangci-lint`, fallback to `go vet`

### 🧱 Project Structure

- `cmd/mochi/` — CLI tool
- `cmd/mochi-run/` — doc/code generator
- `parser/` — lexer and recursive descent parser
- `ast/` — AST nodes, conversion, pretty-printer
- `types/` — type checker
- `interpreter/` — runtime evaluator
- `diagnostic/` — structured error reporting
- `examples/` — testable programs for golden tests

### 🔨 Internal Improvements

- Fully idiomatic Go style using standard libraries
- Testable components with minimal external dependencies
- Self-contained golden test framework
- Support for `Mochi` as a compiler backend or teaching tool


