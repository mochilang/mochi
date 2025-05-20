# 📦 CHANGELOG.md

All notable changes to the Mochi programming language are documented in this file.

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


