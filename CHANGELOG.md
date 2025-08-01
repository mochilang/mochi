# 📦 CHANGELOG.md

## [0.10.55] – 2025-08-02T16:16:26+07:00

### Added

* Kotlin circular primes and call-a-function examples with struct method support
* Pascal, Racket and Zig transpilers extend map handling, dynamic indexing and multi-argument features
* New Rosetta benchmarks such as Floyd–Warshall, Cholesky decomposition and bubble sort

### Changed

* Rosetta outputs regenerated across PHP, Go, Scala, Ruby and more
* Improved map, list and index handling in Pascal, Kotlin, Scala, Swift and C transpilers

### Fixed

* Java list literal typing, Kotlin map assignment and Scala lambda returns
* Swift BigInt modulo overflow, Ruby fetch helper JSON dependency and PHP benchmark timing

## [0.10.54] – 2025-08-02T10:23:53+07:00

### Added

* F# transpiler support for array strings, typed maps and dictionaries
* Swift, Scala and OCaml transpilers gain large integer support, dynamic struct indexing and a `split` builtin
* Rust function prototypes and Clojure catamorphism generation

### Changed

* Rosetta benchmark outputs refreshed across languages including Dart and Go
* Improved float literal emission and parameter casts

### Fixed

* Base64 decode and return handling bugs
* Dart transpiler map index null issue

## [0.10.53] – 2025-08-01T14:33:11+07:00

### Added

* `mochix` print command for AST output
* AST printers implemented across more languages including F#, Haskell, Swift and Prolog
* Kome compiler example with dedicated tests
* Extensive golden tests across languages

### Changed

* Build runs without the `slow` tag
* Swift printer supports `do` statements and interpolation
* Enhanced record, operator and pointer handling across printers

### Fixed

* Minor build issues and printer bugs

## [0.10.51] – 2025-07-31T09:04:29+07:00

### Added

* `mochix` inspect command for AST output
* Typed AST examples for `print_hello`, `cross_join` and `two-sum`
* New builtins `padStart`, `slice` and `repeat`

### Changed

* Parsers migrated to official tree-sitter grammars
* Rosetta outputs regenerated with new benchmarks
* Transpiler improvements across languages

### Fixed

* Variable scope, string key and MD5Hex bugs

## [0.10.50] – 2025-07-30T16:21:04+07:00

### Added

* Typed JSON AST nodes across many languages
* Optional position flags and root aliases
* Minimal AST outputs regenerated

### Changed

* Golden JSON AST samples refreshed

### Fixed

* Minor parser issues for typed nodes

## [0.10.49] – 2025-07-30T09:15:33+07:00

### Added

* JSON AST inspectors for many languages with embedded parsers
* Ruby inspector decoupled from a2mochi
* Extensive golden tests verifying AST outputs
* New builtins and loop handling across compilers

### Changed

* Parsing refinements across C, Pascal, Scheme, Dart and others
* Documentation progress tables refreshed

### Fixed

* Minor output and parser issues resolved across languages

## [0.10.48] – 2025-07-29T09:06:36+07:00

### Added

* Additional program conversions and builtins across many a2mochi converters
* Embedded Java parser and function type support for Kotlin transformations
* Go converter handles list for-loops and struct types
* Dart, Prolog and Lua gain new builtins and loop conversions
* Racket, Scala and PHP transformers cover more examples and nested conditions
* Python, Java and Swift golden files refreshed with extra tests

### Changed

* Converter test suites rewritten without JSON round-trips and shared helpers
* More VM samples pass with refreshed golden files across languages

### Fixed

* Zig output regressions corrected with updated tests
* Various converter bugs uncovered by new coverage

## [0.10.43] – 2025-07-28T11:11:30+07:00

### Added

* Environment variable helpers and OS FFI across multiple transpilers
* BigRat math, union operations and typed maps
* Dynamic function calls, typed lambdas and improved indexing
* Hundreds of new Rosetta programs with updated benchmarks

### Changed

* Progress indexes and examples refreshed across languages
* Map handling and type inference enhanced in various backends

### Fixed

* Numerous bugs with map iteration, globals and casting
## [0.10.42] – 2025-07-27T09:00:51+00:00

### Added

* Numerous new Rosetta programs such as Faulhabers formula, factorions and HQ9+
* Deterministic `now` builtin with updated VM outputs
* Dart transpiler gains slice support, BigInt indexing and block lambdas
* C# adds delegate calls and literal match cases with improved indexing
* C++ supports const maps, auto params and benchmark timing helpers
* TypeScript compiler introduces async fetch and map containment
* Python enumerations and environment variables improved

### Changed

* Rosetta progress index regenerated with refreshed outputs across languages
* Documentation clarified for the Computer Zero interpreter

### Fixed

* Various closure ordering and map field bugs across backends
## [0.10.41] – 2025-07-26T12:01:29+07:00

### Added

* Mochi solutions for many new Rosetta tasks including Dijkstra's algorithm and dining philosophers
* Slice builtin and forward references in the VM with else-if and bigrat cast support
* Transpiler updates across Go, F#, Scala, Swift, TypeScript, Java and C++ with new builtins and benchmark features
* Benchmarks and golden outputs refreshed across languages

### Changed

* Improved index handling and casting logic in multiple backends
* Documentation and progress logs updated for various transpilers

### Fixed

* Numerous compiler issues such as foreach handling in Go and int64 casts in F#
## [0.10.40] – 2025-07-25T10:01:54+07:00

### Added

* Benchmark blocks and modes across many transpilers with memory measurement
* Union pattern matching for Racket with improved map, list and struct handling
* Extended BigInt and BigRat helpers plus numerous new Rosetta programs

### Changed

* `tap-gist` CLI rewritten using `fang`
* Progress tables and golden outputs refreshed

### Fixed

* Benchmark output duration handling for PHP and assorted compiler bugs
## [0.10.39] – 2025-07-24T11:43:58+07:00

### Added

* BigInt operations in Dart with initial Go support
* Balanced ternary, calendar helpers and struct methods in TypeScript
* Deterministic `now` for PHP and C with slice defaults in C++
* Null handling, generic casts and improved strings for Scala
* `lower()` builtin and repeat helpers in ST
* Partial returns and list concatenation in Prolog
* Input builtins and union variants across Pascal, Swift, Scheme and Zig
* Map generics and nested indexing for Kotlin plus open-ended slices in F#
* Numerous Rosetta examples regenerated

### Changed

* Progress logs updated with index numbers across languages
* Golden outputs refreshed across the repository

### Fixed

* Improved type inference for Haskell and Scala
* PRNG overflow fixed in C# with map and index handling bugs resolved
## [0.10.38] – 2025-07-23T10:17:23+07:00

### Added

* Global variable support for Rust and struct pattern matching in Ruby
* Additional Rosetta examples across C#, Lua, Racket and Swift
* Map key handling for TypeScript with selector calls for Zig
* Local variable tracking and typed call arguments in multiple backends
* `padStart` helpers for Python, Ruby and Lua with integer and membership utilities
* List parameter and nested literal support across transpilers
* `net.LookupHost` for Erlang and OCaml plus map indexing in Swift and PHP

### Changed

* Improved type inference for Kotlin and container metadata in Scala
* Enhanced C++ variable management and indentation fixes in C#
* Improved map handling for Java with additional Rosetta programs
* Progress checklists updated across languages
* Rosetta outputs regenerated and stored with source code

### Fixed

* Deterministic `now` seeding for C++
* Global variable rename issues in Rust
* Miscellaneous compilation bugs across languages
* Fixed string slice handling in the C# transpiler
* Various minor issues across PHP and Ruby backends

## [0.10.37] – 2025-07-22T17:53:32+07:00

### Added

* Rosetta golden tests for C, Go, Python and Clojure
* Global variables with save/update statements in the Scala transpiler
* Union support started for the C++ `tree_sum` example
* JSONL load/save and update features for Ruby
* Nested list and map assignments across Prolog and Haskell
* Basic map support in the C backend with a `load_file` helper for Racket
* Save and update capabilities added to Swift
* Special join logic and mix import handling for Python

### Changed

* Partial application and query sorting improved in Scala
* Map literal inference and boolean `in` operator fixes in TypeScript
* Golden outputs refreshed with progress logs updated across languages

### Fixed

* Environment variable path bug in the Scala transpiler
* Formatting cleanup and obsolete error files removed

## [0.10.36] – 2025-07-22T16:14:06+07:00

### Added

* Advanced group query typing for Scala with JSONL and YAML support
* Update statements in Go and enhanced dataset joins in C and C++
* Dynamic maps, JSON builtins and struct features across OCaml and Pascal
* FFI integration for Zig with dataset loading and right join support in Erlang

### Changed

* Join inference and optional handling refined across Python, TypeScript and C++
* Documentation checklists and progress logs refreshed across languages

### Fixed

* Nested loop closure bug in the C transpiler
* Various join and query issues with regenerated golden outputs

## [0.10.35] – 2025-07-22T10:43:45+07:00

### Added

* Int-keyed maps in the C transpiler
* Sum query support in PHP and JSONL load improvements for Elixir
* Group query sorting with HAVING in Python
* Save statements and stable sort in Lua with left joins in Erlang
* `group_by_multi_sort` across Kotlin, Pascal and Swift plus new loop helpers

### Changed

* TypeScript compiler cleaned up with regenerated outputs
* Progress logs and tasks refreshed across languages

### Fixed

* Query and dataset bugs across multiple backends
* Miscellaneous documentation issues

## [0.10.34] – 2025-07-21T19:10:17+07:00

### Added

* Struct casts for the Java transpiler
* More join operations across language backends

### Changed

* Golden outputs regenerated for multiple transpilers
* Boolean, float and map handling improved
* Progress logs refreshed across languages

### Fixed

* Assorted formatting and build issues

## [0.10.33] – 2025-07-21T15:42:34+07:00

### Added

* Query operations including group by, joins and sorting across transpilers
* JSON and map builtins with typed struct inference

### Changed

* Membership operators and numeric inference refined
* Documentation and task logs updated across languages

### Fixed

* Group counting and join ordering bugs
* Duplicate task entries removed

## [0.10.32] – 2025-07-20T15:13:08+07:00

### Added

* Experimental transpiler framework supporting many languages
* AST printing helpers and golden tests for transpiled programs
* Map operations, joins, slicing and membership across languages

### Changed

* Code generation improved with typed variables and header comments
* Documentation and progress logs reorganized across compilers

### Fixed

* Runtime string membership issues
* Go and C build/test stability

## [0.10.31] – 2025-07-19T10:21:06+07:00

### Added

* New Mochi examples such as Deconvolution-1D and Day-of-the-week
* Rosetta golden tests expanded and sorted for determinism
* TypeScript runtime `input` helper for Deno

### Changed

* Kotlin functions now return values
* Scala and Swift handle return statements and list casts
* Go manages top-level variables in `main`
* C and C++ extend builtins and inference
* Rust and Python improve constant handling and DNS lookup

### Fixed

* Go backend `len(any)` and global variable issues
* Ruby and Java index inference bugs
* READMEs refreshed across languages
## [0.10.30] – 2025-07-18T18:44:32+07:00

### Added

* Rosetta Code solutions expanded across datasets and tasks
* `input` and `now` builtins for Racket, Java, F#, Swift and Pascal
* Basic loop constructs in Erlang
* Virtual machine global variable support

### Changed

* Go compiler handles typed append and string conversions
* Kotlin supports float casts and safer `now` builtin
* Scala, C# and other backends sanitize identifiers and improve inference
* Documentation and golden outputs refreshed across languages

### Fixed

* C backend capture assignment handling corrected
* Balanced bracket and other example fixes
* Python backend manages bare returns and global variables

## [0.10.29] – 2025-07-20T09:45:10+07:00

### Added

* Python bigint operations with new machine tests
* Zig range index inference and direct substring slices
* C# query map struct inference
* Fortran TPC-H query 6 example
* Dataset golden outputs refreshed across languages

### Changed

* Kotlin improves sort inference and map comparators
* Go option field inference enhanced
* F# compiler handles import modules and selector methods
* OCaml inlines sum operations and refines union types
* Lua runtime simplified with helper removal
* C backend infers group keys and constant strings
* Erlang tracks nested map fields

### Fixed

* Variant constant loops corrected in the C backend
* Clojure outer join generation without query helper
* Documentation updates for Fortran and Elixir

## [0.10.28] – 2025-07-18T08:39:31+07:00

### Added

* TPC-H dataset golden outputs regenerated across languages
* Machine outputs for Zig, F#, Java and others
* Deterministic `values` builtin for Go
* `string_prefix_slice` support in Zig
* JSON builtin for OCaml
* Precomputed aggregates for constant lists in the C backend

### Changed

* Struct inference improved across C#, Kotlin, Scala and Rust
* Go and Scala optimize joins and indexing
* Lua, Pascal and Erlang refine printing and quoting
* C backend handles map literal pair grouping
* Inlined list operations across Prolog and Elixir
* Scheme infers numeric assignments

### Fixed

* Fortran folds membership and length for constants
* String list inference corrected in the C backend
* Outer join handling fixed in Go
* Machine README checklists updated

## [0.10.27] – 2025-07-16T22:58:30+07:00

### Added

* Rosetta Code golden tests across compilers with VM runner
* Deterministic `now` builtin for reproducible timestamps
* `sha256` builtin for the C backend
* Virtual machine global variable support and entry aliasing
* Struct literals supported in the Smalltalk backend

### Changed

* PHP and Kotlin escape reserved names and keywords
* Go and C++ improve indexing and collections
* Ex compiler adds length helper; Lua provides FFI print helpers
* C backend aliases `main` to avoid duplicate entries

### Fixed

* Fortran newline strings and void function handling
* Outer join sort corrected for TypeScript
* Join length calculation fixed in the C backend
* Faster compilation for packages with slow build tags

## [0.10.26] – 2025-07-14T19:44:27+07:00

### Added

* JOB dataset queries compiled across languages with golden tests
* `starts_with` builtin supported across Python, Prolog, PHP and Lua

### Changed

* Python dataclasses unify field types and provide iteration helpers
* Go compiler prints typed lists and infers structs for query selects
* TypeScript simplifies group-by logic with spread-based appends
* C backend tracks stack arrays and emits static arrays for struct lists
* C++ and Java refine struct references and dataclass types

### Fixed

* Numeric casts corrected for TPCH query 1 in Go
* Cross join output and helper comments fixed in the C backend
* Query loop formatting improved for TypeScript
* Dataclass float handling updated for Python

## [0.10.25] – 2025-07-13T09:42:07+07:00

### Added

* TPCH queries 1–22 compiled across languages with golden outputs
* Deterministic version headers for reproducible builds
* New scripts and artifacts for dataset compilation

### Changed

* Zig infers structs for query results with improved group slices
* C#, Java and Kotlin refine equality, comparisons and numeric handling
* C and C++ simplify initialization with better match defaults
* Rust and Python improve float casting and typed dataclasses

### Fixed

* C backend list helpers with dynamic loops
* Build headers consistent across machines
* Updated machine checklists for Go, Rust and others


## [0.10.24] – 2025-07-12T16:18:37+07:00

### Added

* Numerous new Rosetta Code tasks including Barnsley fern and Benford's law
* TPC-H query 1 tests with updated dataset outputs
* Bool list arrays and list length helpers in the C backend
* JSON helper for TypeScript and typed grouping for C#

### Changed

* TypeScript compiler checks globals, preserves doc comments and uses interface-based structs
* Scala compiler improves group and join generation with option types
* Kotlin, Java and Go compilers refine join handling and map indexing
* Python, C++ and Rust compilers enhance type inference and formatting

### Fixed

* Fortran query line continuations and circular prime example
* Java map assignment bugs and TypeScript map loop issues
* Dart numeric casts and C++ aggregate inference

## [0.10.23] – 2025-07-11T15:23:14+07:00

### Added

* Basic Go and Python FFI support in the virtual machine
* Builtin module infrastructure for the C backend with JSON struct support
* Numerous Rosetta Code tasks including 15-puzzle, Active Directory search and arithmetic examples
* TypeScript compiler generates typed queries and supports builtin imports
* Rust and Java compilers handle FFI imports with improved output

### Changed

* Automatic struct inference across C#, Kotlin, Python, Scala, Swift, Java and Go
* C backend uses `calloc` with allocation checks and stack-based list literals
* Go compiler features sprint helper and improved variable naming
* C++ and Zig compilers improve type inference and constant string operations
* Java compiler provides descriptive class names and better struct naming

### Fixed

* Java compiler map handling and helper issues
* Scala map membership bugs and case class derivation
* Swift group query logic
* Rust `in` operator semantics and environment typing
* Python compiler map literal handling
## [0.10.22] – 2025-07-10T17:45:54+07:00

### Added

* Go FFI MD5 example with tests
* Outer and right join support for Java and Kotlin
* Nested map assignment and union pattern support in the C backend
* COBOL map operations and struct literals
* Prolog update statements and test blocks with load/save builtins
* YAML parser builtin for Dart and YAML/JSONL runtime support for Rust

### Changed

* Improved type inference across Go, OCaml, Scala, Swift and others
* Better printing and formatting across Rust, Zig and Smalltalk compilers
* Automated README generation with refreshed machine outputs
* Enhanced join handling and sanitized names in the C backend

### Fixed

* Zero-arg call evaluation in Smalltalk and variable tracking in Scala
* Map field casts and join query issues across Swift and Java
* Removed obsolete error files and refreshed translations

## [0.10.21] – 2025-07-10T09:38:20+07:00

### Added

* Join and group-by support across C, C++, Pascal, Kotlin, Swift and more
* String map handling and group-by string support for the C backend
* Java compiler load/save and update/test features
* Map size, max and json builtins for Haskell, Zig and Kotlin
* YAML loading capability for the Scheme compiler

### Changed

* Improved type inference and runtime helper reuse across compilers
* Optimized join queries with captured closure support

### Fixed

* Refresh machine outputs and examples across languages
* Minor dataset and YAML test corrections

## [0.10.20] – 2025-07-09T10:05:52+07:00

### Added

* Cross join support for the C backend with join features in TypeScript, Go and Elixir compilers
* Refactored Python compiler for idiomatic output
* Updated machine outputs across languages

### Changed

* Numeric handling improvements and formatting cleanups across backends

### Fixed

* Python group_by_left_join output and Ex join regression
* Removed outdated error artifacts


## [0.10.19] – 2025-07-08T08:40:55+07:00

### Added

* Simplified Lua compiler with initial outputs
* Basic query and loop support for the F# compiler
* Extensive translations for dataset and join programs across languages

### Changed

* Go, Haskell, Swift, PHP, Erlang and Dart compilers handle loops, indexing and closures
* C, C++, C# and PHP backends inline builtins with string helpers
* READMEs converted to checklists with updated translation progress

### Fixed

* Dart compiler buffer logic and function types
* Elixir print output and Zig average builtin
* Miscellaneous dataset and test updates across languages

## [0.10.18] – 2025-07-07T08:14:56+07:00

### Added

* Manual translations for example programs across languages such as Haskell, C, Zig, Swift, Smalltalk, Scheme, Scala, Prolog, PHP, Pascal, OCaml, Kotlin, Lua, F#, Fortran, Elixir, Clojure, Dart, C#, C++, TypeScript and Python
* VM roundtrip tools compare compiler outputs with the Mochi VM

### Changed

* Improved type inference across C, Rust, Zig and Pascal compilers with extra hints
* Golden tests regenerated and validated via the VM

### Fixed

* Minor translation and dataset comparison issues

## [0.10.17] – 2025-07-06T14:31:29+07:00

### Added

* CLI-based AST parsers for languages such as C, C++, Java, Kotlin, Scala and Rust
* Fallback parser resolves files from the repository root
* Converters support loops, structs, interfaces, slices and generics

### Changed

* Converters reorganized into subpackages with improved diagnostics
* Improved translations for Go, Python, TypeScript, Dart, Ruby, Pascal, Lua, Prolog, Smalltalk, C# and PHP
* Golden outputs and build scripts refreshed

### Fixed

* Build issues and `go vet` errors
* VM golden files updated with tests marked slow
## [0.10.16] – 2025-07-05T17:01:44+07:00

### Added

* Runner protocol with sandbox containers and unit tests
* Any2mochi converts Python, Go and TypeScript with server support
* Node-based ts2mochi and basic Python converter
* C backend `min`/`max` builtins
* SQL Logic Test group-by cases 300–499 and aggfunc support
* sqlite2duck CLI with extended conversions
* Doom example movement helpers

### Changed

* go2mochi and py2mochi handle more constructs with improved snippets
* SQL Logic Test generator processes SELECT ALL and distinct cases
* sqlite2duck conversions expanded with time/date and hex functions
* Conversion tests consolidated across languages
* Scala map field access and Fortran string builtins

### Fixed

* Clojure compiler errors and Deno tests
* py2mochi list slice formatting
* Miscellaneous converter issues

## [0.10.15] – 2025-07-04T22:41:30+07:00

### Added

* Map update statements across Go, C, C#, C++ and TypeScript
* `reduce` builtin for Go and C backends with optimized count and exists
* SQL Logic Test group-by, aggregate and expression cases
* sqlite2duck conversion tool and tests

### Changed

* Typed helpers and improved type inference across Go, C++, Python and Zig
* Inline helpers and case builtin optimizations
* Deterministic table order for SQL Logic Test generation

### Fixed

* Numeric operation errors in the virtual machine


## [0.10.14] – 2025-07-04T10:49:16+07:00

### Added

* SQL Logic Test generator covers select3 cases 1–3320 with numeric separator support
* Nested if-then-else golden tests
* Documented case1400–case1499 generation

### Changed

* Regenerated select1 cases with improved type detection
* Reduced parentheses in generated code

### Fixed

* Coalesce expressions now correctly parenthesized



## [0.10.13] – 2025-07-03T20:54:57+07:00

### Added

* SQL Logic Test generator covers select2 cases 1–1000 with rowsort, coalesce and IS NULL support

### Changed

* Treated `nil` strings as null with improved boolean detection
* Preserved column types when nulls are present and reformatted logs
* Regenerated SLT outputs with cleaner errors

### Fixed

* Virtual machine now propagates nulls in arithmetic and `avg` operations
* Corrected rowsort handling across test cases

## [0.10.12] – 2025-07-03T10:13:46+07:00

### Added

* SQL Logic Test generator covers select1 cases 1–1000 with update statement support across compilers
* Real TPC-DS queries 50–59 included with updated examples

### Changed

* Improved SLT parsing of boolean strings, unary and parenthesis expressions
* Deduplicated VM constant registers and fixed sorted query allocation
* Normalized float constants with extended runner timeout and logging

### Fixed

* Subquery caching issues in SLT runs
* Sorting value overwrite bug and miscellaneous generator fixes

## [0.10.11] – 2025-07-02T08:31:19+07:00

### Added

* Full TPC-DS query coverage with golden outputs across languages
* `first` builtin for Ruby and Elixir
* `exists` builtin for Python, TypeScript and Elixir with `append` support
* Grouped sort operations in Prolog and Scheme
* Join helpers for F# and null literal handling in Scheme

### Changed

* Updated TPC-DS IR outputs and dataset tests
* Documentation updates for Pascal coverage
* Additional golden files for C++, Go and Java compilers

### Fixed

* Query 39 variance calculation
* Racket group query parentheses
* Miscellaneous dataset example fixes

## [0.10.10] – 2025-07-01T18:41:01+07:00

### Added

* Sample TPC-DS dataset with queries 1–19 across languages
* `first` builtin in Python, TypeScript and Go
* `union` and `union_all` operations in Go and Python
* `matrix_mul` builtin in the C backend and `if` expressions for C++
* Typed map elements and join-group support in C#

### Changed

* Compiler tests marked slow with optional golden skips
* Updated golden outputs for F#, Java, Rust and Zig compilers

### Fixed

* Kotlin query argument casting bug
* Zig backend grouping issue with joins
* Elixir TPC-DS query 1 test case
* Miscellaneous fixes after helper additions

## [0.10.8] – 2025-06-30

### Added

* Numeric string parsing for generic operations
* Reverse builtin with nested map lookup and cycle-safe printing
* Expanded TPC-DS query examples

### Changed

* Constant register reuse and canonicalized float constants
* Optimized union operations with updated IR and benchmarks

### Fixed

* Boolean compile errors and row map handling in groups
* `toFloat` null cases and safe slice indices
## [0.10.7] – 2025-06-30T11:05:48+07:00

### Added

* Hashed outer and right joins
* Pure call folding for builtins
* TPC-DS queries with data for 1–99

### Changed

* Optimized group-by and join map allocation
* Registers compacted with deduplicated constants

### Fixed

* Map register handling and len(null) cases

## [0.10.6] – 2025-06-29T12:36:00+07:00

### Added

* Matrix multiplication and join benchmarks
* Hash join and early exit optimizations
* Updated JOB and TPC-H IR outputs
* Documentation and VS Code LSP enhancements

### Fixed

* Map constant register allocation bug

## [0.10.5] – 2025-06-27T20:39:39+07:00

### Added

* Join Order Benchmark dataset queries and golden tests across languages
* `starts_with`, `like`, `min` and `max` builtins in the VM
* JOB query examples 16–33
* Map key inference and contains improvements

### Changed

* VM constant folding re-enabled with interpreter removed
* Null ordering handled during comparisons

### Fixed

* C++ map literal inference
* Zig operator precedence
* `starts_with` edge cases
* JOB C++ compilation issues


## [0.10.4] – 2025-06-27T14:33:48+07:00

### Added

* Bytecode optimizer now performs constant folding and dead code elimination using liveness analysis
* Formatter integration across all language backends
* JOB dataset task list for benchmarking

### Changed

* Improved VM disassembly and toolchain updates
* All compiled functions are optimized automatically

### Fixed

* `union_all` parsing bug
* Append liveness issue
* Normalized golden outputs

## [0.10.3] – 2025-06-26T18:37:31+07:00

### Added

* Typed `fetch` builtin with HTTP options and struct casting
* Dataset load and save support for JSON across languages

### Fixed

* Type inference bug for dataset load generics
* Pascal string quoting issue and golden file updates
* Erlang fetch runtime corrections

## [0.10.2] – 2025-06-26T12:39:20+07:00

### Added

* `values` builtin with source context stack traces in the VM
* `sum` helpers and group-by filters across backends
* Query `let` clause and new TPC-H examples

### Changed

* CLI no longer executes code via the interpreter

### Fixed

* Binary operator precedence bug in the VM
* TPC-H query corrections and compiler updates

## [0.10.1] – 2025-06-26T08:03:05+07:00

### Added

* Map sorting and HAVING clause support in the VM
* Aggregate queries with `exists`, `contains` and `substring` builtins
* Test blocks and `expect` statements across compilers
* `sum`, `min` and `max` helpers in many backends
* CLI `run` now defaults to the VM

### Changed

* Improved VM disassembly and C++ JSON escaping

### Fixed

* TPC-H query syntax corrections and Java map key handling

## [0.10.0] – 2025-06-25T20:05:45+07:00

### Added

* Dataset query sorting, joining, grouping and pagination across backends
* Distinct queries in Dart with dataset load/save for C++
* Predicate pushdown in the VM and data planner
* Compilers push down where filters with skip/take optimization

### Changed

* Improved dataset casting in Kotlin

### Fixed

* PHP query environment capture
* COBOL print indentation


## [0.9.5] – 2025-06-25T09:23:00+07:00

### Added

* Relative data paths and `fetch` expression in the VM
* Map iteration with `keys` builtin across languages
* Nested struct and type declarations
* Functions as parameters and return values across backends
* Prolog backend supports passing and returning functions
* Linter library integration with the CLI

### Changed

* Consolidated type inference across backends

### Fixed

* Parser type duplication and duplicate builtin cases
* Map membership bug in the Lua compiler
* If expression handling in the VM

## [0.9.4] – 2025-06-25T00:02:13+07:00

### Added

* Typed dataset loading in the VM
* Group query left joins with group-by
* String indexing, slicing, membership and concatenation across languages
* `upper` and `lower` string builtins
* `union_all` list operator support

### Changed

* TinyGo build stubs and playground updates

### Fixed

* Right join ordering bug in the VM
* Prolog and C++ compiler tests

## [0.9.3] – 2025-06-24T22:34:20+07:00

### Added

* String indexing and concatenation in the VM
* `min` and `max` builtins
* Join, multi join and left join dataset queries
* `sort`, `skip`, `take`, `load`, `save` and `group by` operations
* List set operators in C, Dart and Elixir
* List operation inference in the C# compiler
* Basic query expression compilation with identifier map keys
* Erlang tests for list operators

### Changed

* Benchmarks refreshed

### Fixed

* Identifier map keys handled correctly in the VM

## [0.9.2] – 2025-06-25

### Added

* Specialized numeric bytecode with CFG-based register inference
* Dynamic map literals, membership and iteration
* Builtins `append`, `str`, `input`, `count`, `avg` with extended `len`
* Partial application, tail call optimization, unary negation
* Nested index assignment and list set operations
* Error stack traces from the VM
* `vmreport` command and VM test runner
* Nested functions in C and exported functions in Zig
* Functions and lambdas in the PL backend
* Scala infers function expression types

### Changed

* VM enforces variable declaration on assignment
* Benchmarks updated without PyPy or Cython

### Fixed

* Ruby lambda helpers propagate correctly

## [0.9.1] – 2025-06-24

### Added

* Function expressions and generic calls in the VM
* Function support in the pure Mochi interpreter
* Simple function support for the JIT
* Map indexing operations

### Changed

* REPL simplified without Bubble Tea UI and displays evaluation results
* Human-readable call disassembly in the VM
* Benchmarks updated with IR and C outputs
* TypeScript runtime computes globals at runtime

### Fixed

* Corrected IR disassembly comment placement
* Resolved VM `now` builtin bug
* Refreshed REPL prompt and output handling
* Fixed C benchmark outputs and runtime compilation

## [0.9.0] – 2025-06-23

### Added

* Official `std/` standard library packages (`math`, `strings`, `time`, `list`, `set`, `uuid`, `fmt`, `test`)
* CLI autoload support and documentation extraction for standard packages

### Changed

* Explicit `export` requirements for library functions
* Improved package import paths for `std/` modules

### Fixed

* Minor package import bugs and builtin function errors

## [0.8.10] – 2025-06-22

### Added

* Agent support for C# and Dart
* Right join in Ruby with left/right joins in C# and join helpers in Erlang
* Query sorting, pagination and advanced join operations in Clojure
* CSV, JSON and YAML dataset helpers in Elixir, Kotlin and Dart
* Extern declarations for Kotlin, Rust and the C backend
* Now and JSON builtins across C, C++, Dart and Swift
* Eval builtins in Ruby and C#
* Fetch expression support in Swift and new fetch options in Erlang
* Step range loops for Zig with constant and descending ranges in COBOL
* Multi-dimensional slice assignments in Racket and Fortran
* Open-ended slices, print/call statements and abs builtin for COBOL
* Map membership and struct support in Zig with map-key iteration in C++
* Union/extern type support in the C backend and union_all in Java
* Push/keys builtins in Scheme and generative dataset helpers in F#
* Logic programming support for Prolog and an input builtin in OCaml

### Changed

* Compiler codebases reorganized with smaller files and runtime helpers

### Fixed

* Average builtin and Pascal temporary variable issues resolved
* Fortran golden tests corrected
## [0.8.9] – 2025-06-21

### Added

* Sort builtin for Rust and join queries in Ruby
* Imports and multi-dimensional indexing in Racket
* Package declarations and Python FFI imports for Scala
* Map loops, len and reduce builtin in Zig
* Python agent initialization with fields
* Group queries in Lua with struct support for the C backend
* List slicing and negative indexing in COBOL
* JSON helpers and reduce builtin for Kotlin
* Type method support with generic map casting in C#
* Block-bodied lambdas in Racket and list ops in Scheme
* Inline avg builtin for Fortran
* Reduce builtin in C++ and Prolog test blocks

### Changed

* Runtime helpers are deduplicated and inlined across languages

### Fixed

* Method handling corrected in the Clojure compiler

## [0.8.8] – 2025-06-20

### Added

* Dataset `fetch`, `generate`, `load` and `save` across Ruby, Erlang, Elixir,
  Java and Scheme
* Method declarations in Ruby and Dart with type methods for Kotlin
* Test block support for Scala, Java, F#, Zig, Swift and C
* Negative list indexing in Swift and Scala
* `sort by` queries in C++ and `where`/`skip`/`take` in F#
* JSON helpers and a `now` builtin in Haskell, Kotlin, Zig and F#
* Pagination added to the Rust compiler

### Changed

* F# runtime moved to the preamble
* Documentation updated for multiple backends

### Fixed

* Minor README corrections and refreshed C++ examples
## [0.8.7] – 2025-06-19

### Added

* Break and continue support in the Scala compiler
* COBOL loops with grouped ranges, negative counts and modulo operations
* Struct casts and new examples in the Ruby backend
* Dart trailing newline emission and map/list helpers
* 64-bit literal support for Fortran
* Smalltalk golden tests and parenthesized expressions
* Expanded Rust factorial and fibonacci tests

### Changed

* Clojure only includes runtime helpers when needed
* LeetCode solutions refreshed for Fortran

### Fixed

* Improved printing and loop handling across compilers
## [0.8.6] – 2025-06-19

### Added

* LeetCode examples 1–5 compile across all experimental compilers
* New runners for Kotlin, Swift, Dart, Racket and Erlang
* String slicing and concatenation in C, C++, Fortran, F#, Lua and more
* Float and cast support in Scheme, Clojure, Fortran and others

### Changed

* Golden outputs refreshed and output naming unified
* `leetcode-runner` validates toolchains and allows custom SWI-Prolog path

### Fixed

* F# string indexing and Clojure concatenation issues
* Java underscore handling and COBOL generation bugs
* Erlang branch formatting and assorted compiler errors

## [0.8.5] – 2025-06-18

### Added

* LeetCode example tests for problems 1–3 across all compilers
* Build command supports all targets at once
* Improved .NET and PHP installation scripts

### Changed

* Compilers gain variable assignment, while loops and break/continue support
* List and string operations expanded in Rust, Scheme, Java and others

### Fixed

* Fortran and Pascal compiler issues
* Prolog if-block handling and C# golden outputs

## [0.8.4] – 2025-06-18

### Added

* Two Sum example tests across many compilers
* Prolog early return support and if-else
* Pascal boolean literals and reserved keyword tests
* Scheme multi-argument print and Racket else clauses
* F# function expressions
* Zig backend auto-installs when missing

### Changed

* Swift uses `let` for immutable parameters
* Go `count` builtin improved
* Golden tests refreshed for several languages

### Fixed

* Smalltalk and Elixir compiler issues
* Erlang variable naming corrections

## [0.8.3] – 2025-06-18

### Added

* CLI exposes all compiler targets and accepts `python` alias
* Basic Zig backend with golden tests
* READMEs for every experimental compiler backend
* Cross-platform installation helpers for many languages
* Union types, match expressions and closures across backends
* Golden tests expanded for C, COBOL, Pascal, Prolog, Fortran and more

### Changed

* Python golden outputs removed from other compilers
* C++ backend gains range loop support
* Improved dotnet and Rust setup on macOS and Linux

### Fixed

* C compiler average function and list concatenation
* Assignment handling in Clojure backend
* Erlang boolean literals
* Ruby query printing
* Smalltalk backend compilation

## [0.8.2] – 2025-06-18

### Added

* Minimal compilers for Clojure, COBOL, Fortran, OCaml, Pascal, Prolog, Scheme, Racket and Zig
* Cross join and sort/skip/take queries in C#, Dart, Elixir and Erlang
* Negative string indexing and slicing in Java, Swift and Lua
* Map literals and membership checks in Java, Scala and Swift
* `str`, `input`, `count` and `avg` builtins across more backends
* Build command handles every compiler target
* Benchmarks compile C output
* Golden tests for all compilers

### Changed

* CLI no longer references the removed Clojure integration

### Fixed

* Scala test runner detection
* Lua integer division uses `//` when possible

## [0.8.0] – 2025-06-19

### Added

* Experimental compiler backends for C, C#, Dart, Erlang, F#, Haskell, Java, Lua, PHP, Ruby, Scala, Swift and Rust
* Support for variable assignment, while loops, break and continue across the new backends
* Builtin functions `str`, `input`, `avg` and `count`
* Map and string iteration in C, Dart, Java and Rust
* Basic query operations in Kotlin and Erlang with dataset queries in C# and Ruby
* Struct support in Ruby, nested functions in Lua and parameter mutation in Swift
* Membership checks in Elixir and PHP loops
* Litebase storage client for the runtime
* Golden tests for the new compilers
* LeetCode 402 example

### Changed

* Heavy compiler tests are skipped by default
* Documentation lists all compile targets

### Fixed

* Kotlin golden tests and F# indentation issues


## [0.7.6] – 2025-06-18

### Added

* Primitive casts in Go and Python compilers
* Typed empty list assignment and concatenation in Go
* Map key casts and typed struct field hints
* Basic group-by in Go and TypeScript
* `input` builtin for reading lines
* LeetCode 401 example

### Changed

* Go compiler infers empty literal types in equality
* Benchmarks regenerated with updated results

### Fixed

* Argument count validation for builtins
* Forward type declarations in the type checker
* Float literal handling and return hints in the Go compiler
* Name conflicts in Python and Go tests


## [0.7.5] – 2025-06-17

### Added

* Union list and slice assignment in the Go compiler
* Map literals with type hints
* Python and TypeScript output for LeetCode 128
* Palindrome golden test

### Changed

* LeetCode outputs reorganized under language folders
* Makefile builds run in parallel with extra commands

### Fixed

* Reserved keyword handling in the Go compiler
* Empty container literals in let statements
* Pattern match casts, list concatenation and while loops


## [0.7.4] – 2025-06-16

### Added

* LeetCode solutions 241–400 with Python and TypeScript outputs
* Go versions compiled for problems 361–400
* Numeric comparisons across integers and floats
* Slice concatenation support
* Nested functions and globals in the TypeScript compiler
* Go compiler arithmetic with `any` and empty literal assignments

### Fixed

* Stable float formatting in the Python compiler
* Random Pick Index and LeetCode 321 examples

## [0.7.3] – 2025-06-15

### Added

* LeetCode solutions 101–240 with improved examples
* Operator precedence handling in the Go compiler
* Documentation clarifies string comparison semantics

### Changed

* Rewrote the LRU cache example for clarity

### Fixed

* Python compiler precedence and reserved name handling

## [0.7.2] – 2025-06-14

### Added

* LeetCode solutions 1–100 with Makefile and Sudoku example
* `mochi test` now accepts directories with `...`
* Documentation on common language errors

## [0.7.1] – 2025-06-13

### Added

* `str`, `count` and `avg` builtins across compilers
* Index assignment for lists and maps
* `in` operator checks map membership
* Empty map initialization

### Changed

* Maps allow integer keys and infer iteration
* TypeScript inlines `len` for typed arrays and strings
* Removed dynamic iteration helpers

## [0.7.0] – 2025-06-13

### Added

* `eval` builtin for dynamic code execution
* Stream-backed mailboxes for agents
* Go, Python and TypeScript package listing helpers
* Go FFI supports struct types and methods
* Local file imports with `./` and `../`

### Changed

* Removed `sleep` builtin

## [0.6.3] – 2025-06-13

### Added

* `package` and `export` keywords for modules
* Default alias when `import` has no `as`
* `init` and `get` commands for the CLI
* Dataset `load` and `save` in Go, Python and TypeScript
* `libmochi` helpers for Go, Python and TypeScript
* Parser captures documentation comments

### Changed

* WebAssembly interpreter built on demand

### Fixed

* TypeScript package import declarations

## [0.6.2] – 2025-06-12

### Added

* `import` statement and FFI runtimes for Go, Python and TypeScript
* Deno FFI runtime for TypeScript
* JSON and YAML dataset loaders

### Changed

* Consolidated FFI logic under `runtime/ffi`

### Fixed

* Notebook magic path test
* TypeScript remote import test


## [0.6.1] – 2025-06-11

### Added

* `load` expression for CSV and JSONL files
* `save` statement for writing datasets
* STDIN/STDOUT support for load/save
* Foreign function interface and `extern object`
* Optional DuckDB query engine
* `--wasm-toolchain` flag

### Changed

* Compiler type inference improvements

### Fixed

* Build without DuckDB
* Updated golden tests

## [0.6.0] – 2025-06-10

### Added

* Dataset queries with `from`, `where`, `select` and `group by`
* Join operations: `join`, `left join`, `right join`, `outer join` and cross joins
* `sort by`, `skip` and `take` clauses for datasets
* In-memory dataset driver and runtime library
* Dataset support in Go, Python and TypeScript compilers
* WebAssembly compiler
* `%%mochi` Jupyter cell magic
* List set operators `union`, `union all`, `except` and `intersect`
* Vector search runtime with a flat index

## [0.5.1] – 2025-06-09

### Added

* `cheatsheet` command for language reference
* `--memo` flag to memoize pure function calls
* `--aot` flag for ahead-of-time constant folding
* Interpreter folds constants in `let` declarations
* Go compiler memoizes pure calls
* Python compiler emits type hints
* TypeScript compiler annotates variables and functions
* Agent stream example
* Compilers skip async overhead when no streams


## [0.5.0] – 2025-06-09

### Added

* `agent` blocks with persistent state and `intent` functions
* Agent runtime integrated with the interpreter
* String indexing and iteration
* `in` operator for string containment
* `while` statement for loops
* Compiler support for agents, strings and `while` in Go, Python and TypeScript

### Fixed

* Race condition in agent runtime

## [0.4.0] – 2025-06-08

### Added

* `stream` declarations for typed event flows
* `emit` statements for sending events
* `on` handlers for asynchronous processing
* Runtime library queuing and replay
* Go, Python and TypeScript compiler support
* Interpreter with async stream handlers
* VS Code extension for syntax highlighting and agent integration

### Changed

* Stream runtime refactored with interfaces
* Syntax highlighting updated for new keywords

### Fixed

* Go compiler deadlock in stream handler

## [0.3.5] – 2025-06-07

### Added

* `fetch` expression for HTTP requests with typed JSON
* `with` options for method, headers and body
* Python and TypeScript compiler support for `fetch`

### Changed

* Type checking for fetch options

### Fixed

* Minor runtime improvements for HTTP requests

## [0.3.4] – 2025-06-07

### Added

* Tool calling via `tools` field in `generate` blocks
* Inline methods defined inside `type` blocks
* Basic union types with pattern matching

### Changed

* Tool descriptions exported in schemas

### Fixed

* Various improvements to tool call handling

## [0.3.3] – 2025-06-06

### Added

* `match` expression with pattern cases
* Logical operators `&&` and `||`

## [0.3.2] – 2025-06-06

### Added

* `model` block for reusable model aliases
* `model` field in `generate` blocks
* `generate embedding` block producing vectors
* Embedding support with optional `normalize`

### Changed

* Generate blocks call the shared LLM runtime
* Removed interpolation in generate fields

### Fixed

* Documentation and tests for generative features


## [0.3.1] – 2025-06-05

### Added

* Structured `generate` blocks return typed structs
* Optional fields `temperature`, `top_p`, `max_tokens` and `stop`
* `type` declarations and struct literals
* Nested field selectors with type checking
* Python compiler support for `test` blocks
* JSON schema inference for `generate`

### Changed

* LLM parameters passed via `Params` map
* Providers honor `ResponseFormat`
* Cheatsheet updated with examples

### Fixed

* Type errors for struct fields
* Minor provider issues


## [0.3.0] – 2025-06-05

### Added

* `stream` declarations for event types
* `on` handlers and `emit` for dispatching events
* `agent` blocks with persistent `state` and `intent` functions
* `generate text` expression with pluggable LLM providers

### Changed

* MCP server reports version 0.3.0
* Documentation and cheatsheet updated

### Fixed

* Minor issues discovered during streaming integration


## [0.2.11] – 2025-06-05

### Added

* `runtime/llm` package with DSN-based providers (OpenAI, Claude, Cohere, Mistral, Gemini, Grok, Ollama, LlamaCPP, Chutes)
* Experimental `generate text` expression
* Optional Postgres logging via `tools/db`

### Changed

* MCP server logs full errors and disables color
* `mochi_eval` accepts a filename parameter
* Node installer works across platforms

### Fixed

* Various logging issues in the MCP server

## [0.2.10] – 2025-06-04

### Added

* Python compiler via `mochi build --target python` (or `py`) or `.py` output
* Build command auto-detects target language from extension
* Benchmarks auto-install Mochi, Deno and Python

### Changed

* Benchmarks measure durations in microseconds
* Clarified benchmark labels and merged test code

### Fixed

* Go compiler handles map literals with int values

## [0.2.9] – 2025-06-04

### Added

* TypeScript compiler for generating `.ts` output via `mochi build --ts`
* Integrated benchmarks that compile to Go and TypeScript with checked-in results

### Changed

* `mochi build` emits runtime helpers only when necessary
* Benchmarks use `performance.now` for accurate Deno timings
* Documentation covers new CLI options and Makefile tasks

### Fixed

* Minor Makefile issue affecting builds


All notable changes to the Mochi programming language are documented in this file.

## [0.2.8] – 2025-06-04

### Added

* `mochi build` command to compile Mochi source files into standalone binaries
* `mochi build --ts` flag to generate TypeScript instead of a binary
* Distribution via `npx` (`mochilang/mochi` or `@mochilang/mochi`) without local installation
* `mochi_eval` and `mochi_cheatsheet` tools exposed through the MCP server

### Changed

* Replaced the external `mcp-go` dependency with a minimal built‑in server
* Introduced a tagged `Value` type for better interpreter performance
* Benchmarks updated with a target that ensures the benchmark binary is built
* Slimmer npm package and new `make publish-npm` target
* Expanded parser, type checker and interpreter tests

### Fixed

* Clearer runtime errors for invalid slice and map usage
* Addressed type unification and parameter checking issues

## [0.2.7] – 2025-06-04

### Added

* Mutable variable bindings via the `var` keyword

### Changed

* `let` variables are now immutable and assignments to them will fail at compile time or runtime


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


## [0.2.2] - 2025-05-21

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



