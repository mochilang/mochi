# Scheme Backend Tasks for TPCH Queries

The Scheme backend now targets chibi-scheme and can compile the `tpc-h/q1.mochi` and `tpc-h/q2.mochi` benchmarks. Initial JOB dataset support was explored for `job/q1.mochi`. Later queries compile but fail at runtime because sorting and date operations are incomplete.

### Recent Enhancements
- 2025-07-13 05:05 – Added machine generated output for `tpc-h/q1.mochi` and updated checklist.
- 2025-07-13 17:36 – Added golden test for `tpc-h/q2.mochi` which now compiles and runs successfully.
- 2025-07-13 17:53 – Implemented aggregate query handling and added golden files for `tpc-h/q6.mochi`.
- 2025-07-13 17:59 – Expanded TPCH golden test to cover `q1` through `q22`.
- 2025-07-15 06:36 – Added TPC-DS golden test and helper script to compile queries.
- 2025-07-15 07:28 – Generated Scheme code for TPC-DS queries `q40`, `q77`, `q78`, `q93` and `q97`.
- 2025-07-16 11:34 – Added Rosetta golden test and compile script; improved
  variable handling to avoid runtime errors.
- 2025-07-16 12:20 – Implemented `now()` builtin using `(chibi time)` to reduce
  runtime failures when compiling Rosetta examples.
- 2025-07-16 12:45 – Integer division now uses `quotient` when operands are
  integers, reducing runtime errors for several Rosetta tasks.
- 2025-07-16 13:10 – Added golden tests for `tests/vm/valid` programs and
  enabled Python `math` imports without the `auto` keyword.
- 2025-07-17 01:30 – Improved `_date_number` to handle timestamps and `/`-based
  separators when comparing dates.
- 2025-07-17 02:00 – `_date_number` now supports 8-digit `YYYYMMDD` strings for
  JOB benchmarks.
- 2025-07-17 03:00 – Comparator generation now uses type inference to avoid
  emitting dataset helper functions when operands are numeric or strings.
- 2025-07-17 09:12 – Aggregate builtins now use Scheme primitives when operating
  on numeric lists, removing unnecessary helper functions.
- 2025-07-17 14:05 – Built-in sorting uses SRFI-95 when keys are numeric or
  strings and YAML loader converts keys to symbols to avoid runtime type errors.
- 2025-07-17 18:00 – `count` builtin uses `string-length` or `length` when
  possible, eliminating unnecessary dataset helpers.

### Remaining Work
- [ ] Better handling of date comparisons and sorting when running JOB benchmarks
- [ ] More efficient dataset grouping and aggregation
- [ ] Support for concurrent agents and streaming primitives
- [ ] Improve macro support for generated Scheme code
- [ ] Add pattern matching for union types
- [ ] Implement async/await semantics
- [ ] Optimize tail-call recursion
- [ ] Enhance foreign function interface bindings
- [ ] Provide REPL mode for compiled programs
- [ ] Add generic type parameter compilation
- [ ] Extend dataset query language with window functions
- [ ] Improve error messages for invalid constructs
