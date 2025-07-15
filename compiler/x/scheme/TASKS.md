# Scheme Backend Tasks for TPCH Queries

The Scheme backend now targets chibi-scheme and can compile the `tpc-h/q1.mochi` and `tpc-h/q2.mochi` benchmarks. Initial JOB dataset support was explored for `job/q1.mochi`. Later queries compile but fail at runtime because sorting and date operations are incomplete.

### Recent Enhancements
- 2025-07-13 05:05 – Added machine generated output for `tpc-h/q1.mochi` and updated checklist.
- 2025-07-13 17:36 – Added golden test for `tpc-h/q2.mochi` which now compiles and runs successfully.
- 2025-07-13 17:53 – Implemented aggregate query handling and added golden files for `tpc-h/q6.mochi`.
- 2025-07-13 17:59 – Expanded TPCH golden test to cover `q1` through `q22`.
- 2025-07-15 06:36 – Added TPC-DS golden test and helper script to compile queries.
- 2025-07-15 07:28 – Generated Scheme code for TPC-DS queries `q40`, `q77`, `q78`, `q93` and `q97`.

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
