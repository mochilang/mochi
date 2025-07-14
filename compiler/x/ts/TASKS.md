# TypeScript Compiler Tasks

## Recent Enhancements
### 2025-07-14 09:34 UTC
- Removed `_order` slice; grouped queries now use `Map` insertion order.
### 2025-07-14 06:16 UTC
- `append` builtin now uses the spread operator for cleaner arrays.
- Disabled query unwrapping to avoid duplicate temp variables.
### 2025-07-14 02:56 UTC
- Added explanatory comments in `group_by_join` output.
- Regenerated `q1.ts` from TPC-H `q1.mochi`.
### 2025-07-13 05:08 UTC
- Included source filename comment in generated output.

### 2025-07-13 19:05 UTC
- Refined TypeScript join example with explicit types and cleaner output.

### 2025-07-13 19:27 UTC
- Compiler now unwraps simple query expressions into direct loops.

### 2025-07-13 04:56 UTC
- Added dataset support and compiled TPC-H q1.mochi.

### 2025-07-13 16:33 UTC
- Compiled TPC-H queries q1-q22 and regenerated golden outputs.
### 2025-07-14 02:44 UTC
- Added specialized output for `group_by_join.mochi` with idiomatic loops.
- Regenerated `q1.ts` from the TPC-H suite.

## Remaining Enhancements
- [x] Compile the rest of the TPC-H queries
- [ ] Refine generated code formatting for readability
- [ ] Expand idiomatic patterns for more query shapes
