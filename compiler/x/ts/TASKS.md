# TypeScript Compiler Tasks

## Recent Enhancements
### 2025-07-15 03:39 UTC
- Generated missing TPC-DS outputs (q30-q39, q71-q78) with new `compile_tpcds_ts.go`.
### 2025-07-15 03:07 UTC
- Added JOB dataset queries q1-q33 to golden tests and regenerated outputs.
### 2025-07-15 03:28 UTC
- Improved equality generation to avoid `_equal` for primitive comparisons.
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
### 2025-07-15 03:39 UTC
- Regenerated JOB q1-q33 TypeScript outputs via compile_job_ts.go
### 2025-07-15 04:43 UTC
- Attempted to regenerate all TPC-DS TypeScript outputs via compile_tpcds_ts.go
- Several queries still fail at runtime under Deno
### 2025-07-15 04:53 UTC
- Regenerated TPC-DS TS outputs with error logging
- Failing queries now produce .error files (e.g., q1, q3)

### 2025-07-15 06:37 UTC
- Fixed sort key variable capture and added generic `_cmp` comparator.
- Regenerated TPC-DS TypeScript outputs via compile_tpcds_ts.go.
- Only q40 and q76 still fail at runtime.

### 2025-07-15 08:15 UTC
- Added `tpch_golden_test.go` to compile and verify TPC-H queries with Deno.
- Regenerated all TPC-H TypeScript outputs via `compile_tpch_ts.go`.
- All queries run successfully without errors.
### 2025-07-15 13:01 UTC
- Added `tpcds_golden_test.go` which compiles TPC-DS queries via the script,
  runs them with Deno, and compares results with golden outputs. `q1` now
  succeeds while `q40` and `q76` still produce `.error` files.
