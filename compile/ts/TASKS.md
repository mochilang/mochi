# TypeScript Backend Job Queries

The TypeScript backend now compiles the JOB dataset queries `q1` through `q10`. Compiled code and runtime output are checked in under `tests/dataset/job/compiler/ts`.

All JOB queries from `q1` through `q10` now compile and execute correctly. The
`in` operator was extended to handle unknown operand types via a `_contains`
helper, fixing `q5` which previously produced `0` instead of "A Film".

The TPCH dataset has golden tests for `q1` and `q2` stored under
`tests/dataset/tpc-h/compiler/ts`. Both queries compile and run successfully with Deno.

