# TypeScript Backend Job Queries

The TypeScript backend now compiles the JOB dataset queries `q1` through `q10`. Compiled code and runtime output are checked in under `tests/dataset/job/compiler/ts`.

Query `q5` fails its embedded test when run via Deno. The result `[{"typical_european_movie":0}]` differs from the expected `[{"typical_european_movie":"A Film"}]`. Investigation is required to fix `_min` handling of strings or query evaluation.

