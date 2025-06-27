# Go Compiler JOB Queries

Some JOB dataset programs do not compile or run correctly.
The generator script attempted to compile `q1` to `q10` but the
following queries failed:

- `q3` and `q9` produce Go code with global statements and fail to build.
- `q5` fails to run due to invalid generated Go syntax.
- `q7` fails during compilation: boolean `&&` on `bool` and `any` unsupported.

Remaining queries (`q2`, `q4`, `q6`, `q8`, `q10`) compiled and ran successfully.
Further work is needed to fix code generation for the failing queries.
