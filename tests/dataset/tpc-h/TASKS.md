# TPCH Runtime Issues

The TPCH example programs were re-executed using `TestVM_TPCH`. Most queries now
run and match the recorded `.out` and `.ir.out` files. The remaining runtime
issue is summarised below.

## q21.mochi
- Execution fails with `expect condition failed` while validating the final
  result.
- Investigate the `exists` subquery logic and dataset used to compute
  `numwait` so the output matches the expected value.
