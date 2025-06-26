# TPCH Example Issues

The TPCH example programs were re-executed and several files failed to run correctly.
The following tasks outline the work required to restore passing results.

## q8.mochi
- Execution fails with `Expect condition failed` after computing the result.
- Investigate floating point rounding or query logic causing the test to fail.
- Update the test expectations or query so `print result` matches the expected value.

## q13.mochi
- Type checker reports `undefined variable: from` around the `count` query.
- The query inside `count` is split across lines. Wrap the subquery in parentheses
  (`count(from ... select ...)`) or update the parser to handle the newline.

## q21.mochi
- Running the program prints the result but the final expectation fails.
- Verify the dataset and the `exists` subquery logic to ensure `numwait` is
  computed as expected.

## q22.mochi
- Parsing fails near the `avg` expression in the `avg_balance` definition.
- Ensure the query is written as `avg(from ... select ...)` or modify the parser
  to allow a newline after the opening parenthesis.

Addressing these issues will allow all TPCH examples to run without errors and
regenerate stable `.out` and `.ir.out` files.
