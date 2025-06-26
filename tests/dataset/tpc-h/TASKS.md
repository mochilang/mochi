# TPCH Example Issues

Running `go test -tags slow ./tests/vm -run TestVM_TPCH` shows that several
queries still fail under the runtime VM.  The items below capture the issues
that need to be addressed in the VM or query logic so that all TPCH programs
can execute successfully.

## q8.mochi
- Execution fails with `Expect condition failed` after computing the result.
- Investigate floating point rounding or query logic causing the test to fail.
- Update the test expectations or query so `print result` matches the expected value.

## q13.mochi
- Query now compiles but the runtime VM fails the final expectation.
- Investigate the grouping logic to ensure counts match the expected output.

## q21.mochi
- Running the program prints the result but the final expectation fails.
- Verify the dataset and the `exists` subquery logic to ensure `numwait` is
  computed as expected.

## q22.mochi
- Parsing still fails around the aggregate and `exists` expression.
- Review the syntax for aggregate queries and nested `exists` to resolve the error.

Addressing these issues will allow all TPCH examples to run without errors and
regenerate stable `.out` and `.ir.out` files.
