## Tests

Use `test` blocks with `expect` to verify behavior. Each `expect` expression should evaluate to `true`. Tests can be run with `mochi test` or `make test`.

```mochi
test "math" {
  expect 2 + 2 == 4
  expect 5 > 3
}
```

You can group related tests into separate files. Failing expectations report the line number and expression that failed.

```mochi
test "reverse" {
  fun rev(s: string): string {
    return join(reverse(chars(s)))
  }
  expect rev("abc") == "cba"
}
```

To update golden files for snapshot tests, run `make update-golden`. This will regenerate any stored outputs used by the test suite.
