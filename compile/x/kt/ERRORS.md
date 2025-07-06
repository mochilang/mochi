# Kotlin roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: listOf
  --> :2:11

help:
  Ensure the function is defined before it's called.
```

