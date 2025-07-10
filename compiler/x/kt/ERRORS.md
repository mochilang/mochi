# Kotlin roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: listOf
  --> :2:11

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/avg_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
```

