# Racket compiler VM comparison failures

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- racket --
7
#t
#t
-- vm --
7
true
true
```

