# Haskell roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
runhaskell error: exit status 1

tests/vm/valid/append_builtin.hs.out:183:10: error:
    Variable not in scope: append :: [a0] -> t0 -> a1
    Suggested fix:
      Perhaps use one of these:
        ‘BSL.append’ (imported from Data.ByteString.Lazy.Char8),
        ‘T.append’ (imported from Data.Text),
        ‘mappend’ (imported from Prelude)
    |
183 |   print (append a 3)
    |          ^^^^^^

```

## tests/vm/valid/avg_builtin.mochi

```
output mismatch
-- hs --
2.0
-- vm --
2
```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- hs --
7
True
True
-- vm --
7
true
true
```

