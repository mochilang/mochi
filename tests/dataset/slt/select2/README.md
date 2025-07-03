# Supported select2.test cases

All cases from `select2.test` numbered 1 through 99 were processed using
`mochi-slt gen`. Some cases correspond to table setup statements and do
not produce test programs. For the remaining cases a Mochi program was
generated in `tests/dataset/slt/out/select2`. When execution fails an
`.error` file is stored alongside the `.mochi` and `.out` files.

The following cases ran without errors:

```
26,42,77,92
```

All other generated cases produced an `.error` file.
