# Elixir Backend

The Elixir backend compiles Mochi programs to Elixir source code. It is used when targeting the BEAM runtime or for quick experimentation with Elixir tooling.

## Files

- `compiler.go` – walks the AST and emits Elixir source
- `expr.go` – expression helpers
- `runtime.go` – helper functions injected into generated code
- `tools.go` – ensures the `elixir` binary is available during tests
- `compiler_test.go` and `tpch_golden_test.go` – golden tests verifying generated programs run correctly

## TPC-H Progress

The TPC-H suite is exercised via golden tests. Most queries compile and run
successfully, generating Elixir code under
`tests/dataset/tpc-h/compiler/ex`.

**Passed:** `q1` `q2` `q3` `q4` `q5` `q6` `q7` `q8` `q9` `q10` `q11`
`q12` `q15` `q16` `q17` `q18` `q19` `q20` `q21` `q22`

**Failed:** `q13` `q14`

You can re-generate the golden files and verify execution with:

```bash
go test ./compiler/x/ex -run TestExCompiler_TPCHQueries -tags=slow -v
```

## Rosetta Progress

The Elixir backend is regularly tested against the Mochi Rosetta suite located
under `tests/rosetta/x/Mochi`. Most programs now compile and run correctly.
Recent improvements to map indexing reduced several `.error` files. Identifier
sanitization now converts CamelCase names to snake_case so calls such as
`net.lookup_host` compile correctly.

### Rosetta status (first 20)

| # | Program | Result |
|---|---------|--------|
| 1 | 100-doors-2 | ✅ |
| 2 | 100-doors-3 | ✅ |
| 3 | 100-doors | ✅ |
| 4 | 100-prisoners | ✅ |
| 5 | 15-puzzle-game | ❌ |
| 6 | 15-puzzle-solver | ❌ |
| 7 | 2048 | ✅ |
| 8 | 21-game | ❌ |
| 9 | 24-game-solve | ✅ |
|10 | 24-game | ✅ |
|11 | 4-rings-or-4-squares-puzzle | ✅ |
|12 | 9-billion-names-of-god-the-integer | ❌ |
|13 | 99-bottles-of-beer-2 | ❌ |
|14 | 99-bottles-of-beer | ✅ |
|15 | DNS-query | ❌ |
|16 | a+b | ✅ |
|17 | abbreviations-automatic | ✅ |
|18 | abbreviations-easy | ❌ |
|19 | abbreviations-simple | ❌ |
|20 | abc-problem | ❌ |
