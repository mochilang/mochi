# VM bench harness

The MEP-17 reference suite for the Mochi VM. Ten programs, one
`testing.B` driver per program, expected outputs that double as
conformance fixtures.

## Run

The harness is behind the `slow` build tag so it stays out of the
default CI sweep. To run:

```
go test -tags=slow -bench=. -benchmem -benchtime=1s ./runtime/vm/bench/...
```

To run only the conformance assertions (every program executed once,
stdout compared against `expected/<name>.out`):

```
go test -tags=slow -run TestBenchPrograms ./runtime/vm/bench/...
```

## What each program stresses

| Program            | Stresses                                                |
|--------------------|---------------------------------------------------------|
| `fib`              | recursive call, integer arithmetic, return path         |
| `iter_sum`         | tight numeric loop, `for i in 1..N`, `OpAddInt`         |
| `string_cat`       | string concatenation, allocation pressure on `OpStr`    |
| `map_get`          | map lookup in a loop, optional unwrap via `??`          |
| `list_build`       | `append` in a loop, list growth                         |
| `struct_field`     | struct field read and rebuild in a loop                 |
| `hof_map`          | higher-order map, closure invocation through `OpCall`   |
| `query_select`     | `from ... where ... select ...` over a list array       |
| `closure_dispatch` | list of closures invoked in a loop (intent-table shape) |
| `json_emit`        | nested map + list traversal, JSON encoder hot path      |

`closure_dispatch` and `json_emit` stand in for the `agent_emit` and
`json_round` slots in the MEP-17 table. The agent runtime and a
`json.parse` builtin do not exist in the VM yet, so the suite
exercises the same hot paths (function-value dispatch and JSON encode
respectively) through features that work today. The MEP table notes
the substitution; the names will be reinstated when the underlying
features land.

## Reproducibility

The harness sets `MOCHI_NOW_SEED=1` and `MOCHI_BENCH=1` in its `init`
function so wall-clock-derived heuristics produce stable numbers across
runs on the same host. Cross-host comparison is meaningless: record the
CPU, OS, and Go version in any reported benchstat.

## Baseline history

`history/` holds one `benchstat`-formatted file per minor release. The
release script appends a new file; nothing in this directory should be
hand-edited.

To produce a release baseline:

```
go test -tags=slow -bench=. -benchmem -benchtime=3s -count=5 \
  ./runtime/vm/bench/... > history/v0.X.0.txt
```

Then run `benchstat history/v0.X-1.txt history/v0.X.0.txt` to see the
delta.

## Regression budget

Per MEP-17 §Regression budget:

- A PR that targets benchmark X must not regress X.
- A PR may regress unrelated benchmarks by up to 3% if the suite
  geomean does not regress.
- A PR may not regress the suite geomean at all.
- A 5% regression in `B/op` or `allocs/op` triggers the same review
  gate as a 3% time regression.

These tighten as the floor rises. See the MEP for the full rules.
