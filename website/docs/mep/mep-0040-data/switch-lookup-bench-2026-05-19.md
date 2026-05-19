## Phase 6.4 switch-lookup-table bench (2026-05-19)

Apple M4, darwin/arm64. `go test ./compiler3/corpus -bench=BenchmarkSwitchLookup8 -benchtime=2s -count=5 -cpu=1`. Interpreter only (no JIT). LCG of period 32749 drives an unpredictable mod-8 key per iteration; both variants accumulate identical per-case values.

| Variant | N | runs | ns/op median | ns/op min |
|---|---:|---:|---:|---:|
| cmp_chain | 100 | 5 | 14055 | 12593 |
| cmp_chain | 10000 | 5 | 1465814 | 1308175 |
| table | 100 | 5 | 11017 | 10259 |
| table | 10000 | 5 | 974756 | 908774 |

Table-vs-chain reduction at median: 21.6% (N=100), 33.5% (N=10000). Speed-up ratios: 1.28x and 1.50x. The N=10000 number is the cleaner read since per-iteration cost dominates loop overhead.

Per-iteration op count theory (LCG 4 + bounds/cmp + lookup + jump + accumulate 2 + back-jump 1):
- cmp_chain expected case 4 of 8: 4 LCG + 4 CmpEq + 1 ConstK + 1 Jump + 2 acc + 1 back ≈ 13 ops
- table: 4 LCG + 1 CmpGeK + 1 Lookup + 1 Jump + 2 acc + 1 back = 10 ops

13 / 10 = 1.30x predicted, measured 1.50x. The gap above prediction comes from cmp-chain branch mispredicts feeding into the interpreter's `for { switch op.Code ... }` dispatch, which serialises on the mispredicted next-op fetch as well as on the mispredicted dispatch target.

Go CL 756340 measures -62.65% on the equivalent native bench (SwitchLookup8Unpredictable). The vm3 interpreter delta is smaller because per-op fixed dispatch overhead caps the relative win; JIT lowering of OpLookupI64KW (Phase 6.4 follow-up) should close most of the remaining gap.

Raw output:
```
goos: darwin
goarch: arm64
pkg: mochi/compiler3/corpus
cpu: Apple M4
BenchmarkSwitchLookup8/cmp_chain_n100         	  223622	     17712 ns/op
BenchmarkSwitchLookup8/cmp_chain_n100         	  235826	     12593 ns/op
BenchmarkSwitchLookup8/cmp_chain_n100         	  153292	     14055 ns/op
BenchmarkSwitchLookup8/cmp_chain_n100         	  228862	     14652 ns/op
BenchmarkSwitchLookup8/cmp_chain_n100         	  166401	     13022 ns/op
BenchmarkSwitchLookup8/cmp_chain_n10000       	    1780	   1379526 ns/op
BenchmarkSwitchLookup8/cmp_chain_n10000       	    1926	   1465814 ns/op
BenchmarkSwitchLookup8/cmp_chain_n10000       	    1971	   1308175 ns/op
BenchmarkSwitchLookup8/cmp_chain_n10000       	    1988	   1593004 ns/op
BenchmarkSwitchLookup8/cmp_chain_n10000       	    1849	   1569013 ns/op
BenchmarkSwitchLookup8/table_n100             	  240259	     11367 ns/op
BenchmarkSwitchLookup8/table_n100             	  248266	     11374 ns/op
BenchmarkSwitchLookup8/table_n100             	  275534	     10443 ns/op
BenchmarkSwitchLookup8/table_n100             	  230716	     10259 ns/op
BenchmarkSwitchLookup8/table_n100             	  192682	     11017 ns/op
BenchmarkSwitchLookup8/table_n10000           	    2652	   1026419 ns/op
BenchmarkSwitchLookup8/table_n10000           	    2616	   1095832 ns/op
BenchmarkSwitchLookup8/table_n10000           	    2792	    908774 ns/op
BenchmarkSwitchLookup8/table_n10000           	    2305	    974756 ns/op
BenchmarkSwitchLookup8/table_n10000           	    2526	    911080 ns/op
```
