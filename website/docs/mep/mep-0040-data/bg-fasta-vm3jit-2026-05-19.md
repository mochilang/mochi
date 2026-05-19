## BG fasta, vm3 JIT closure, 2026-05-19

Apple M4, darwin/arm64. Bench commands:

```
go test ./runtime/jit/vm3jit -run='^$' \
  -bench='BenchmarkCorpusJITRunner/fasta_n' -benchtime=2s -count=5 -cpu=1
go test ./compiler3/corpus -run='^$' \
  -bench='BenchmarkGoKernels/fasta_n' -benchtime=2s -count=5 -cpu=1
```

| N | vm3 JIT ns/op (median) | Go ns/op (median) | vm3 JIT / Go | vm3 interp / Go | vm2 / Go (baseline) | improvement vs vm2 |
|---:|---:|---:|---:|---:|---:|---:|
| 10000  | 136594  | 129419  | 1.06x | 8.79x  | 3.81x | -72.2% |
| 100000 | 1932635 | 2533190 | 0.76x | 3.98x  | 4.00x | -81.0% |

Under 2x at both sizes. N=100000 is faster than the native-Go reference because the inner-loop hash uses a Mersenne mod (`hash %= 2147483647`) and the vm3 JIT lowers `OpModI64` to ARM64 `UDIV; MSUB`, which compiles tighter than the bounds-checked path Go emits for the same expression.

The vm3 corpus port is a single-function 29-op program (`compiler3/corpus/fasta.go`) with `NumRegsI64=10` (under the ARM64 cap of 17) and a 5-entry `Consts` pool for wide constants (139968, 2^31-1, three integer cascade thresholds). vm2's fasta was 5 functions (main + 4 tail-recursive byte arms); collapsing to one function with a 3-way `OpCmpLtI64Br` cascade plus per-byte K-load + `OpJump` join eliminates the per-iter `OpTailCallSelfA4` BLR site that drove vm2's residual.

Every opcode in the function admits to the ARM64 lowerer:

| opcode | lower_arm64.go site |
|---|---|
| OpConstI64K     | :854 / :1040 |
| OpConstI64KW    | :859 / :1043 |
| OpCmpGeI64Br    | :889 / :1101 (group) |
| OpCmpLtI64Br    | :889 / :1101 (group) |
| OpMulI64K       | :877 / :1088 |
| OpAddI64K       | :877 / :1082 |
| OpModI64        | :872 / :1067 |
| OpAddI64        | :867 / :1050 |
| OpJump          | :899 / :1129 |
| OpReturnI64     | :901 / :1138 |

Result is bit-identical to `c2corpus.ExpectFasta` for `N` in `{0, 1, 2, 10, 100, 1000, 10000}` (`TestMathKernelsMatchVm2/fasta`).

Raw output:

```
goos: darwin
goarch: arm64
pkg: mochi/runtime/jit/vm3jit
cpu: Apple M4
BenchmarkCorpusJITRunner/fasta_n10000         	   19005	    136594 ns/op
BenchmarkCorpusJITRunner/fasta_n10000         	   19236	    122229 ns/op
BenchmarkCorpusJITRunner/fasta_n10000         	   19743	    139176 ns/op
BenchmarkCorpusJITRunner/fasta_n10000         	   19782	    133382 ns/op
BenchmarkCorpusJITRunner/fasta_n10000         	   19353	    143376 ns/op
BenchmarkCorpusJITRunner/fasta_n100000        	    1202	   1883072 ns/op
BenchmarkCorpusJITRunner/fasta_n100000        	    1448	   2004603 ns/op
BenchmarkCorpusJITRunner/fasta_n100000        	    1354	   1932635 ns/op
BenchmarkCorpusJITRunner/fasta_n100000        	    1364	   2125722 ns/op
BenchmarkCorpusJITRunner/fasta_n100000        	    1176	   1822511 ns/op

goos: darwin
goarch: arm64
pkg: mochi/compiler3/corpus
cpu: Apple M4
BenchmarkGoKernels/fasta_n10000         	   21028	    129419 ns/op
BenchmarkGoKernels/fasta_n10000         	   19063	    136343 ns/op
BenchmarkGoKernels/fasta_n10000         	   18007	    123125 ns/op
BenchmarkGoKernels/fasta_n10000         	   20438	    132700 ns/op
BenchmarkGoKernels/fasta_n10000         	   20700	    116699 ns/op
BenchmarkGoKernels/fasta_n100000        	     824	   2533190 ns/op
BenchmarkGoKernels/fasta_n100000        	    1018	   2247382 ns/op
BenchmarkGoKernels/fasta_n100000        	     878	   2598532 ns/op
BenchmarkGoKernels/fasta_n100000        	     883	   2348646 ns/op
BenchmarkGoKernels/fasta_n100000        	     950	   2602843 ns/op
```
