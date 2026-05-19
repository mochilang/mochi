## BG nsieve, vm3 port (interp only), 2026-05-19

Apple M4, darwin/arm64. `go test ./compiler3/corpus -bench='BenchmarkMathKernels/nsieve|BenchmarkGoKernels/nsieve' -benchtime=2s -count=5 -cpu=1`. Single-function while-loop encoding (`compiler3/corpus/nsieve.go`), interpreter only. No JIT (OpListSetI64 not yet admitted by `checkCellBankAdmissible`, so the function falls back to the interp).

| N | vm3 ns/op (median) | Go ns/op (median) | vm3 / Go | vm2 / Go (baseline) | improvement vs vm2 |
|---:|---:|---:|---:|---:|---:|
| 1000 | 200684 | 2661 | 75.4x | 126.05x | -40.2% |
| 10000 | 1794847 | 30738 | 58.4x | 134.10x | -56.4% |

Result is bit-identical to `c2corpus.ExpectNsieve` for `N` in `{0, 1, 2, 10, 50, 100, 1000}` (`TestMathKernelsMatchVm2/nsieve`).

The vm3 port collapses the 4-function tail-recursive vm2 shape (`main`, `fill`, `mark`, `outer`) into one function with three nested `while` loops. This alone halves the per-iteration call overhead (no `OpCallMixed` BLR site, no parameter shuffling per iter, no per-call frame snapshot/restore). The remaining ~58-75x gap to Go is dominated by:

1. **List storage density.** vm3 stores marks as 8-byte `Cell` (NaN-boxed i64); Go uses `[]bool` at 1 byte. The inner mark loop is bandwidth-bound: 8x more cache footprint per slot.
2. **Interpreter dispatch.** Every `OpListSetI64` is ~5-10 host instructions; Go compiles it to a single store. The inner mark loop touches `xs[j]` once and increments `j` by `i`, so 2 list ops + 2 arith ops per iter, vs ~3 instructions in Go.
3. **No JIT yet.** Nsieve's inner loop uses `OpListSetI64` which is not on the Cell-bank admission whitelist (`runtime/jit/vm3jit/compile.go:217-256`). Adding `OpListSetI64` lowering + admission is the next concrete lever; rough est: 5-10x speedup once it lowers, putting nsieve in the 6-15x of Go range.

Closing the residual 6-15x to under 2x then requires either:
- A typed-bytes list bank (i8 / bytes, eliminates the 8x density tax), or
- Loop hoisting of the slab base + bounds metadata (already done for `OpListGetI64`/`OpListPushI64` in the `lists_fill_sum` warm cache; same hoist applies here once `OpListSetI64` is admitted).

Raw output:
```
goos: darwin
goarch: arm64
pkg: mochi/compiler3/corpus
cpu: Apple M4
BenchmarkMathKernels/nsieve_n1000         	   10000	    220999 ns/op
BenchmarkMathKernels/nsieve_n1000         	   12141	    207521 ns/op
BenchmarkMathKernels/nsieve_n1000         	   12954	    178709 ns/op
BenchmarkMathKernels/nsieve_n1000         	   10000	    200684 ns/op
BenchmarkMathKernels/nsieve_n1000         	   13130	    174533 ns/op
BenchmarkMathKernels/nsieve_n10000        	    1275	   1934274 ns/op
BenchmarkMathKernels/nsieve_n10000        	    1267	   1824380 ns/op
BenchmarkMathKernels/nsieve_n10000        	    1422	   1755562 ns/op
BenchmarkMathKernels/nsieve_n10000        	    1458	   1782285 ns/op
BenchmarkMathKernels/nsieve_n10000        	    1431	   1794847 ns/op
BenchmarkGoKernels/nsieve_n1000           	  923997	      3524 ns/op
BenchmarkGoKernels/nsieve_n1000           	  970858	      2661 ns/op
BenchmarkGoKernels/nsieve_n1000           	  854893	      2771 ns/op
BenchmarkGoKernels/nsieve_n1000           	  780314	      2566 ns/op
BenchmarkGoKernels/nsieve_n1000           	  950257	      2331 ns/op
BenchmarkGoKernels/nsieve_n10000          	   84183	     30738 ns/op
BenchmarkGoKernels/nsieve_n10000          	   85179	     29088 ns/op
BenchmarkGoKernels/nsieve_n10000          	   80517	     33538 ns/op
BenchmarkGoKernels/nsieve_n10000          	   86713	     30128 ns/op
BenchmarkGoKernels/nsieve_n10000          	   80674	     30843 ns/op
```
