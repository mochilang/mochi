## BG nsieve, vm3 JIT closure via OpListSetI64 admission, 2026-05-19

Apple M4, darwin/arm64. Bench commands:

```
go test ./runtime/jit/vm3jit -run='^$' \
  -bench='BenchmarkCorpusJITRunner/nsieve_n' -benchtime=3s -count=10 -cpu=1
go test ./compiler3/corpus -run='^$' \
  -bench='BenchmarkGoKernels/nsieve_n' -benchtime=3s -count=10 -cpu=1
```

| N | vm3 JIT ns/op (median of 10) | Go ns/op (median of 10) | vm3 JIT / Go | vm3 interp / Go | vm2 / Go (baseline) | reduction vs vm2 |
|---:|---:|---:|---:|---:|---:|---:|
| 1000  | 5064  | 3499  | **1.45x** | 75.4x | 126.05x | -98.8% |
| 10000 | 74769 | 40530 | **1.85x** | 58.4x | 134.10x | -98.6% |

Under 2x at both sizes. The closure path is purely the JIT admission of `OpListSetI64`: one whitelist entry plus a 14-line ARM64 lowering that mirrors `OpListGetI64` (no cap check, no `len++`, just a packed Int48 store at `cells.ptr[idx]`).

```
checkCellBankAdmissible whitelist (compile.go:230):
  case vm3.OpListGetI64, vm3.OpListPushI64, vm3.OpListSetI64:
    continue
```

Hot inner-mark loop encoding (3-word hottest form, when `cells.ptr` is pinned in x22):

```
  MOVZ x16, #0xFFFA, LSL #48     ; tagInt48
  BFI  x16, xVal, #0, #48        ; pack 48-bit payload
  STR  x16, [x22, xIdx, LSL #3]  ; cells[idx] = packed
```

The inner mark loop at PC 14..18 of `nsieve.go` thus lowers to:

```
14  CMP  xJ, xN                  ; if j > n -> skip
    B.GT skip
15  MOV  x4, #1                  ; val = 1
16  MOVZ x16, #0xFFFA, LSL #48   ; OpListSetI64 (3 words)
    BFI  x16, x4, #0, #48
    STR  x16, [x22, xJ, LSL #3]
17  ADD  xJ, xJ, xI              ; j += i
18  B    pc14                    ; back to inner_test
```

8 ARM64 instructions per mark iteration. Bit-identical to `c2corpus.ExpectNsieve` across N in {0, 1, 2, 10, 50, 100, 1000} (`TestNsieveJITCompiles` in `runtime/jit/vm3jit/nsieve_jit_test.go` is the gate; `TestMathKernelsMatchVm2/nsieve` in `compiler3/corpus` covers the interp path).

Why this is a generic compiler optimization (no super-op): `OpListSetI64` is the dual of `OpListGetI64`, already admitted. The lowering uses the same `tagInt48` mask + `BFI` packing that lists_fill_sum's push path uses, the same hoisted-`cells.ptr` register pinning that lists_fill_sum's get path uses. Nothing in the lowering knows about nsieve's algorithm; any cell-bank function with a single list and an `xs[i] = v` op in its hot loop benefits identically.

Residual gap to Go:
1. **Storage density**: vm3 stores marks as 8-byte `Cell` (NaN-boxed i64). Go uses `[]bool` at 1 byte. 8x cache footprint on the inner mark loop. Closing this fully requires the Phase 3.6 bytes bank (`regs<U8> / OpBytesSetU8`).
2. **Fill loop overhead**: nsieve pushes n+1 zeros via `OpListPushI64` in a loop. Go uses `make([]bool, n+1)` (single bulk allocation). Closing this needs a generic "push-N-zeros" detection in the JIT, or a new `OpListResize` op.

Both are post-2x-gate improvements; the current closure is via JIT admission alone, with no algorithmic divergence from the vm3 interp.

Raw output:

```
goos: darwin
goarch: arm64
pkg: mochi/runtime/jit/vm3jit
cpu: Apple M4
BenchmarkCorpusJITRunner/nsieve_n1000         	  814204	      5049 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  609294	      5412 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  787413	      5079 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  792477	      4940 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  802581	      5108 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  785716	      5734 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  709171	      5560 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  798172	      4949 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  725142	      4583 ns/op
BenchmarkCorpusJITRunner/nsieve_n1000         	  809112	      4742 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   49568	     66037 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   48631	     75036 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   53604	     66918 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   67396	     71540 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   36874	    130344 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   43196	     76741 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   46508	     65960 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   49557	    107668 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   33358	    106754 ns/op
BenchmarkCorpusJITRunner/nsieve_n10000        	   49192	     74501 ns/op

pkg: mochi/compiler3/corpus
BenchmarkGoKernels/nsieve_n1000         	 1598046	      2402 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1578606	      2242 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1670983	      2568 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      3213 ns/op
BenchmarkGoKernels/nsieve_n1000         	  965792	      5187 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      5048 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      3541 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      5518 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      3596 ns/op
BenchmarkGoKernels/nsieve_n1000         	 1000000	      3456 ns/op
BenchmarkGoKernels/nsieve_n10000        	  116004	     42641 ns/op
BenchmarkGoKernels/nsieve_n10000        	  130692	     36838 ns/op
BenchmarkGoKernels/nsieve_n10000        	  124808	     59485 ns/op
BenchmarkGoKernels/nsieve_n10000        	  111558	     40727 ns/op
BenchmarkGoKernels/nsieve_n10000        	   71994	     54628 ns/op
BenchmarkGoKernels/nsieve_n10000        	   85353	     37588 ns/op
BenchmarkGoKernels/nsieve_n10000        	   90094	     37917 ns/op
BenchmarkGoKernels/nsieve_n10000        	   90289	     39533 ns/op
BenchmarkGoKernels/nsieve_n10000        	   81726	     44736 ns/op
BenchmarkGoKernels/nsieve_n10000        	   99751	     40333 ns/op
```
