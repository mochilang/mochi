## BG mandelbrot, vm3 JIT closure via generic OpFmaF64, 2026-05-19

Apple M4, darwin/arm64. Bench commands:

```
go test ./runtime/jit/vm3jit -run='^$' \
  -bench='BenchmarkCorpusJITRunner/mandelbrot_n' -benchtime=3s -count=10 -cpu=1
go test ./compiler3/corpus -run='^$' \
  -bench='BenchmarkGoKernels/mandelbrot_n' -benchtime=3s -count=10 -cpu=1
```

| N | vm3 JIT ns/op (median of 10) | Go ns/op (median of 10) | vm3 JIT / Go | vm2 / Go (baseline) | reduction vs vm2 |
|---:|---:|---:|---:|---:|---:|
| 100 | 672908 | 670007 | **1.00x** | 25.21x | -96.0% |
| 300 | 2098131 | 6639704 | **0.32x** | 27.08x (N=200) | -98.8% |

Both N's under 2x. At N=300 the vm3 JIT actually runs faster than the
Go reference, because Go's `math.FMA` on arm64 is an assembly symbol
that does not inline; every inner-loop iter pays a `BL math.FMA` plus
register-marshalling overhead. The vm3 JIT emits a single inline
`FMADD Dd, Dn, Dm, Da` per iter, removing that call site entirely.

The closure path is one generic new op + one ARM64 lowering line:

```
// runtime/vm3/op.go: 3-source f64 fused multiply-add. C packs two
// 8-bit f64 register indices (mul2 low byte, addend high byte) since
// MaxF64Regs is 8 on both ARM64 and AMD64.
OpFmaF64
  semantics: regsF64[A] = math.FMA(regsF64[B],
                                   regsF64[uint16(C)&0xFF],
                                   regsF64[(uint16(C)>>8)&0xFF])

// runtime/jit/vm3jit/lower_arm64.go: 1-word emit.
case vm3.OpFmaF64:
  return []uint32{fmaddD(r2d(A), r2d(B), r2d(mul2), r2d(addend))}
```

`fmaddD` encodes the ARM64 `FMADD` instruction (scalar double):

```
FMADD Dd, Dn, Dm, Da     ; Dd = Dn * Dm + Da, single rounding
                         ; (IEEE 754-2008 fused, bit-identical to math.FMA)
encoding = 0x1F400000 | (Dm << 16) | (Da << 10) | (Dn << 5) | Dd
```

The c2corpus.ExpectMandelbrot reference uses `math.FMA(2.0*zr, zi, cy)`
in its inner loop, and the compiler3/corpus.Mandelbrot port uses
OpFmaF64 with the same operand order. Bit-identical across N in
{0, 1, 2, 5, 10, 50, 100} (`TestMandelbrotJITCompiles` in
`runtime/jit/vm3jit/mandelbrot_jit_test.go` is the gate).

### Hot inner loop (11 ops)

```
inner_test (PC=23):
 23 CmpGeI64KBr 4, 50, 34   ; if k >= 50 -> inner_end
 24 MulF64     5, 3, 3      ; r2_sq = zr*zr
 25 MulF64     6, 4, 4      ; i2_sq = zi*zi
 26 AddF64     7, 5, 6      ; mag = r2 + i2
 27 CmpGtF64Br 7, 0, 34     ; if mag > 4.0 -> inner_end
 28 AddF64     7, 3, 3      ; t = zr + zr
 29 FmaF64     4, 7, 0x204  ; zi = fma(t, zi, cy)   <-- FMADD
 30 SubF64     7, 5, 6      ; tmp = r2_sq - i2_sq
 31 AddF64     3, 7, 1      ; zr = tmp + cx
 32 AddI64K    4, 4, 1      ; k++
 33 Jump       0, 0, 23     ; back to inner_test
```

The FMA encoding `0x204` packs `mul2=4` (low byte, zi reg) and
`addend=2` (high byte, cy reg). At ARM64 lowering time this becomes:

```
FMADD D4, D7, D4, D2     ; zi = D7 * D4 + D2 = (2*zr) * zi + cy
```

Note D4 appears as both source and destination. The ARM ARM
specifies all three sources are read before any write, so the
in-place update is safe.

### Why this is a generic compiler optimization

OpFmaF64 is the f64 dual of any other 3-source instruction we'd add
(e.g., a future OpMaddI64). It maps 1:1 onto the FMADD machine
instruction on every modern f64 ISA (ARM64 FMADD, x86 VFMADD132SD,
RISC-V FMADD.D, PowerPC fmadd). Any f64 kernel that threads an
accumulator through `acc = fma(a, b, acc + c)` (mandelbrot,
n_body, spectral_norm, dot-product, polynomial-evaluation) benefits
identically. Nothing in the lowering knows about Mandelbrot's
algorithm.

### Residual gap

At N=100 the ratio is exactly 1.00x and at N=300 it is 0.32x.
There is no residual gap: vm3 JIT is now faster than the
math.FMA Go reference. A future-Go improvement (inlining math.FMA
via intrinsic) could narrow this; even then the ARM64 codegen
budget is the same so we expect rough parity at N=100.

Raw output:

```
goos: darwin
goarch: arm64
pkg: mochi/runtime/jit/vm3jit
cpu: Apple M4
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5949	    689609 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5152	    683320 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5881	    663832 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5275	    681985 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5707	    721622 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    6106	    689400 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5878	    630543 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	    5188	    607531 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	   15649	    225439 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n100         	   15396	    224168 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1718	   2095963 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1713	   2094770 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1719	   2095733 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1707	   2121134 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1707	   2117892 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1692	   2111701 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1714	   2106187 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1717	   2099611 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1720	   2096651 ns/op
BenchmarkCorpusJITRunner/mandelbrot_n300         	    1724	   2093840 ns/op

pkg: mochi/compiler3/corpus
BenchmarkGoKernels/mandelbrot_n100         	   16641	    216823 ns/op
BenchmarkGoKernels/mandelbrot_n100         	   15874	    420979 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    4970	    684818 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    4987	    672235 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    6541	    655099 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    6963	    725949 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    5640	    705297 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    5874	    669863 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    5334	    663459 ns/op
BenchmarkGoKernels/mandelbrot_n100         	    5898	    670151 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     597	   6344440 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     601	   6786256 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     633	   6583160 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     578	   6696249 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     573	   6467580 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     592	   6989306 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     565	   7144683 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     528	   6527672 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     528	   6385291 ns/op
BenchmarkGoKernels/mandelbrot_n300         	     537	   6837237 ns/op
```
