package corpus

import "mochi/runtime/vm3"

// Mandelbrot: BG `mandelbrot` kernel. Sums per-pixel escape counts over
// an NxN grid in [-2, 1] x [-1, 1] with maxIter = 50. Bit-identical to
// compiler2/corpus.ExpectMandelbrot, which threads each per-iter zi
// update through math.FMA (one rounding step). vm3 ports that as
// OpFmaF64, which lowers on ARM64 to a single FMADD instruction.
//
// Inner-loop shape, expressed in Mochi-ish pseudocode:
//
//	zr = 0; zi = 0; k = 0
//	while k < 50 {
//	  r2 = zr*zr; i2 = zi*zi
//	  if r2 + i2 > 4.0 break
//	  t = zr + zr               // 2*zr, exact in f64
//	  zi = fma(t, zi, cy)       // bit-identical to Go's math.FMA
//	  zr = (r2 - i2) + cx
//	  k++
//	}
//	sum += k
//
// Banks: i64 (n, sum, row, col, k) + f64. NumRegsI64 = 5,
// NumRegsF64 = 8 (= MaxF64Regs cap on ARM64/AMD64). Pinning:
//   - f0 = 4.0 (escape threshold, loaded once at program start)
//   - f1 = cx (per-pixel)
//   - f2 = cy (per-row, hoisted out of the column loop)
//   - f3 = zr
//   - f4 = zi
//   - f5..f7 = scratch (r2_sq, i2_sq, mag/2zr/r2-i2/n_f reload)
//
// We reload float64(n) inline for the cx/cy divides rather than pinning
// it in a register; this frees the slot that 4.0 takes and keeps the
// inner-loop body within the 8-reg ARM64 cap. The cx setup runs once
// per pixel and the cy setup runs once per row, so the 1-extra-cast
// per scope adds < 1% overhead vs. the inner loop body.
//
// f64 Consts pool:
//
//	[0] = 0.0   (zr/zi init)
//	[1] = 1.0   (cy subtraction)
//	[2] = 2.0   (cy multiplication, cx subtraction)
//	[3] = 3.0   (cx multiplication)
//	[4] = 4.0   (escape threshold)
//
// Bytecode (40 ops):
//
//	  0 ConstI64K  1, 0, 0    ; sum = 0
//	  1 ConstI64K  2, 0, 0    ; row = 0
//	  2 ConstF64K  0, 0, 4    ; f0 = 4.0 (pin)
//	    row_test (PC=3):
//	  3 CmpGeI64Br 2, 0, 39   ; if row >= n -> end
//	  4 I64ToF64   2, 2       ; f2 = f64(row)
//	  5 I64ToF64   5, 0       ; f5 = f64(n)
//	  6 DivF64     2, 2, 5    ; f2 /= n_f
//	  7 ConstF64K  6, 0, 2    ; f6 = 2.0
//	  8 MulF64     2, 2, 6    ; f2 *= 2
//	  9 ConstF64K  6, 0, 1    ; f6 = 1.0
//	 10 SubF64     2, 2, 6    ; cy = f2 - 1
//	 11 ConstI64K  3, 0, 0    ; col = 0
//	    col_test (PC=12):
//	 12 CmpGeI64Br 3, 0, 37   ; if col >= n -> row_inc
//	 13 I64ToF64   1, 3       ; f1 = f64(col)
//	 14 I64ToF64   5, 0       ; f5 = f64(n)
//	 15 DivF64     1, 1, 5    ; f1 /= n_f
//	 16 ConstF64K  6, 0, 3    ; f6 = 3.0
//	 17 MulF64     1, 1, 6    ; f1 *= 3
//	 18 ConstF64K  6, 0, 2    ; f6 = 2.0
//	 19 SubF64     1, 1, 6    ; cx = f1 - 2
//	 20 ConstF64K  3, 0, 0    ; zr = 0
//	 21 ConstF64K  4, 0, 0    ; zi = 0
//	 22 ConstI64K  4, 0, 0    ; k = 0
//	    inner_test (PC=23):
//	 23 CmpGeI64KBr 4, 50, 34 ; if k >= 50 -> inner_end
//	 24 MulF64     5, 3, 3    ; r2 = zr*zr
//	 25 MulF64     6, 4, 4    ; i2 = zi*zi
//	 26 AddF64     7, 5, 6    ; mag = r2 + i2
//	 27 CmpGtF64Br 7, 0, 34   ; if mag > 4 -> inner_end
//	 28 AddF64     7, 3, 3    ; t = zr + zr
//	 29 FmaF64     4, 7, pack(mul2=4, add=2)  ; zi = fma(t, zi, cy)
//	 30 SubF64     7, 5, 6    ; tmp = r2 - i2
//	 31 AddF64     3, 7, 1    ; zr = tmp + cx
//	 32 AddI64K    4, 4, 1    ; k++
//	 33 Jump       0, 0, 23   ; back to inner_test
//	    inner_end (PC=34):
//	 34 AddI64     1, 1, 4    ; sum += k
//	 35 AddI64K    3, 3, 1    ; col++
//	 36 Jump       0, 0, 12   ; back to col_test
//	    row_inc (PC=37):
//	 37 AddI64K    2, 2, 1    ; row++
//	 38 Jump       0, 0, 3    ; back to row_test
//	    end (PC=39):
//	 39 ReturnI64  1, 0, 0
//
// Cross-lang harness uses BG side = N and maxIter = 50; bench sizes
// match the BG cross-lang baseline (N=100 and N=300).
var Mandelbrot = &Program{
	Name: "mandelbrot",
	Build: func(_ int64) *vm3.Program {
		// FmaF64 C packs mul2 reg in low byte and addend reg in high byte.
		fmaC := int16(int(4) | (int(2) << 8))
		fn := &vm3.Function{
			Name:       "mandelbrot",
			NumRegsI64: 5,
			NumRegsF64: 8,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Consts: []vm3.Cell{
				vm3.CFloat(0.0),
				vm3.CFloat(1.0),
				vm3.CFloat(2.0),
				vm3.CFloat(3.0),
				vm3.CFloat(4.0),
			},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 4),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 39),
				vm3.MakeOp(vm3.OpI64ToF64, 2, 2, 0),
				vm3.MakeOp(vm3.OpI64ToF64, 5, 0, 0),
				vm3.MakeOp(vm3.OpDivF64, 2, 2, 5),
				vm3.MakeOp(vm3.OpConstF64K, 6, 0, 2),
				vm3.MakeOp(vm3.OpMulF64, 2, 2, 6),
				vm3.MakeOp(vm3.OpConstF64K, 6, 0, 1),
				vm3.MakeOp(vm3.OpSubF64, 2, 2, 6),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 3, 0, 37),
				vm3.MakeOp(vm3.OpI64ToF64, 1, 3, 0),
				vm3.MakeOp(vm3.OpI64ToF64, 5, 0, 0),
				vm3.MakeOp(vm3.OpDivF64, 1, 1, 5),
				vm3.MakeOp(vm3.OpConstF64K, 6, 0, 3),
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 6),
				vm3.MakeOp(vm3.OpConstF64K, 6, 0, 2),
				vm3.MakeOp(vm3.OpSubF64, 1, 1, 6),
				vm3.MakeOp(vm3.OpConstF64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpConstF64K, 4, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64KBr, 4, 50, 34),
				vm3.MakeOp(vm3.OpMulF64, 5, 3, 3),
				vm3.MakeOp(vm3.OpMulF64, 6, 4, 4),
				vm3.MakeOp(vm3.OpAddF64, 7, 5, 6),
				vm3.MakeOp(vm3.OpCmpGtF64Br, 7, 0, 34),
				vm3.MakeOp(vm3.OpAddF64, 7, 3, 3),
				vm3.MakeOp(vm3.OpFmaF64, 4, 7, fmaC),
				vm3.MakeOp(vm3.OpSubF64, 7, 5, 6),
				vm3.MakeOp(vm3.OpAddF64, 3, 7, 1),
				vm3.MakeOp(vm3.OpAddI64K, 4, 4, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 23),
				vm3.MakeOp(vm3.OpAddI64, 1, 1, 4),
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 12),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 3),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
