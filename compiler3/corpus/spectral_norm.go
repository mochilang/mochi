package corpus

import (
	"math"

	"mochi/runtime/vm3"
)

// ExpectSpectralNorm is the Go oracle that mirrors the SpectralNorm
// kernel bit-for-bit. It computes one A*u round (with u seeded to
// ones), then sqrt(sum(u*v) / sum(v*v)) where A(i,j) =
// 1/((i+j)(i+j+1)/2 + i + 1). Both this function and the vm3 kernel
// evaluate the floating-point ops in the same order so the test
// asserts near-equality (1e-12 tolerance).
func ExpectSpectralNorm(n int64) float64 {
	u := make([]float64, n)
	v := make([]float64, n)
	for i := range u {
		u[i] = 1
	}
	for i := int64(0); i < n; i++ {
		var s float64
		for j := int64(0); j < n; j++ {
			denom := (i+j)*(i+j+1)/2 + i + 1
			s += (1.0 / float64(denom)) * u[j]
		}
		v[i] = s
	}
	var vu, vv float64
	for i := int64(0); i < n; i++ {
		vu += u[i] * v[i]
		vv += v[i] * v[i]
	}
	return math.Sqrt(vu / vv)
}

// SpectralNorm: BG `spectral_norm` kernel port from
// compiler2/corpus.BuildSpectralNormKernel. One round of A*u (with u
// seeded to ones), then sqrt(u.v / v.v) where the matrix entries are
// A(i,j) = 1/((i+j)(i+j+1)/2 + i + 1).
//
// Lowered as a single vm3 function with three nested loops:
//
//  1. fill u with 1.0
//  2. for each i compute v[i] = sum_j A(i,j) * u[j]
//  3. accumulate vu = sum(u[i]*v[i]) and vv = sum(v[i]*v[i])
//
// Bench n is baked into the OpNewF64Array constants at Build time so
// the two arrays can be pre-allocated up front (admit shape for the
// j.5.b F64Array JIT lowering on ARM64). For n > 32767 we fall back to
// a push-loop seeded with 0.0 because the OpNewF64Array C field is
// int16; current bench sizes (100, 1000) stay well inside the limit.
//
// Register banks (NumRegsI64=5, NumRegsF64=5, NumRegsCell=2):
//
//	I64: 0 n_in, 1 i, 2 j, 3 acc (ij_sum -> tri -> denomI), 4 sumP1
//	F64: 0 s_or_vu, 1 a_or_ui, 2 uj_or_vi, 3 vv, 4 tmp
//	Cell: 0 u, 1 v
//
// Consts (f64): [0]=0.0, [1]=1.0.
//
// PC map (45 ops):
//
//	 0..1   NewF64Array(n) for u and v
//	 2..7   fill_loop: u[i] = 1.0
//	 8..29  outer matmul i loop containing inner j loop (v[i] = sum)
//	30..41  final dot loop accumulating vu and vv
//	42..44  ratio = vu / vv ; sqrt ; return
//
// Branch targets (uint16(C)):
//
//	fill_loop  pc=4   -> fill_done  pc=8
//	outer_loop pc=9   -> outer_done pc=30
//	inner_loop pc=12  -> inner_done pc=27
//	final_loop pc=33  -> final_done pc=42
var SpectralNorm = &Program{
	Name: "spectral_norm",
	Build: func(n int64) *vm3.Program {
		// The bench harness perturbs args[0] = tc.n + (i & 1) to defeat
		// constant folding, so the kernel may be called with a runtime n
		// of either tc.n or tc.n+1. Both u and v are pre-allocated with
		// a baked size at build time; provision one extra slot so the
		// perturbed call cannot overrun.
		if n < 0 || n > 32766 {
			panic("spectral_norm: n must fit in int16-1 for OpNewF64Array")
		}
		size := int16(n + 1)
		fn := &vm3.Function{
			Name:        "spectral_norm",
			NumRegsI64:  5,
			NumRegsF64:  5,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankF64,
			Consts: []vm3.Cell{
				vm3.CFloat(0.0), // [0]
				vm3.CFloat(1.0), // [1]
			},
			Code: []vm3.Op{
				// pre-alloc u and v
				vm3.MakeOp(vm3.OpNewF64Array, 0, 0, size), // pc=0: u
				vm3.MakeOp(vm3.OpNewF64Array, 1, 0, size), // pc=1: v
				// fill_loop setup
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0), // pc=2: i = 0
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 1), // pc=3: f0 = 1.0
				// fill_loop pc=4
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 8),     // pc=4: if i>=n -> fill_done
				vm3.MakeOp(vm3.OpF64ArraySetF64, 0, 0, 1), // pc=5: u[i] = 1.0
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=6: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 4),           // pc=7 -> fill_loop
				// fill_done pc=8: outer matmul loop
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),    // pc=8: i = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 30),  // pc=9: if i>=n -> outer_done
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),    // pc=10: s = 0.0
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),    // pc=11: j = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 27),  // pc=12: if j>=n -> inner_done
				// denomI = (i+j)*(i+j+1)/2 + i + 1
				vm3.MakeOp(vm3.OpAddI64, 3, 1, 2),      // pc=13: ij_sum = i + j
				vm3.MakeOp(vm3.OpAddI64K, 4, 3, 1),     // pc=14: sumP1 = ij_sum + 1
				vm3.MakeOp(vm3.OpMulI64, 3, 3, 4),      // pc=15: tri_x2 = ij_sum * sumP1
				vm3.MakeOp(vm3.OpDivI64K, 3, 3, 2),     // pc=16: tri = ... / 2
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 1),      // pc=17: tri + i
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),     // pc=18: denomI = ... + 1
				vm3.MakeOp(vm3.OpI64ToF64, 1, 3, 0),    // pc=19: f1 = float64(denomI)
				vm3.MakeOp(vm3.OpConstF64K, 2, 0, 1),   // pc=20: f2 = 1.0
				vm3.MakeOp(vm3.OpDivF64, 1, 2, 1),      // pc=21: a = 1.0 / denomF
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 0, 2), // pc=22: u[j]
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 2),      // pc=23: a * u[j]
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),      // pc=24: s += ...
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),     // pc=25: j++
				vm3.MakeOp(vm3.OpJump, 0, 0, 12),       // pc=26 -> inner_loop
				// inner_done pc=27
				vm3.MakeOp(vm3.OpF64ArraySetF64, 1, 0, 1), // pc=27: v[i] = s
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=28: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 9),           // pc=29 -> outer_loop
				// outer_done pc=30: final dot loops
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),   // pc=30: vu = 0
				vm3.MakeOp(vm3.OpConstF64K, 3, 0, 0),   // pc=31: vv = 0
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),   // pc=32: i = 0
				// final_loop pc=33
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 42),    // pc=33: if i>=n -> final_done
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 1, 0, 1), // pc=34: ui = u[i]
				vm3.MakeOp(vm3.OpF64ArrayGetF64, 2, 1, 1), // pc=35: vi = v[i]
				vm3.MakeOp(vm3.OpMulF64, 4, 1, 2),         // pc=36: ui*vi
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 4),         // pc=37: vu += ...
				vm3.MakeOp(vm3.OpMulF64, 4, 2, 2),         // pc=38: vi*vi
				vm3.MakeOp(vm3.OpAddF64, 3, 3, 4),         // pc=39: vv += ...
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),        // pc=40: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 33),          // pc=41 -> final_loop
				// final_done pc=42
				vm3.MakeOp(vm3.OpDivF64, 0, 0, 3),    // pc=42: ratio = vu / vv
				vm3.MakeOp(vm3.OpSqrtF64, 0, 0, 0),   // pc=43: sqrt
				vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0), // pc=44
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
