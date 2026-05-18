package corpus

import "mochi/runtime/vm3"

// F64Threshold: walk i=1..n and return the first i for which 1.0/f64(i)
// is strictly less than 0.1 (mathematically i=11). Returns 0 if no such
// i exists in [1, n]. Exercises the Phase 6.2b F64 cmp/br path:
//
//   - OpCmpLtF64Br on the (y < 0.1) test
//   - OpDivF64 to compute y = 1.0 / f64(i)
//   - OpI64ToF64 to cast the loop counter into the f64 bank
//   - OpConstF64K for the 1.0 numerator and 0.1 threshold
//   - mixed-bank return: OpReturnI64 / OpReturnConstK from an f64 fn
//
//	fun threshold(n) {
//	  var i = 1
//	  while i <= n {
//	    var y = 1.0 / f64(i)
//	    if y < 0.1 { return i }
//	    i = i + 1
//	  }
//	  return 0
//	}
//
// Bank: i64 + f64. NumRegsI64 = 2 (n, i); NumRegsF64 = 4 (one, ten_inv,
// x_f, y).
//
//	0  ConstI64K  A=1, C=1          ; i = 1
//	1  ConstF64K  A=0, C=0          ; F0 = 1.0
//	2  ConstF64K  A=1, C=1          ; F1 = 0.1
//	3  CmpGtI64Br A=1, B=0, C=9     ; if i > n jump to 9 (return 0)
//	4  I64ToF64   A=2, B=1          ; F2 = f64(i)
//	5  DivF64     A=3, B=0, C=2     ; y = 1.0 / F2
//	6  CmpLtF64Br A=3, B=1, C=10    ; if y < 0.1 jump to 10 (return i)
//	7  AddI64K    A=1, B=1, C=1     ; i = i + 1
//	8  Jump              C=3        ; back to test
//	9  ReturnConstK     C=0         ; return 0
//	10 ReturnI64  A=1               ; return i
var F64Threshold = &Program{
	Name: "f64_threshold",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "f64_threshold",
			NumRegsI64: 2,
			NumRegsF64: 4,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Consts:     []vm3.Cell{vm3.CFloat(1.0), vm3.CFloat(0.1)},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstF64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 1, 0, 9),
				vm3.MakeOp(vm3.OpI64ToF64, 2, 1, 0),
				vm3.MakeOp(vm3.OpDivF64, 3, 0, 2),
				vm3.MakeOp(vm3.OpCmpLtF64Br, 3, 1, 10),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 3),
				vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 0),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
