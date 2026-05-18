package corpus

import "mochi/runtime/vm3"

// F64DotSum: s = 0.5 * (0 + 1 + ... + n-1) = n*(n-1)/4 (as f64).
// Drives the loop with an i64 counter and accumulates in f64, so it
// exercises:
//   - OpI64ToF64 (i64 -> f64 cast)
//   - OpMulF64, OpAddF64 (f64 arithmetic)
//   - OpConstF64K (f64 constant load from pool)
//   - OpReturnF64
// alongside the existing i64 cmp/br/AddK kernel shape.
//
//	fun dot_sum(n) {
//	  var s = 0.0
//	  var k = 0.5
//	  var i = 0
//	  while i < n {
//	    var x = f64(i)
//	    s = s + x * k
//	    i = i + 1
//	  }
//	  return s
//	}
//
// Bank: i64 + f64. NumRegsI64 = 2 (n, i); NumRegsF64 = 3 (s, x, k).
//
//	0  ConstF64K  A=0, C=0          ; s = 0.0
//	1  ConstF64K  A=2, C=1          ; k = 0.5
//	2  ConstI64K  A=1, C=0          ; i = 0
//	3  CmpGeI64Br A=1, B=0, C=9     ; if i >= n jump to 9
//	4  I64ToF64   A=1, B=1          ; x_f64 = f64(i_i64)
//	5  MulF64     A=1, B=1, C=2     ; x = x * k
//	6  AddF64     A=0, B=0, C=1     ; s = s + x
//	7  AddI64K    A=1, B=1, C=1     ; i = i + 1
//	8  Jump              C=3        ; back to test
//	9  ReturnF64  A=0
var F64DotSum = &Program{
	Name: "f64_dot_sum",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "f64_dot_sum",
			NumRegsI64: 2,
			NumRegsF64: 3,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankF64,
			Consts:     []vm3.Cell{vm3.CFloat(0.0), vm3.CFloat(0.5)},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstF64K, 2, 0, 1),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 0, 9),
				vm3.MakeOp(vm3.OpI64ToF64, 1, 1, 0),
				vm3.MakeOp(vm3.OpMulF64, 1, 1, 2),
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 3),
				vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
