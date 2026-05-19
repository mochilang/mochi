package corpus

import "mochi/runtime/vm3"

// F64SqrtSum: s = sqrt(1) + sqrt(2) + ... + sqrt(n). Drives the loop
// with an i64 counter and accumulates in f64, exercising OpSqrtF64
// alongside the existing i64 cmp/br/AddK + f64 add shape.
//
//	fun sqrt_sum(n) {
//	  var s = 0.0
//	  var i = 1
//	  while i <= n {
//	    s = s + sqrt(f64(i))
//	    i = i + 1
//	  }
//	  return s
//	}
//
// Bank: i64 + f64. NumRegsI64 = 2 (n, i); NumRegsF64 = 2 (s, x).
// OpConstI64K uses op.C as a literal i64 immediate (no Consts lookup).
//
//	0  ConstF64K  A=0, C=0          ; s = Consts[0] = 0.0
//	1  ConstI64K  A=1, C=1          ; i = 1 (immediate)
//	2  CmpGtI64Br A=1, B=0, C=8     ; if i > n jump to 8
//	3  I64ToF64   A=1, B=1          ; x = f64(i)
//	4  SqrtF64    A=1, B=1          ; x = sqrt(x)
//	5  AddF64     A=0, B=0, C=1     ; s = s + x
//	6  AddI64K    A=1, B=1, C=1     ; i = i + 1
//	7  Jump              C=2        ; back to test
//	8  ReturnF64  A=0
var F64SqrtSum = &Program{
	Name: "f64_sqrt_sum",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "f64_sqrt_sum",
			NumRegsI64: 2,
			NumRegsF64: 2,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankF64,
			Consts:     []vm3.Cell{vm3.CFloat(0.0)},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstF64K, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpCmpGtI64Br, 1, 0, 8),
				vm3.MakeOp(vm3.OpI64ToF64, 1, 1, 0),
				vm3.MakeOp(vm3.OpSqrtF64, 1, 1, 0),
				vm3.MakeOp(vm3.OpAddF64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnF64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
