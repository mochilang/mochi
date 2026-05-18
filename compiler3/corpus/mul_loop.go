package corpus

import "mochi/runtime/vm3"

// MulLoop: product [1, n) = 1 * 2 * ... * n-1 = (n-1)!.
// Matches compiler2/corpus.ExpectMulLoop.
//
//	fun mul(n) {
//	  var p = 1
//	  var i = 1
//	  while i < n {
//	    p = p * i
//	    i = i + 1
//	  }
//	  return p
//	}
//
// Bank: i64. NumRegsI64 = 3 (n, p, i).
//
//	0  ConstI64K  A=1, C=1          ; p = 1
//	1  ConstI64K  A=2, C=1          ; i = 1
//	2  CmpGeI64Br A=2, B=0, C=6     ; if i >= n jump to 6
//	3  MulI64     A=1, B=1, C=2     ; p = p * i
//	4  AddI64K    A=2, B=2, C=1     ; i = i + 1
//	5  Jump                  C=2    ; back to loop test
//	6  ReturnI64  A=1
var MulLoop = &Program{
	Name: "mul_loop",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "mul_loop",
			NumRegsI64: 3,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 1),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 0, 6),
				vm3.MakeOp(vm3.OpMulI64, 1, 1, 2),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
