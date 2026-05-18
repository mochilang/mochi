package corpus

import "mochi/runtime/vm3"

// FibIter: iterative Fibonacci.
//
//	fun fib(n) {
//	  var a = 0
//	  var b = 1
//	  for i in range(n) {
//	    var t = a + b
//	    a = b
//	    b = t
//	  }
//	  return a
//	}
//
// Bank: i64 only. NumRegsI64 = 5 (n, a, b, i, t).
//
// Bytecode:
//
//	 0  ConstI64K  a=1, imm=0       ; a = 0
//	 1  ConstI64K  a=2, imm=1       ; b = 1
//	 2  ConstI64K  a=3, imm=0       ; i = 0
//	 3  CmpGeI64Br A=3, B=0, C=8    ; if i >= n jump to 8 (end)
//	 4  AddI64     A=4, B=1, C=2    ; t = a + b
//	 5  MovI64     A=1, B=2         ; a = b
//	 6  MovI64     A=2, B=4         ; b = t
//	 7  AddI64K    A=3, B=3, C=1    ; i = i + 1; falls through to jump
//	 8  Jump                  C=3   ; back to loop test     <-- wait this overlaps end
//	 9  ReturnI64  A=1              ; return a
//
// Fix the layout: end label must be after the back-jump. Re-emit with
// 11 ops:
//
//	 0  ConstI64K  a=1, imm=0       ; a = 0
//	 1  ConstI64K  a=2, imm=1       ; b = 1
//	 2  ConstI64K  a=3, imm=0       ; i = 0
//	 3  CmpGeI64Br A=3, B=0, C=9    ; if i >= n jump to 9 (end)
//	 4  AddI64     A=4, B=1, C=2    ; t = a + b
//	 5  MovI64     A=1, B=2         ; a = b
//	 6  MovI64     A=2, B=4         ; b = t
//	 7  AddI64K    A=3, B=3, C=1    ; i = i + 1
//	 8  Jump                  C=3   ; back to loop test
//	 9  ReturnI64  A=1              ; return a
var FibIter = &Program{
	Name: "fib_iter",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "fib_iter",
			NumRegsI64: 5,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 1),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				vm3.MakeOp(vm3.OpCmpGeI64Br, 3, 0, 9),
				vm3.MakeOp(vm3.OpAddI64, 4, 1, 2),
				vm3.MakeOp(vm3.OpMovI64, 1, 2, 0),
				vm3.MakeOp(vm3.OpMovI64, 2, 4, 0),
				vm3.MakeOp(vm3.OpAddI64K, 3, 3, 1),
				vm3.MakeOp(vm3.OpJump, 0, 0, 3),
				vm3.MakeOp(vm3.OpReturnI64, 1, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
