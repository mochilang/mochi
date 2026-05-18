package corpus

import "mochi/runtime/vm3"

// FibRec: recursive Fibonacci.
//
//	fun fib(n) {
//	  if n < 2 return n
//	  return fib(n-1) + fib(n-2)
//	}
//
// Bank: i64. NumRegsI64 = 5 (n, arg1, r1, arg2, r2).
//
// Bytecode:
//
//	0  CmpLtI64KBr A=0, B=2, C=7    ; if n < 2 jump to 7 (base: return n)
//	1  SubI64K     A=1, B=0, C=1    ; arg1 = n - 1
//	2  CallI64     A=2, B=1, C=0    ; r1 = fib(arg1)
//	3  SubI64K     A=3, B=0, C=2    ; arg2 = n - 2
//	4  CallI64     A=4, B=3, C=0    ; r2 = fib(arg2)
//	5  AddI64      A=2, B=2, C=4    ; r1 = r1 + r2
//	6  ReturnI64   A=2
//	7  ReturnI64   A=0
var FibRec = &Program{
	Name: "fib_rec",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "fib_rec",
			NumRegsI64: 5,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpLtI64KBr, 0, 2, 7),
				vm3.MakeOp(vm3.OpSubI64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpCallI64, 2, 1, 0),
				vm3.MakeOp(vm3.OpSubI64K, 3, 0, 2),
				vm3.MakeOp(vm3.OpCallI64, 4, 3, 0),
				vm3.MakeOp(vm3.OpAddI64, 2, 2, 4),
				vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
