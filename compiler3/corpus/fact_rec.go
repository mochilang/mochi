package corpus

import "mochi/runtime/vm3"

// FactRec: recursive factorial.
//
//	fun fact(n) {
//	  if n <= 1 return 1
//	  return n * fact(n-1)
//	}
//
// Bank: i64. NumRegsI64 = 3 (n, arg, result).
//
// Bytecode:
//
//	0  CmpLeI64KBr A=0, B=1, C=5    ; if n <= 1 jump to 5 (base case)
//	1  SubI64K     A=1, B=0, C=1    ; arg = n - 1
//	2  CallI64     A=2, B=1, C=0    ; result = fact(arg); call self (func idx 0)
//	3  MulI64      A=2, B=0, C=2    ; result = n * result
//	4  ReturnI64   A=2
//	5  ReturnConstK            C=1  ; return 1
var FactRec = &Program{
	Name: "fact_rec",
	Build: func(_ int64) *vm3.Program {
		fn := &vm3.Function{
			Name:       "fact_rec",
			NumRegsI64: 3,
			ParamBanks: []vm3.Bank{vm3.BankI64},
			ResultBank: vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpLeI64KBr, 0, 1, 5),
				vm3.MakeOp(vm3.OpSubI64K, 1, 0, 1),
				vm3.MakeOp(vm3.OpCallI64, 2, 1, 0),
				vm3.MakeOp(vm3.OpMulI64, 2, 0, 2),
				vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),
				vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 1),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{fn}, Entry: 0}
	},
}
