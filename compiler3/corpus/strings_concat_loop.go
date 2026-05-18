package corpus

import "mochi/runtime/vm3"

// StringsConcatLoop: concatenate "a" n times via tail-recursive helper.
//
//	fun main(n) {
//	  return concat_loop("a", "a", 0, n)
//	}
//	fun concat_loop(acc, lit, i, n) {
//	  if i >= n return len(acc)
//	  return concat_loop(acc ++ lit, lit, i+1, n)
//	}
//
// The helper takes 2 Cell + 2 I64 params, so it exercises the mixed-bank
// call protocol added in Phase 3.1. The caller arranges per-bank args at
// regs<bank>[op.B + k] where k is the param position; positions in banks
// that don't match the param's bank are unused.
//
// Function 0 ("main"): single I64 param n at regsI64[0].
//
//	NumRegsI64 = 6, NumRegsCell = 4.
//	regsI64[0]   = n (input, also reused for result)
//	regsCell[2]  = "a" (acc, param 0 at arg-base 2)
//	regsCell[3]  = "a" (lit, param 1)
//	regsI64[4]   = 0   (i, param 2)
//	regsI64[5]   = n   (param 3, copied from regsI64[0])
//
//	0  OpConstStrKW   A=2 C=0           ; acc = "a"
//	1  OpConstStrKW   A=3 C=0           ; lit = "a"
//	2  OpConstI64K    A=4 C=0           ; i = 0
//	3  OpMovI64       A=5 B=0           ; n copy
//	4  OpCallMixed    A=0 B=2 C=1 BF=I64; regsI64[0] = concat_loop(...)
//	5  OpReturnI64    A=0
//
// Function 1 ("concat_loop"): ParamBanks=[Cell, Cell, I64, I64].
//
//	NumRegsI64 = 4, NumRegsCell = 2.
//	regsCell[0] = acc, regsCell[1] = lit (params 0,1)
//	regsI64[2]  = i,   regsI64[3]  = n   (params 2,3)
//	regsI64[0]  = result scratch
//
//	0  OpCmpGeI64Br   A=2 B=3 C=4       ; if i >= n jump to 4 (done)
//	1  OpAddI64K      A=2 B=2 C=1       ; i = i + 1
//	2  OpConcatStr    A=0 B=0 C=1       ; acc = acc ++ lit
//	3  OpTailCallMixed A=0 B=0 C=1      ; tail call self
//	4  OpLenStr       A=0 B=0           ; result = len(acc)
//	5  OpReturnI64    A=0
var StringsConcatLoop = &Program{
	Name: "strings_concat_loop",
	Build: func(_ int64) *vm3.Program {
		// Inline "a" as a short-string Cell (CSStr, fits in 5-byte slot).
		strA := vm3.CSStr([]byte{'a'})
		main := &vm3.Function{
			Name:        "main",
			NumRegsI64:  6,
			NumRegsCell: 4,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Consts:      []vm3.Cell{strA},
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstStrKW, 2, 0, 0),
				vm3.MakeOp(vm3.OpConstStrKW, 3, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 4, 0, 0),
				vm3.MakeOp(vm3.OpMovI64, 5, 0, 0),
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 2, C: 1},
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		helper := &vm3.Function{
			Name:        "concat_loop",
			NumRegsI64:  4,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankCell, vm3.BankI64, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 3, 4),
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),
				vm3.MakeOp(vm3.OpConcatStr, 0, 0, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 1),
				vm3.MakeOp(vm3.OpLenStr, 0, 0, 0),
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{main, helper}, Entry: 0}
	},
}
