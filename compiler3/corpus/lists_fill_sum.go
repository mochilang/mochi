package corpus

import "mochi/runtime/vm3"

// ListsFillSum: fill a list with [0..n) then sum it.
//
//	fun main(n) {
//	  xs := []
//	  fill(xs, 0, n)
//	  return sum(xs, 0, n, 0)
//	}
//	fun fill(xs, i, n) {     // returns unit
//	  if i >= n return
//	  xs.push(i)
//	  return fill(xs, i+1, n)
//	}
//	fun sum(xs, j, n, acc) {
//	  if j >= n return acc
//	  return sum(xs, j+1, n, acc + xs[j])
//	}
//
// Two tail-recursive helpers exercise the list opcodes (NewList,
// ListPushI64, ListGetI64). The helpers also use the mixed-bank call
// ABI (Cell + I64 params) added in Phase 3.1.
//
// Function 0 ("main"): single I64 param n.
//
//	NumRegsI64 = 4, NumRegsCell = 2.
//	regsI64[0]  = n input (also final result)
//	regsCell[0] = xs (call arg position 0)
//
// Function 1 ("fill"): ParamBanks=[Cell, I64, I64]. NumRegsI64 = 3,
// NumRegsCell = 1. regsCell[0]=xs, regsI64[1]=i, regsI64[2]=n.
//
// Function 2 ("sum"): ParamBanks=[Cell, I64, I64, I64]. NumRegsI64 = 4,
// NumRegsCell = 1. regsCell[0]=xs, regsI64[1]=j, regsI64[2]=n,
// regsI64[3]=acc. regsI64[0] is scratch for ListGetI64 destination.
var ListsFillSum = &Program{
	Name: "lists_fill_sum",
	Build: func(_ int64) *vm3.Program {
		main := &vm3.Function{
			Name:        "main",
			NumRegsI64:  4,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewList, 0, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpMovI64, 2, 0, 0),
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 3, B: 0, C: 1},
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 0),
				vm3.MakeOp(vm3.OpMovI64, 2, 0, 0),
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 0, B: 0, C: 2},
				vm3.MakeOp(vm3.OpReturnI64, 0, 0, 0),
			},
		}
		fill := &vm3.Function{
			Name:        "fill",
			NumRegsI64:  3,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 2, 4),
				vm3.MakeOp(vm3.OpListPushI64, 0, 1, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 1),
				vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 0),
			},
		}
		sum := &vm3.Function{
			Name:        "sum",
			NumRegsI64:  4,
			NumRegsCell: 1,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64, vm3.BankI64, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGeI64Br, 1, 2, 5),
				vm3.MakeOp(vm3.OpListGetI64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnI64, 3, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{main, fill, sum}, Entry: 0}
	},
}
