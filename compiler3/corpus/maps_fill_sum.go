package corpus

import "mochi/runtime/vm3"

// MapsFillSum: fill a map with i -> i for i in [0..n) then sum the
// values. Map analogue of lists_fill_sum (shared expected result
// n*(n-1)/2, validating MapSetI64/MapGetI64).
//
//	fun main(n) {
//	  m := {}
//	  fill(m, 0, n)
//	  return sum(m, 0, n, 0)
//	}
//	fun fill(m, i, n) {     // returns unit
//	  if i >= n return
//	  m[i] = i
//	  return fill(m, i+1, n)
//	}
//	fun sum(m, j, n, acc) {
//	  if j >= n return acc
//	  return sum(m, j+1, n, acc + m[j])
//	}
//
// Function 0 ("main"): single I64 param n. NumRegsI64 = 4, NumRegsCell = 2.
// regsI64[0] = n input (also final result). regsCell[0] = m.
//
// Function 1 ("fill"): ParamBanks=[Cell, I64, I64]. NumRegsI64 = 3,
// NumRegsCell = 1. regsCell[0] = m, regsI64[1] = i, regsI64[2] = n.
//
// Function 2 ("sum"): ParamBanks=[Cell, I64, I64, I64]. NumRegsI64 = 4,
// NumRegsCell = 1. regsCell[0] = m, regsI64[1] = j, regsI64[2] = n,
// regsI64[3] = acc. regsI64[0] is scratch for OpMapGetI64I64 destination.
var MapsFillSum = &Program{
	Name: "maps_fill_sum",
	Build: func(_ int64) *vm3.Program {
		main := &vm3.Function{
			Name:        "main",
			NumRegsI64:  4,
			NumRegsCell: 2,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpNewMap, 0, 0, 0),
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
				vm3.MakeOp(vm3.OpMapSetI64I64, 0, 1, 1),
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
				vm3.MakeOp(vm3.OpMapGetI64I64, 0, 0, 1),
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 0),
				vm3.MakeOp(vm3.OpAddI64K, 1, 1, 1),
				vm3.MakeOp(vm3.OpTailCallMixed, 0, 0, 2),
				vm3.MakeOp(vm3.OpReturnI64, 3, 0, 0),
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{main, fill, sum}, Entry: 0}
	},
}
