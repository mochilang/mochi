package corpus

import "mochi/runtime/vm3"

// ExpectBinaryTrees mirrors the BG `binary_trees` Go template: build
// 2^depth depth-`depth` trees with two-element nodes, return the total
// node count summed across all `iters` trees. Each depth-`d` tree has
// 2^(d+1)-1 nodes by induction (2 children + self), so the result is
// `iters * (2^(depth+1) - 1) = 2^depth * (2^(depth+1) - 1)`. For
// depth=10 this is 1024 * 2047 = 2,096,128; for depth=12 it is
// 4096 * 8191 = 33,550,336.
func ExpectBinaryTrees(depth int64) int64 {
	if depth <= 0 {
		return 1
	}
	iters := int64(1)
	for range depth {
		iters *= 2
	}
	nodes := int64(1)
	for range depth + 1 {
		nodes *= 2
	}
	nodes--
	return iters * nodes
}

// BinaryTrees: BG `binary_trees` cross-lang shape port. Three vm3
// functions exercise the new Phase 6.3.4.m.1 pair-arena ops
// (OpNewPair, OpPairFst, OpPairSnd) end-to-end:
//
//  1. make_tree(d) -> Cell: allocates 2^(d+1)-1 pairs recursively.
//     Leaves are OpNewPair(reg, reg) for arbitrary `reg`; the slot
//     contents are never read because check_tree terminates on d==0
//     before touching the pair.
//  2. check_tree(t, d) -> i64: walks the tree, returning 2^(d+1)-1 by
//     reading PairFst/PairSnd at every non-leaf and recursing.
//  3. main(depth) -> i64: 2^depth iterations of
//     `total += check_tree(make_tree(depth), depth)`.
//
// Bank usage by callee:
//
//	make_tree:   ParamBanks=[I64], ResultBank=Cell.
//	check_tree:  ParamBanks=[Cell, I64], ResultBank=I64.
//	main:        ParamBanks=[I64], ResultBank=I64.
//
// Per-node cost: 2 pair reads + 2 cross-fn calls + 2 i64 adds on the
// check side, 1 OpNewPair on the make side. Allocation pressure is one
// vmPair slot per node (leaves included), matching the Go peer's one
// slice header per node.
var BinaryTrees = &Program{
	Name: "binary_trees",
	Build: func(depth int64) *vm3.Program {
		// make_tree(d): allocate pair tree.
		//   regsI64[0]  = d (param)
		//   regsI64[1]  = d-1 (arg slot for recursive call)
		//   regsCell[0] = fst child (return slot for first recursive call)
		//   regsCell[1] = snd child (return slot for second recursive call)
		//   regsCell[2] = pair result
		makeTree := &vm3.Function{
			Name:        "make_tree",
			NumRegsI64:  2,
			NumRegsCell: 3,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankCell,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGtI64KBr, 0, 0, 3),                                       // pc=0: if d > 0 -> 3
				vm3.MakeOp(vm3.OpNewPair, 2, 0, 0),                                           // pc=1: leaf pair(reg0,reg0)
				vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                        // pc=2: return leaf
				vm3.MakeOp(vm3.OpSubI64K, 1, 0, 1),                                           // pc=3: arg = d-1
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 0, B: 1, C: 1},    // pc=4: regsCell[0] = make_tree(d-1)
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 1, B: 1, C: 1},    // pc=5: regsCell[1] = make_tree(d-1)
				vm3.MakeOp(vm3.OpNewPair, 2, 0, 1),                                           // pc=6: pair(fst, snd)
				vm3.MakeOp(vm3.OpReturnCell, 2, 0, 0),                                        // pc=7: return pair
			},
		}
		// check_tree(t, d): walk tree, return node count.
		// Callee sees regsCell[0]=t, regsI64[1]=d (ParamBanks=[Cell, I64]).
		//   regsI64[1]  = d (param)
		//   regsI64[2]  = sum (result)
		//   regsI64[3]  = d-1 (arg1 for recursive call, B=2 -> regsI64[B+1])
		//   regsI64[4]  = left count return
		//   regsI64[5]  = right count return
		//   regsCell[0] = t (param)
		//   regsCell[2] = current child (arg0 for recursive call, regsCell[B])
		checkTree := &vm3.Function{
			Name:        "check_tree",
			NumRegsI64:  6,
			NumRegsCell: 3,
			ParamBanks:  []vm3.Bank{vm3.BankCell, vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpCmpGtI64KBr, 1, 0, 2),                                       // pc=0: if d > 0 -> 2
				vm3.MakeOp(vm3.OpReturnConstK, 0, 0, 1),                                      // pc=1: leaf returns 1
				vm3.MakeOp(vm3.OpSubI64K, 3, 1, 1),                                           // pc=2: d_minus_1 = d-1
				vm3.MakeOp(vm3.OpPairFst, 2, 0, 0),                                           // pc=3: regsCell[2] = PairFst(t)
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 4, B: 2, C: 2},     // pc=4: left = check_tree(fst, d-1)
				vm3.MakeOp(vm3.OpPairSnd, 2, 0, 0),                                           // pc=5: regsCell[2] = PairSnd(t)
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 5, B: 2, C: 2},     // pc=6: right = check_tree(snd, d-1)
				vm3.MakeOp(vm3.OpAddI64, 2, 4, 5),                                            // pc=7: sum = left + right
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),                                           // pc=8: sum += 1
				vm3.MakeOp(vm3.OpReturnI64, 2, 0, 0),                                         // pc=9: return sum
			},
		}
		// main(depth) -> i64.
		//   regsI64[0]  = depth (param)
		//   regsI64[1]  = iters
		//   regsI64[2]  = i (outer loop counter)
		//   regsI64[3]  = total (accumulator)
		//   regsI64[4]  = arg slot for make_tree (depth), B=4
		//   regsI64[5]  = arg slot for check_tree's d (also reused as k during 2^depth pre-loop)
		//   regsI64[6]  = check_tree return
		//   regsCell[4] = tree (make_tree dst, check_tree arg0 at B=4)
		// Pre-loop: iters = 2^depth.
		// Main loop: total += check_tree(make_tree(depth), depth) repeated iters times.
		main := &vm3.Function{
			Name:        "binary_trees_main",
			NumRegsI64:  7,
			NumRegsCell: 5,
			ParamBanks:  []vm3.Bank{vm3.BankI64},
			ResultBank:  vm3.BankI64,
			Code: []vm3.Op{
				vm3.MakeOp(vm3.OpConstI64K, 1, 0, 1),                                         // pc=0:  iters = 1
				vm3.MakeOp(vm3.OpConstI64K, 5, 0, 0),                                         // pc=1:  k = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 5, 0, 6),                                        // pc=2:  if k >= depth -> 6
				vm3.MakeOp(vm3.OpMulI64K, 1, 1, 2),                                           // pc=3:  iters *= 2
				vm3.MakeOp(vm3.OpAddI64K, 5, 5, 1),                                           // pc=4:  k++
				vm3.MakeOp(vm3.OpJump, 0, 0, 2),                                              // pc=5:  -> 2
				vm3.MakeOp(vm3.OpConstI64K, 2, 0, 0),                                         // pc=6:  i = 0
				vm3.MakeOp(vm3.OpConstI64K, 3, 0, 0),                                         // pc=7:  total = 0
				vm3.MakeOp(vm3.OpCmpGeI64Br, 2, 1, 16),                                       // pc=8:  if i >= iters -> 16
				vm3.MakeOp(vm3.OpMovI64, 4, 0, 0),                                            // pc=9:  regsI64[4] = depth
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankCell), A: 4, B: 4, C: 1},    // pc=10: regsCell[4] = make_tree(depth)
				vm3.MakeOp(vm3.OpMovI64, 5, 0, 0),                                            // pc=11: regsI64[5] = depth
				{Code: vm3.OpCallMixed, BankFlags: uint8(vm3.BankI64), A: 6, B: 4, C: 2},     // pc=12: regsI64[6] = check_tree(regsCell[4], regsI64[5])
				vm3.MakeOp(vm3.OpAddI64, 3, 3, 6),                                            // pc=13: total += check_ret
				vm3.MakeOp(vm3.OpAddI64K, 2, 2, 1),                                           // pc=14: i++
				vm3.MakeOp(vm3.OpJump, 0, 0, 8),                                              // pc=15: -> 8
				vm3.MakeOp(vm3.OpReturnI64, 3, 0, 0),                                         // pc=16: return total
			},
		}
		return &vm3.Program{Funcs: []*vm3.Function{main, makeTree, checkTree}, Entry: 0}
	},
}
