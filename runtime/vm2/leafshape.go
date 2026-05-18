package vm2

// AnalyzeLeafShape inspects fn.Code for a "constant-leaf base case"
// entry sequence and, if it matches, populates the LeafKind / LeafGuard*
// / LeafReturn* fields. Called once per Function at program build time
// (see compiler2/emit). The shortcut is consumed by the call-site
// dispatch handlers in eval.go (OpCall* families).
//
// Pattern matched (MEP-39 §6.10 iter 3):
//
//	pc=0: OpJumpIfNotEqualI64K  A=guardReg, B=K,            C=2
//	pc=1: OpReturnI64K          A=Vint                                  (LeafKindReturnI64K)
//	      OpReturnNewPairKK     B=Pa, C=Pb                              (LeafKindReturnNewPairKK)
//
// In the source, this corresponds to:
//
//	func f(... p_guard ...) {
//	    if p_guard != K { ... }           // jump-not-equal skips the leaf
//	    return <constant>                  // leaf base case
//	    ...
//	}
//
// Only the entry pair is consulted, so the branch path is free to be
// arbitrarily complex. The guard reg must be a parameter (index <
// NumParams) so the caller's "argument at this position" check is
// well-defined.
func AnalyzeLeafShape(fn *Function) {
	fn.LeafKind = LeafKindNone
	if len(fn.Code) < 2 {
		return
	}
	g := fn.Code[0]
	l := fn.Code[1]
	if g.Op != OpJumpIfNotEqualI64K {
		return
	}
	if g.C != 2 {
		// jump must skip exactly the leaf return; otherwise the leaf
		// path isn't the fall-through next instruction.
		return
	}
	if int(g.A) >= fn.NumParams {
		// guard reg must be a parameter so the caller can supply it.
		return
	}
	switch l.Op {
	case OpReturnI64K:
		fn.LeafKind = LeafKindReturnI64K
		fn.LeafGuardReg = g.A
		fn.LeafGuardK = g.B
		fn.LeafReturnA = l.A
	case OpReturnNewPairKK:
		fn.LeafKind = LeafKindReturnNewPairKK
		fn.LeafGuardReg = g.A
		fn.LeafGuardK = g.B
		fn.LeafReturnB = l.B
		fn.LeafReturnC = l.C
	}
}

// leafReturnCell materializes the cached leaf return value. Caller must
// ensure fn.LeafKind != LeafKindNone before invoking.
func (fn *Function) leafReturnCell(vm *VM) Cell {
	if fn.LeafKind == LeafKindReturnI64K {
		return CInt(int64(fn.LeafReturnA))
	}
	// LeafKindReturnNewPairKK
	return vm.newPair(CInt(int64(fn.LeafReturnB)), CInt(int64(fn.LeafReturnC)))
}

// tryLeafA1 returns (leafReturn, true) when fn matches the cached leaf
// shape on a single-arg call (guard reg must be 0). Used by OpCallA1
// and OpCallSelfA1 dispatch.
func (fn *Function) tryLeafA1(vm *VM, a0 Cell) (Cell, bool) {
	if fn.LeafGuardReg != 0 {
		return Cell{}, false
	}
	if a0.Int() != int64(fn.LeafGuardK) {
		return Cell{}, false
	}
	return fn.leafReturnCell(vm), true
}

// tryLeafA2 returns (leafReturn, true) when fn matches the cached leaf
// shape on a two-arg call. Used by OpCallA2 / OpCallSelfA2 dispatch.
// Both args are inspected, indexed by fn.LeafGuardReg.
func (fn *Function) tryLeafA2(vm *VM, a0, a1 Cell) (Cell, bool) {
	var g int64
	switch fn.LeafGuardReg {
	case 0:
		g = a0.Int()
	case 1:
		g = a1.Int()
	default:
		return Cell{}, false
	}
	if g != int64(fn.LeafGuardK) {
		return Cell{}, false
	}
	return fn.leafReturnCell(vm), true
}

// tryLeafA2Guard1 returns (leafReturn, true) when fn matches the cached
// leaf shape AND the guard register is arg1. Used by the Pair-fused
// call sites (OpPairFst/SndCallA2, OpPairFst/SndCallSelfA2): arg0 is a
// pair half there, which the guard cannot meaningfully be compared
// against without extracting the half, so we only handle guard-on-arg1.
func (fn *Function) tryLeafA2Guard1(vm *VM, a1 Cell) (Cell, bool) {
	if fn.LeafGuardReg != 1 {
		return Cell{}, false
	}
	if a1.Int() != int64(fn.LeafGuardK) {
		return Cell{}, false
	}
	return fn.leafReturnCell(vm), true
}
