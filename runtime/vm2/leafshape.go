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

// The leaf shortcut is consumed inline in each of the eight call-site
// dispatch arms in eval.go. Earlier iterations of this code routed the
// guard/materialize step through tryLeafA1 / tryLeafA2 / tryLeafA2Guard1
// + leafReturnCell helpers, but Go's inliner rejected them at cost > 80
// (one materialize-newPair call plus the guard switch); function-call
// overhead then dominated the saved frame setup on hot kernels. Iter 4
// of MEP-39 §6.10 inlines the check directly: each arm reads
// LeafKind, LeafGuardReg, and LeafGuardK from the callee and short-
// circuits to a CInt or vm.newPair before paying the frame-push cost.
