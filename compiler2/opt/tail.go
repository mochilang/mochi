package opt

import "mochi/compiler2/ir"

// TailCall rewrites every block whose last two instructions are
//
//	r = Call fn(args)
//	Ret r
//
// into a single OpTailCall, dropping the Ret. The call no longer
// grows the frame stack at runtime: the vm2 OpTailCall handler reuses
// or swaps the frame in place.
//
// Returns the number of call sites rewritten.
func TailCall(f *ir.Function) int {
	n := 0
	for _, blk := range f.Blocks {
		if len(blk.Insts) < 2 {
			continue
		}
		retVID := blk.Insts[len(blk.Insts)-1]
		callVID := blk.Insts[len(blk.Insts)-2]
		ret := f.Values[retVID]
		call := f.Values[callVID]
		if ret.Op != ir.OpRet || call.Op != ir.OpCall {
			continue
		}
		if len(ret.Args) != 1 || ret.Args[0] != callVID {
			continue
		}
		// Rewrite: turn the call into a tail call (still a value
		// node, but now a terminator). Drop the Ret.
		f.Values[callVID].Op = ir.OpTailCall
		// Invalidate the Ret slot.
		f.Values[retVID] = ir.Inst{Op: ir.OpInvalid}
		blk.Insts = blk.Insts[:len(blk.Insts)-1]
		n++
	}
	return n
}
