// Package opt contains IR optimization passes. Each pass takes an
// ir.Function (sometimes ir.Module) and rewrites it in place. Passes
// must preserve the verifier's structural invariants.
package opt

import "mochi/compiler2/ir"

// ConstFold folds arithmetic and comparison ops whose operands are
// both OpConstI64 / OpConstBool. The original instruction is rewritten
// in place to the new constant op so existing ValueIDs and uses remain
// valid; downstream DCE removes any operands that lose their last use.
//
// Returns the number of instructions folded.
func ConstFold(f *ir.Function) int {
	folded := 0
	for i := range f.Values {
		ins := &f.Values[i]
		switch ins.Op {
		case ir.OpAddI64, ir.OpSubI64, ir.OpMulI64,
			ir.OpLessI64, ir.OpLessEqI64, ir.OpEqualI64:
		default:
			continue
		}
		if len(ins.Args) != 2 {
			continue
		}
		l := f.Values[ins.Args[0]]
		r := f.Values[ins.Args[1]]
		if l.Op != ir.OpConstI64 || r.Op != ir.OpConstI64 {
			continue
		}
		lv, rv := l.Aux, r.Aux
		switch ins.Op {
		case ir.OpAddI64:
			*ins = ir.Inst{Op: ir.OpConstI64, Type: ir.TI64, Aux: lv + rv}
		case ir.OpSubI64:
			*ins = ir.Inst{Op: ir.OpConstI64, Type: ir.TI64, Aux: lv - rv}
		case ir.OpMulI64:
			*ins = ir.Inst{Op: ir.OpConstI64, Type: ir.TI64, Aux: lv * rv}
		case ir.OpLessI64:
			*ins = ir.Inst{Op: ir.OpConstBool, Type: ir.TBool, Aux: boolToAux(lv < rv)}
		case ir.OpLessEqI64:
			*ins = ir.Inst{Op: ir.OpConstBool, Type: ir.TBool, Aux: boolToAux(lv <= rv)}
		case ir.OpEqualI64:
			*ins = ir.Inst{Op: ir.OpConstBool, Type: ir.TBool, Aux: boolToAux(lv == rv)}
		}
		folded++
	}
	return folded
}

func boolToAux(b bool) int64 {
	if b {
		return 1
	}
	return 0
}

// useCounts returns a slice indexed by ValueID giving the number of
// instructions (including phi inputs) that reference each value.
func useCounts(f *ir.Function) []int {
	uc := make([]int, len(f.Values))
	for _, ins := range f.Values {
		for _, a := range ins.Args {
			if int(a) < len(uc) {
				uc[a]++
			}
		}
	}
	return uc
}

// DCE removes pure instructions whose result is unused. Side-effecting
// instructions (OpCall) and terminators are preserved unconditionally.
// Returns the number of instructions removed.
func DCE(f *ir.Function) int {
	uc := useCounts(f)
	removed := 0
	// Iterate to fixed point: removing one inst may zero another's
	// use count.
	for {
		changed := false
		for bi, blk := range f.Blocks {
			out := blk.Insts[:0]
			for _, vid := range blk.Insts {
				ins := f.Values[vid]
				if isPure(ins.Op) && uc[vid] == 0 {
					for _, a := range ins.Args {
						if int(a) < len(uc) && uc[a] > 0 {
							uc[a]--
						}
					}
					f.Values[vid] = ir.Inst{Op: ir.OpInvalid}
					removed++
					changed = true
					continue
				}
				out = append(out, vid)
			}
			f.Blocks[bi].Insts = out
		}
		if !changed {
			break
		}
	}
	return removed
}

func isPure(op ir.Op) bool {
	switch op {
	case ir.OpParam,
		ir.OpConstI64, ir.OpConstBool,
		ir.OpAddI64, ir.OpSubI64, ir.OpMulI64,
		ir.OpLessI64, ir.OpLessEqI64, ir.OpEqualI64,
		ir.OpPhi:
		return true
	}
	return false
}
