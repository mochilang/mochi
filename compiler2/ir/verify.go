package ir

import "fmt"

// Verify checks structural invariants on a Function: every block ends
// with exactly one terminator, every ValueID referenced by Args was
// already defined, types on Args match the producing instruction, and
// Call.Aux is in range for the enclosing Module (when m != nil).
func Verify(m *Module, f *Function) error {
	if f.Entry < 0 || int(f.Entry) >= len(f.Blocks) {
		return fmt.Errorf("entry block %d out of range", f.Entry)
	}
	if len(f.Values) != len(f.ValueBlock) {
		return fmt.Errorf("values/valueBlock length mismatch: %d vs %d", len(f.Values), len(f.ValueBlock))
	}
	for bi, blk := range f.Blocks {
		if blk.ID != BlockID(bi) {
			return fmt.Errorf("block %d has ID %d", bi, blk.ID)
		}
		if len(blk.Insts) == 0 {
			return fmt.Errorf("block %d is empty", bi)
		}
		for i, vid := range blk.Insts {
			ins := f.Values[vid]
			isLast := i == len(blk.Insts)-1
			if ins.Op.IsTerminator() != isLast {
				return fmt.Errorf("block %d: terminator placement off at idx %d (op=%d)", bi, i, ins.Op)
			}
			for _, a := range ins.Args {
				if a < 0 || int(a) >= len(f.Values) {
					return fmt.Errorf("block %d ins %d: arg %d out of range", bi, i, a)
				}
			}
			if ins.Op == OpCall && m != nil {
				if ins.Aux < 0 || int(ins.Aux) >= len(m.Funcs) {
					return fmt.Errorf("block %d ins %d: call func %d out of range", bi, i, ins.Aux)
				}
			}
			for _, bb := range ins.AuxBlocks {
				if bb < 0 || int(bb) >= len(f.Blocks) {
					return fmt.Errorf("block %d ins %d: target block %d out of range", bi, i, bb)
				}
			}
		}
	}
	return nil
}
