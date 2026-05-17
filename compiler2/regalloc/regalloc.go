// Package regalloc assigns physical register indices to SSA values
// using linear-scan allocation (Poletto & Sarkar, 1999).
//
// Scope for step 6: straight-line code and acyclic CFGs. Loops with
// values live across back-edges are correctly handled only when the
// front end has lowered loops to phis at headers and the block order
// passed in is a reverse-postorder. The current consumer (fib + the
// step 8 corpus) has no loops, so block-ID order is sufficient.
package regalloc

import (
	"sort"

	"mochi/compiler2/ir"
)

// Result records register assignments for one Function.
type Result struct {
	// Reg[valueID] is the physical register index assigned to that
	// SSA value. -1 means the value is unused (DCE missed it).
	Reg []int
	// NumRegs is the total count of physical registers used (max
	// reg index + 1, or 0 if no live values).
	NumRegs int
	// Pos[valueID] is the linearized program point assigned to each
	// SSA value's definition. -1 for DCE'd values. The emitter uses
	// this together with LastUse to decide whether a given operand
	// read is the value's last use within the function.
	Pos []int
	// LastUse[valueID] is the program point of the last instruction
	// that reads the value, or Pos[valueID] when the value is
	// defined but never read (no extension). The emitter consults
	// this when setting MEP-36 Phase 3c operand-last-use bits on
	// container ops: if the position of an op consuming valueID v
	// equals LastUse[v], that read is the final read.
	LastUse []int
}

// Run computes register assignments for fn. blockOrder controls the
// linearization; pass nil to use the natural block-ID order.
func Run(fn *ir.Function, blockOrder []ir.BlockID) Result {
	if blockOrder == nil {
		blockOrder = make([]ir.BlockID, len(fn.Blocks))
		for i := range blockOrder {
			blockOrder[i] = ir.BlockID(i)
		}
	}

	// Linearize: assign each instruction a monotonically increasing
	// position (program point).
	pos := make([]int, len(fn.Values))
	for i := range pos {
		pos[i] = -1
	}
	p := 0
	for _, bid := range blockOrder {
		for _, vid := range fn.Blocks[bid].Insts {
			pos[vid] = p
			p++
		}
	}

	// Compute live intervals: [defPos, lastUsePos].
	type interval struct {
		vid        ir.ValueID
		start, end int
	}
	intervals := make([]interval, 0, len(fn.Values))
	for vid, ins := range fn.Values {
		dp := pos[vid]
		if dp < 0 {
			continue // unreached / DCE'd
		}
		end := dp
		_ = ins
		intervals = append(intervals, interval{vid: ir.ValueID(vid), start: dp, end: end})
	}
	idxByVID := make(map[ir.ValueID]int, len(intervals))
	for i, iv := range intervals {
		idxByVID[iv.vid] = i
	}
	// Extend interval ends based on actual uses (a value lives at
	// least until the last instruction that reads it).
	for vid, ins := range fn.Values {
		dp := pos[vid]
		if dp < 0 {
			continue
		}
		for _, a := range ins.Args {
			ai, ok := idxByVID[a]
			if !ok {
				continue
			}
			if dp > intervals[ai].end {
				intervals[ai].end = dp
			}
		}
	}

	// Snapshot last-use positions before we re-sort intervals; the
	// emitter needs this map keyed by ValueID. extension above already
	// pushed each entry's end to the max read position, so intervals[i].end
	// is precisely the last-read program point (or the def position when
	// the value is unread).
	lastUse := make([]int, len(fn.Values))
	for i := range lastUse {
		lastUse[i] = -1
	}
	for _, iv := range intervals {
		lastUse[iv.vid] = iv.end
	}

	sort.Slice(intervals, func(i, j int) bool {
		return intervals[i].start < intervals[j].start
	})

	reg := make([]int, len(fn.Values))
	for i := range reg {
		reg[i] = -1
	}

	// Active intervals sorted by end (we keep a slice and re-sort on
	// insert; corpus size is small).
	type active struct {
		end int
		reg int
		vid ir.ValueID
	}
	var act []active
	// freeRegs is a min-heap-like slice of available reg indices.
	var freeRegs []int
	maxReg := -1

	for _, iv := range intervals {
		// Expire.
		out := act[:0]
		for _, a := range act {
			if a.end < iv.start {
				freeRegs = append(freeRegs, a.reg)
			} else {
				out = append(out, a)
			}
		}
		act = out
		// Allocate.
		var r int
		if n := len(freeRegs); n > 0 {
			// Use lowest free reg for tighter packing.
			sort.Ints(freeRegs)
			r = freeRegs[0]
			freeRegs = freeRegs[1:]
		} else {
			maxReg++
			r = maxReg
		}
		reg[iv.vid] = r
		act = append(act, active{end: iv.end, reg: r, vid: iv.vid})
	}

	return Result{Reg: reg, NumRegs: maxReg + 1, Pos: pos, LastUse: lastUse}
}
