// Package emit lowers compiler2/ir + regalloc results to runtime/vm2
// bytecode per MEP-21 v2. The emitter is purely mechanical: it makes
// no optimization decisions and trusts the IR's type tags to pick the
// matching typed opcode.
package emit

import (
	"fmt"
	"math"

	"mochi/compiler2/ir"
	"mochi/compiler2/regalloc"
	vm2 "mochi/runtime/vm2"
)

// Compile lowers an ir.Module to a vm2.Program.
func Compile(m *ir.Module) (*vm2.Program, error) {
	funcs := make([]*vm2.Function, len(m.Funcs))
	for i, f := range m.Funcs {
		if err := ir.Verify(m, f); err != nil {
			return nil, fmt.Errorf("ir.Verify %s: %w", f.Name, err)
		}
		ra := regalloc.Run(f, nil)
		fn, err := compileFunction(f, ra, i)
		if err != nil {
			return nil, fmt.Errorf("emit %s: %w", f.Name, err)
		}
		funcs[i] = fn
	}
	return &vm2.Program{Funcs: funcs, Main: m.Main}, nil
}

func compileFunction(f *ir.Function, ra regalloc.Result, selfIdx int) (*vm2.Function, error) {
	np := len(f.Params)

	// Pin params to regs [0..np). Shift all non-param regalloc
	// assignments up by np so they live in [np..np+ra.NumRegs). This
	// gives the OpCall convention (args in regs [0..n)) a fixed home
	// at function entry without parallel-copy gymnastics.
	finalReg := make([]int, len(f.Values))
	for vid, ins := range f.Values {
		if ins.Op == ir.OpParam {
			finalReg[vid] = int(ins.Aux)
		} else if ra.Reg[vid] < 0 {
			finalReg[vid] = -1
		} else {
			finalReg[vid] = ra.Reg[vid] + np
		}
	}

	// Determine max args across all calls; reserve a contiguous arg-
	// staging block above the assigned range.
	maxArgs := 0
	for _, ins := range f.Values {
		switch ins.Op {
		case ir.OpCall, ir.OpTailCall:
			if len(ins.Args) > maxArgs {
				maxArgs = len(ins.Args)
			}
		}
	}
	callBase := np + ra.NumRegs
	numRegs := callBase + maxArgs
	if numRegs < np {
		numRegs = np
	}

	// Use counts for fusion safety: a compare can be folded into the
	// following OpCondBr only if it has exactly one use (that CondBr).
	useCount := make([]int, len(f.Values))
	for _, ins := range f.Values {
		for _, a := range ins.Args {
			useCount[a]++
		}
	}

	// Program-point lookup: walking f.Values in ValueID order is the
	// same linearization regalloc uses by default (one program point
	// per non-DCE'd value, in block-ID order). We track the running
	// position so each emit site can ask "is this operand's read the
	// value's last use?" by comparing pos against ra.LastUse[opVid].
	// pos[vid] >= 0 means defined; -1 means unreached / DCE'd.
	pos := make([]int, len(f.Values))
	for i := range pos {
		pos[i] = -1
	}
	{
		p := 0
		for _, blk := range f.Blocks {
			for _, vid := range blk.Insts {
				pos[vid] = p
				p++
			}
		}
	}
	isLastUseOf := func(useVid, operandVid ir.ValueID) bool {
		// Operand never used after this point if useVid's position is
		// at least the operand's LastUse. We use >= because an unread
		// value has LastUse == defPos (no extension); in that case the
		// first reader is by definition the last.
		if ra.LastUse == nil {
			return false
		}
		up := pos[useVid]
		lu := ra.LastUse[operandVid]
		return up >= 0 && lu >= 0 && up >= lu
	}

	// Per-block scan: identify compare→CondBr pairs we can fuse. Both
	// must live in the same block, the compare must immediately precede
	// the CondBr, and the compare's result must have exactly one use.
	// Suppress the compare emit; emit a fused jump in place of the
	// CondBr. When fallthrough matches one of the targets we also skip
	// the trailing OpJump.
	// suppress is the set of value IDs whose emit should be skipped
	// because some later instruction fuses them in directly (compare
	// folded into a branch, const folded into an add immediate, etc).
	suppress := make(map[ir.ValueID]bool)
	fuseCondBr := make(map[ir.ValueID]ir.ValueID) // condbr vid -> cmp vid
	// addK records an OpAddI64 fused with a single-use ConstI64
	// operand: the constant value k and the value ID of the non-const
	// addend. Emit lowers these to OpAddI64K instead of two separate
	// dispatches, which cuts ~14% off the hot loop step in iter_sum.
	type addKInfo struct {
		k           int64
		nonConstArg ir.ValueID
	}
	addK := make(map[ir.ValueID]addKInfo)
	for vid, ins := range f.Values {
		if ins.Op != ir.OpAddI64 {
			continue
		}
		a0, a1 := ins.Args[0], ins.Args[1]
		try := func(constArg, otherArg ir.ValueID) bool {
			c := f.Values[constArg]
			if c.Op != ir.OpConstI64 || useCount[constArg] != 1 {
				return false
			}
			k := c.Aux
			if k < -(1<<31) || k > (1<<31)-1 {
				return false
			}
			suppress[constArg] = true
			addK[ir.ValueID(vid)] = addKInfo{k: k, nonConstArg: otherArg}
			return true
		}
		if !try(a1, a0) {
			try(a0, a1)
		}
	}
	for bi := range f.Blocks {
		blk := f.Blocks[bi]
		insts := blk.Insts
		if len(insts) < 2 {
			continue
		}
		last := insts[len(insts)-1]
		lastIns := f.Values[last]
		if lastIns.Op != ir.OpCondBr {
			continue
		}
		cmpVid := lastIns.Args[0]
		// Find the cmp's position: must be the immediately preceding
		// instruction in this block that isn't OpInvalid (DCE'd).
		var prev ir.ValueID = -1
		for i := len(insts) - 2; i >= 0; i-- {
			if f.Values[insts[i]].Op != ir.OpInvalid {
				prev = insts[i]
				break
			}
		}
		if prev != cmpVid {
			continue
		}
		cmpIns := f.Values[cmpVid]
		switch cmpIns.Op {
		case ir.OpLessI64, ir.OpLessEqI64, ir.OpEqualI64:
		default:
			continue
		}
		if useCount[cmpVid] != 1 {
			continue
		}
		suppress[cmpVid] = true
		fuseCondBr[last] = cmpVid
	}

	// Const pool with dedup.
	consts := []vm2.Cell{}
	constIdx := map[vm2.Cell]int32{}
	cAdd := func(c vm2.Cell) int32 {
		if i, ok := constIdx[c]; ok {
			return i
		}
		i := int32(len(consts))
		consts = append(consts, c)
		constIdx[c] = i
		return i
	}

	// Emit per block in block-ID order. Record blockPC[bid] = pc at
	// block start; patch jump targets in a second pass.
	var code []vm2.Instr
	blockPC := make([]int32, len(f.Blocks))
	type pendingJump struct {
		insIdx int
		target ir.BlockID
		field  byte // 'A' or 'B'
	}
	var pending []pendingJump
	emit := func(ins vm2.Instr) int {
		code = append(code, ins)
		return len(code) - 1
	}

	// MEP-38 §3.3 leaf-inline phi resolution: lower each ir.OpPhi at the
	// head of a block to OpMove instructions emitted at the end of each
	// predecessor, immediately before the predecessor's terminator. The
	// leaf inliner only produces phis whose predecessors terminate in
	// OpBr (a single successor), so every move scheduled here lives on a
	// non-critical edge; the assumption is enforced via a runtime check
	// the first time a phi with a non-OpBr predecessor would be lowered.
	phiMovesByPred := make(map[ir.BlockID][]pmove)
	hasPhi := false
	for _, blk := range f.Blocks {
		for _, vid := range blk.Insts {
			ins := f.Values[vid]
			if ins.Op != ir.OpPhi {
				continue
			}
			hasPhi = true
			if finalReg[vid] < 0 {
				continue
			}
			dstReg := int32(finalReg[vid])
			for i, predID := range ins.AuxBlocks {
				srcVid := ins.Args[i]
				if finalReg[srcVid] < 0 {
					continue
				}
				srcReg := int32(finalReg[srcVid])
				phiMovesByPred[predID] = append(phiMovesByPred[predID],
					pmove{dst: dstReg, src: srcReg})
			}
		}
	}
	// Critical-edge check: if any phi-target block has a predecessor that
	// terminates in OpCondBr (two successors), the moves can't be safely
	// hoisted ahead of the branch without edge splitting. Reject loudly
	// so the caller knows to split critical edges before emit.
	if hasPhi {
		for _, blk := range f.Blocks {
			if len(phiMovesByPred[blk.ID]) == 0 {
				continue
			}
			term := f.Values[blk.Insts[len(blk.Insts)-1]]
			if term.Op != ir.OpBr {
				return nil, fmt.Errorf("emit %s: phi predecessor block %d terminates in %d, only OpBr predecessors are supported (split critical edges first)", f.Name, blk.ID, term.Op)
			}
		}
	}
	flushPhiMoves := func(predID ir.BlockID) {
		moves := phiMovesByPred[predID]
		if len(moves) == 0 {
			return
		}
		emitParallelMoves(moves, int32(callBase), emit)
	}

	for bi := range f.Blocks {
		blockPC[bi] = int32(len(code))
		blk := f.Blocks[bi]
		for _, vid := range blk.Insts {
			ins := f.Values[vid]
			dst := int32(finalReg[vid])
			switch ins.Op {
			case ir.OpParam:
				// Already in canonical reg; nothing to emit.
			case ir.OpConstI64:
				if suppress[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpLoadConstI, A: dst, B: cAdd(vm2.CInt(ins.Aux))})
			case ir.OpConstBool:
				emit(vm2.Instr{Op: vm2.OpLoadConstI, A: dst, B: cAdd(vm2.CBool(ins.Aux != 0))})
			case ir.OpAddI64:
				if info, ok := addK[vid]; ok {
					emit(vm2.Instr{Op: vm2.OpAddI64K, A: dst,
						B: int32(finalReg[info.nonConstArg]),
						C: int32(info.k)})
					break
				}
				emit(vm2.Instr{Op: vm2.OpAddI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpSubI64:
				emit(vm2.Instr{Op: vm2.OpSubI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpMulI64:
				emit(vm2.Instr{Op: vm2.OpMulI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpDivI64:
				emit(vm2.Instr{Op: vm2.OpDivI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpModI64:
				emit(vm2.Instr{Op: vm2.OpModI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLessI64:
				if suppress[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpLessI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLessEqI64:
				if suppress[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpLessEqI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpEqualI64:
				if suppress[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpEqualI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpConstStr:
				// Aux indexes into f.Strings; copy bytes verbatim into
				// the vm2 string-const pool. The pool is forwarded onto
				// the emitted vm2.Function below, no dedup beyond what
				// the builder already did per function.
				emit(vm2.Instr{Op: vm2.OpLoadStrK, A: dst, B: int32(ins.Aux)})
			case ir.OpConcatStr:
				emit(vm2.Instr{Op: vm2.OpConcatStr, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLenStr:
				emit(vm2.Instr{Op: vm2.OpLenStr, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpIndexStr:
				emit(vm2.Instr{Op: vm2.OpIndexStr, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpEqualStr:
				emit(vm2.Instr{Op: vm2.OpEqualStr, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpHashStr:
				emit(vm2.Instr{Op: vm2.OpHashStr, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpNewList:
				emit(vm2.Instr{Op: vm2.OpNewList, A: dst, B: int32(ins.Aux)})
			case ir.OpListLen:
				emit(vm2.Instr{Op: vm2.OpListLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpListGet:
				emit(vm2.Instr{Op: vm2.OpListGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpListSet:
				emit(vm2.Instr{Op: vm2.OpListSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpListPush:
				emit(vm2.Instr{Op: vm2.OpListPush,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]])})
			case ir.OpListAppend:
				// MEP-36 Phase 3c §3.5: tag the source-list operand
				// (B) as last-use whenever the IR confirms no later
				// instruction reads it. The dispatcher then mutates
				// the source instead of copying. emit doesn't need
				// to prove uniqueness of the underlying *vmList; the
				// builder contract is that ListAppend reads its first
				// arg, so if regalloc says no later read exists, no
				// aliased caller can observe the in-place mutation.
				flags := uint8(0)
				if isLastUseOf(vid, ins.Args[0]) {
					flags |= vm2.InstrFlagBLastUse
				}
				emit(vm2.Instr{Op: vm2.OpListAppend, Flags: flags,
					A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpNewMap:
				emit(vm2.Instr{Op: vm2.OpNewMap, A: dst})
			case ir.OpMapLen:
				emit(vm2.Instr{Op: vm2.OpMapLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpMapGet:
				emit(vm2.Instr{Op: vm2.OpMapGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpMapHas:
				emit(vm2.Instr{Op: vm2.OpMapHas, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpMapSet:
				emit(vm2.Instr{Op: vm2.OpMapSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpMapDel:
				emit(vm2.Instr{Op: vm2.OpMapDel,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]])})
			case ir.OpConstF64:
				f := math.Float64frombits(uint64(ins.Aux))
				emit(vm2.Instr{Op: vm2.OpLoadConstF, A: dst, B: cAdd(vm2.CFloat(f))})
			case ir.OpAddF64:
				emit(vm2.Instr{Op: vm2.OpAddF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpSubF64:
				emit(vm2.Instr{Op: vm2.OpSubF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpMulF64:
				emit(vm2.Instr{Op: vm2.OpMulF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpDivF64:
				emit(vm2.Instr{Op: vm2.OpDivF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpNegF64:
				emit(vm2.Instr{Op: vm2.OpNegF64, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpAbsF64:
				emit(vm2.Instr{Op: vm2.OpAbsF64, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpSqrtF64:
				emit(vm2.Instr{Op: vm2.OpSqrtF64, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpLessF64:
				emit(vm2.Instr{Op: vm2.OpLessF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLessEqF64:
				emit(vm2.Instr{Op: vm2.OpLessEqF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpEqualF64:
				emit(vm2.Instr{Op: vm2.OpEqualF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpFmaF64:
				emit(vm2.Instr{Op: vm2.OpFmaF64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]]),
					D: int32(finalReg[ins.Args[2]])})
			case ir.OpI64ToF64:
				emit(vm2.Instr{Op: vm2.OpI64ToF64, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpF64ToI64:
				emit(vm2.Instr{Op: vm2.OpF64ToI64, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpNewF64Array:
				emit(vm2.Instr{Op: vm2.OpNewF64Array, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpF64ArrLen:
				emit(vm2.Instr{Op: vm2.OpF64ArrLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpF64ArrGet:
				emit(vm2.Instr{Op: vm2.OpF64ArrGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpF64ArrSet:
				emit(vm2.Instr{Op: vm2.OpF64ArrSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpNewI64Array:
				emit(vm2.Instr{Op: vm2.OpNewI64Array, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpI64ArrLen:
				emit(vm2.Instr{Op: vm2.OpI64ArrLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpI64ArrGet:
				emit(vm2.Instr{Op: vm2.OpI64ArrGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpI64ArrSet:
				emit(vm2.Instr{Op: vm2.OpI64ArrSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpNewU8Array:
				emit(vm2.Instr{Op: vm2.OpNewU8Array, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpU8ArrLen:
				emit(vm2.Instr{Op: vm2.OpU8ArrLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpU8ArrGet:
				emit(vm2.Instr{Op: vm2.OpU8ArrGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpU8ArrSet:
				emit(vm2.Instr{Op: vm2.OpU8ArrSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpNewPair:
				emit(vm2.Instr{Op: vm2.OpNewPair, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpPairFst:
				emit(vm2.Instr{Op: vm2.OpPairFst, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpPairSnd:
				emit(vm2.Instr{Op: vm2.OpPairSnd, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpBytesNew:
				emit(vm2.Instr{Op: vm2.OpBytesNew, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpBytesLen:
				emit(vm2.Instr{Op: vm2.OpBytesLen, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpBytesGet:
				emit(vm2.Instr{Op: vm2.OpBytesGet, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpBytesSet:
				emit(vm2.Instr{Op: vm2.OpBytesSet,
					A: int32(finalReg[ins.Args[0]]),
					B: int32(finalReg[ins.Args[1]]),
					C: int32(finalReg[ins.Args[2]])})
			case ir.OpBytesSlice:
				emit(vm2.Instr{Op: vm2.OpBytesSlice, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]]),
					D: int32(finalReg[ins.Args[2]])})
			case ir.OpBytesEqual:
				emit(vm2.Instr{Op: vm2.OpBytesEqual, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpBytesHash:
				emit(vm2.Instr{Op: vm2.OpBytesHash, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpBytesFromU8Array:
				emit(vm2.Instr{Op: vm2.OpBytesFromU8Array, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpBytesFromStr:
				emit(vm2.Instr{Op: vm2.OpBytesFromStr, A: dst,
					B: int32(finalReg[ins.Args[0]])})
			case ir.OpStdoutWriteBytes:
				emit(vm2.Instr{Op: vm2.OpStdoutWriteBytes,
					A: int32(finalReg[ins.Args[0]])})
			case ir.OpStdinReadAll:
				emit(vm2.Instr{Op: vm2.OpStdinReadAll, A: dst})
			case ir.OpCall:
				for i, a := range ins.Args {
					emit(vm2.Instr{Op: vm2.OpMove,
						A: int32(callBase + i),
						B: int32(finalReg[a])})
				}
				emit(vm2.Instr{Op: vm2.OpCall, A: dst,
					B: int32(ins.Aux),
					C: int32(callBase), D: int32(len(ins.Args))})
			case ir.OpTailCall:
				if int(ins.Aux) == selfIdx {
					// Same-function tail call. Move args directly into
					// the param slots [0..n) via a parallel-move
					// schedule, then dispatch OpTailCallSelf which only
					// rewinds IP. This kills the staging copy + the
					// dispatch-side memmove that show up in profiles of
					// loop-heavy programs (every `for` lowers to one of
					// these). callBase is unused on this path and
					// serves as the cycle-breaking temp register.
					moves := make([]pmove, 0, len(ins.Args))
					for i, a := range ins.Args {
						moves = append(moves, pmove{dst: int32(i), src: int32(finalReg[a])})
					}
					emitParallelMoves(moves, int32(callBase), emit)
					emit(vm2.Instr{Op: vm2.OpTailCallSelf})
				} else {
					for i, a := range ins.Args {
						emit(vm2.Instr{Op: vm2.OpMove,
							A: int32(callBase + i),
							B: int32(finalReg[a])})
					}
					emit(vm2.Instr{Op: vm2.OpTailCall,
						A: int32(ins.Aux),
						B: int32(callBase), C: int32(len(ins.Args))})
				}
			case ir.OpRet:
				if len(ins.Args) > 0 {
					emit(vm2.Instr{Op: vm2.OpReturn, A: int32(finalReg[ins.Args[0]])})
				} else {
					emit(vm2.Instr{Op: vm2.OpReturn, A: 0})
				}
			case ir.OpBr:
				flushPhiMoves(blk.ID)
				idx := emit(vm2.Instr{Op: vm2.OpJump})
				pending = append(pending, pendingJump{insIdx: idx, target: ins.AuxBlocks[0], field: 'A'})
			case ir.OpCondBr:
				thenB := ins.AuxBlocks[0]
				elseB := ins.AuxBlocks[1]
				var nextB ir.BlockID = -1
				if bi+1 < len(f.Blocks) {
					nextB = f.Blocks[bi+1].ID
				}
				if cmpVid, ok := fuseCondBr[vid]; ok {
					cmp := f.Values[cmpVid]
					b := int32(finalReg[cmp.Args[0]])
					c := int32(finalReg[cmp.Args[1]])
					// Direct (jump-if-true) and inverted forms per cmp op.
					// Picking inverted means we jump to elseB on the
					// negated predicate; that's only valid when thenB is
					// the fallthrough, so we can drop the trailing jump.
					var direct, inverted vm2.Op
					switch cmp.Op {
					case ir.OpLessI64:
						direct, inverted = vm2.OpJumpIfLessI64, vm2.OpJumpIfGreaterEqI64
					case ir.OpLessEqI64:
						direct, inverted = vm2.OpJumpIfLessEqI64, vm2.OpJumpIfGreaterI64
					case ir.OpEqualI64:
						direct, inverted = vm2.OpJumpIfEqualI64, vm2.OpJumpIfNotEqualI64
					}
					switch {
					case elseB == nextB:
						// Jump on true to thenB, fall through to elseB.
						idx := emit(vm2.Instr{Op: direct, A: b, B: c})
						pending = append(pending, pendingJump{insIdx: idx, target: thenB, field: 'C'})
					case thenB == nextB:
						// Jump on !cond to elseB, fall through to thenB.
						idx := emit(vm2.Instr{Op: inverted, A: b, B: c})
						pending = append(pending, pendingJump{insIdx: idx, target: elseB, field: 'C'})
					default:
						idx := emit(vm2.Instr{Op: direct, A: b, B: c})
						pending = append(pending, pendingJump{insIdx: idx, target: thenB, field: 'C'})
						idx2 := emit(vm2.Instr{Op: vm2.OpJump})
						pending = append(pending, pendingJump{insIdx: idx2, target: elseB, field: 'A'})
					}
					break
				}
				// Unfused fallback: jump on false to elseB; fall
				// through on true. Drop the trailing OpJump when thenB
				// is the layout-next block.
				idx := emit(vm2.Instr{Op: vm2.OpJumpIfFalse, A: int32(finalReg[ins.Args[0]])})
				pending = append(pending, pendingJump{insIdx: idx, target: elseB, field: 'B'})
				if thenB != nextB {
					idx2 := emit(vm2.Instr{Op: vm2.OpJump})
					pending = append(pending, pendingJump{insIdx: idx2, target: thenB, field: 'A'})
				}
			case ir.OpInvalid:
				// DCE'd
			case ir.OpPhi:
				// Phi nodes lower to OpMove instructions emitted at the
				// end of each predecessor block, not at the phi's own
				// site. The pre-pass above already populated
				// phiMovesByPred; nothing to emit here.
			default:
				return nil, fmt.Errorf("emit %s: unsupported op %d at vid %d", f.Name, ins.Op, vid)
			}
		}
	}

	for _, pj := range pending {
		target := blockPC[pj.target]
		switch pj.field {
		case 'A':
			code[pj.insIdx].A = target
		case 'B':
			code[pj.insIdx].B = target
		case 'C':
			code[pj.insIdx].C = target
		}
	}

	strConsts := make([][]byte, len(f.Strings))
	for i, s := range f.Strings {
		strConsts[i] = []byte(s)
	}

	// MEP-36 Phase 3: a register window holds a container only if some
	// IR value of container type (TStr/TList/TMap/TPtr) was assigned a
	// real register. Pure int/bool functions get HasContainerSlots=false
	// and popFrame elides the clear sweep.
	hasContainer := false
	for vid, ins := range f.Values {
		if finalReg[vid] < 0 {
			continue
		}
		switch ins.Type {
		case ir.TStr, ir.TList, ir.TMap, ir.TPtr,
			ir.TF64Array, ir.TI64Array, ir.TU8Array, ir.TPair, ir.TBytes:
			hasContainer = true
		}
		if hasContainer {
			break
		}
	}

	return &vm2.Function{
		Name:              f.Name,
		NumParams:         np,
		NumRegs:           numRegs,
		Code:              code,
		Consts:            consts,
		StrConsts:         strConsts,
		HasContainerSlots: hasContainer,
	}, nil
}

// pmove is one entry in a parallel-move schedule: regs[dst] <- regs[src].
type pmove struct{ dst, src int32 }

// emitParallelMoves lowers a parallel-move set into a serial OpMove
// sequence that preserves the all-at-once semantics. Trivial src==dst
// pairs are dropped. The graph is emitted in reverse topological order
// (a move whose dst is not anyone else's src is safe to emit). Cycles
// are broken by spilling one src to tempReg and rewriting every
// in-edge that read it. tempReg must be a register slot not otherwise
// live across the move sequence.
func emitParallelMoves(moves []pmove, tempReg int32, emit func(vm2.Instr) int) {
	out := moves[:0]
	for _, m := range moves {
		if m.src != m.dst {
			out = append(out, m)
		}
	}
	moves = out
	for len(moves) > 0 {
		progressed := false
		for i := 0; i < len(moves); i++ {
			d := moves[i].dst
			isSrc := false
			for j := range moves {
				if j != i && moves[j].src == d {
					isSrc = true
					break
				}
			}
			if !isSrc {
				emit(vm2.Instr{Op: vm2.OpMove, A: d, B: moves[i].src})
				moves = append(moves[:i], moves[i+1:]...)
				progressed = true
				break
			}
		}
		if progressed {
			continue
		}
		// Only cycles remain. Spill the first move's src to tempReg and
		// rewrite every consumer of that src so they read tempReg
		// instead. After this, at least one move's dst is no longer
		// referenced as a src and the loop makes progress.
		spillSrc := moves[0].src
		emit(vm2.Instr{Op: vm2.OpMove, A: tempReg, B: spillSrc})
		for i := range moves {
			if moves[i].src == spillSrc {
				moves[i].src = tempReg
			}
		}
	}
}
