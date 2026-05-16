// Package emit lowers compiler2/ir + regalloc results to runtime/vm2
// bytecode per MEP-21 v2. The emitter is purely mechanical: it makes
// no optimization decisions and trusts the IR's type tags to pick the
// matching typed opcode.
package emit

import (
	"fmt"

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
		fn, err := compileFunction(f, ra)
		if err != nil {
			return nil, fmt.Errorf("emit %s: %w", f.Name, err)
		}
		funcs[i] = fn
	}
	return &vm2.Program{Funcs: funcs, Main: m.Main}, nil
}

func compileFunction(f *ir.Function, ra regalloc.Result) (*vm2.Function, error) {
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

	// Per-block scan: identify compare→CondBr pairs we can fuse. Both
	// must live in the same block, the compare must immediately precede
	// the CondBr, and the compare's result must have exactly one use.
	// Suppress the compare emit; emit a fused jump in place of the
	// CondBr. When fallthrough matches one of the targets we also skip
	// the trailing OpJump.
	suppressCmp := make(map[ir.ValueID]bool)
	fuseCondBr := make(map[ir.ValueID]ir.ValueID) // condbr vid -> cmp vid
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
		suppressCmp[cmpVid] = true
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
				emit(vm2.Instr{Op: vm2.OpLoadConstI, A: dst, B: cAdd(vm2.CInt(ins.Aux))})
			case ir.OpConstBool:
				emit(vm2.Instr{Op: vm2.OpLoadConstI, A: dst, B: cAdd(vm2.CBool(ins.Aux != 0))})
			case ir.OpAddI64:
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
				if suppressCmp[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpLessI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLessEqI64:
				if suppressCmp[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpLessEqI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpEqualI64:
				if suppressCmp[vid] {
					break
				}
				emit(vm2.Instr{Op: vm2.OpEqualI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
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
				for i, a := range ins.Args {
					emit(vm2.Instr{Op: vm2.OpMove,
						A: int32(callBase + i),
						B: int32(finalReg[a])})
				}
				emit(vm2.Instr{Op: vm2.OpTailCall,
					A: int32(ins.Aux),
					B: int32(callBase), C: int32(len(ins.Args))})
			case ir.OpRet:
				if len(ins.Args) > 0 {
					emit(vm2.Instr{Op: vm2.OpReturn, A: int32(finalReg[ins.Args[0]])})
				} else {
					emit(vm2.Instr{Op: vm2.OpReturn, A: 0})
				}
			case ir.OpBr:
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
				return nil, fmt.Errorf("emit %s: phi at vid %d not supported in step 7", f.Name, vid)
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

	return &vm2.Function{
		Name:      f.Name,
		NumParams: np,
		NumRegs:   numRegs,
		Code:      code,
		Consts:    consts,
	}, nil
}
