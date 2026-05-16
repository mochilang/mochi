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
			case ir.OpLessI64:
				emit(vm2.Instr{Op: vm2.OpLessI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpLessEqI64:
				emit(vm2.Instr{Op: vm2.OpLessEqI64, A: dst,
					B: int32(finalReg[ins.Args[0]]),
					C: int32(finalReg[ins.Args[1]])})
			case ir.OpEqualI64:
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
				idx := emit(vm2.Instr{Op: vm2.OpJumpIfFalse, A: int32(finalReg[ins.Args[0]])})
				pending = append(pending, pendingJump{insIdx: idx, target: ins.AuxBlocks[1], field: 'B'})
				idx2 := emit(vm2.Instr{Op: vm2.OpJump})
				pending = append(pending, pendingJump{insIdx: idx2, target: ins.AuxBlocks[0], field: 'A'})
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
		if pj.field == 'A' {
			code[pj.insIdx].A = target
		} else {
			code[pj.insIdx].B = target
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
