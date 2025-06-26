package vm

// optimizeFunction removes dead register writes from fn.
func optimizeFunction(fn *Function) {
	for {
		changed := false
		changed = removeNoopMoves(fn) || changed
		changed = removeDeadOnce(fn) || changed
		if !changed {
			break
		}
	}
	compactRegisters(fn)
}

func removeDeadOnce(fn *Function) bool {
	live := Live(fn)
	keep := make([]bool, len(fn.Code))
	changed := false
	for i, ins := range fn.Code {
		keep[i] = true
		if r, ok := defReg(ins); ok {
			if !live.Out[i][r] && !sideEffect(ins) {
				keep[i] = false
				changed = true
			}
		}
	}
	if !changed {
		return false
	}
	newCode := make([]Instr, 0, len(fn.Code))
	oldToNew := make([]int, len(fn.Code))
	for i, ins := range fn.Code {
		if keep[i] {
			oldToNew[i] = len(newCode)
			newCode = append(newCode, ins)
		} else {
			oldToNew[i] = -1
		}
	}
	for i := range newCode {
		ins := newCode[i]
		switch ins.Op {
		case OpJump:
			if ins.A >= 0 && ins.A < len(oldToNew) {
				tgt := oldToNew[ins.A]
				if tgt >= 0 {
					ins.A = tgt
				} else {
					ins.A = len(newCode)
				}
			} else if ins.A == len(oldToNew) {
				ins.A = len(newCode)
			}
		case OpJumpIfFalse, OpJumpIfTrue:
			if ins.B >= 0 && ins.B < len(oldToNew) {
				tgt := oldToNew[ins.B]
				if tgt >= 0 {
					ins.B = tgt
				} else {
					ins.B = len(newCode)
				}
			} else if ins.B == len(oldToNew) {
				ins.B = len(newCode)
			}
		}
		newCode[i] = ins
	}
	fn.Code = newCode
	return true
}

// optimizeProgram applies optimizations to all functions in p.
func optimizeProgram(p *Program) {
	for i := range p.Funcs {
		optimizeFunction(&p.Funcs[i])
	}
}

// compactRegisters rewrites fn so that only used registers remain and are
// renumbered densely starting from zero. This preserves register order so
// contiguous ranges remain valid for call arguments and other ops.
func compactRegisters(fn *Function) {
	used := make([]bool, fn.NumRegs)
	for _, ins := range fn.Code {
		for _, r := range regsUsed(ins) {
			if r >= 0 && r < len(used) {
				used[r] = true
			}
		}
	}

	mapping := make([]int, fn.NumRegs)
	next := 0
	for r, u := range used {
		if u {
			mapping[r] = next
			next++
		} else {
			mapping[r] = -1
		}
	}

	for i, ins := range fn.Code {
		fn.Code[i] = remapInstr(ins, mapping)
	}
	fn.NumRegs = next
}

func mapReg(mapping []int, r int) int {
	if r >= 0 && r < len(mapping) && mapping[r] >= 0 {
		return mapping[r]
	}
	return r
}

// remapInstr applies a register mapping to ins and returns the updated instr.
func remapInstr(ins Instr, m []int) Instr {
	switch ins.Op {
	case OpConst:
		ins.A = mapReg(m, ins.A)
	case OpMove, OpLen, OpStr, OpNot, OpNeg, OpNegInt, OpNegFloat,
		OpCount, OpExists, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
	case OpJSON:
		ins.A = mapReg(m, ins.A)
	case OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpAppend, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
	case OpIndex:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
	case OpSlice:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
		ins.D = mapReg(m, ins.D)
	case OpSetIndex:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
	case OpMakeList:
		ins.A = mapReg(m, ins.A)
		ins.C = mapReg(m, ins.C)
	case OpMakeMap:
		ins.A = mapReg(m, ins.A)
		ins.C = mapReg(m, ins.C)
	case OpPrint:
		ins.A = mapReg(m, ins.A)
	case OpPrint2:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
	case OpPrintN:
		ins.A = mapReg(m, ins.A)
		ins.C = mapReg(m, ins.C)
	case OpCall2:
		ins.A = mapReg(m, ins.A)
		ins.C = mapReg(m, ins.C)
		ins.D = mapReg(m, ins.D)
	case OpCall:
		ins.A = mapReg(m, ins.A)
		ins.D = mapReg(m, ins.D)
	case OpCallV:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.D = mapReg(m, ins.D)
	case OpReturn:
		ins.A = mapReg(m, ins.A)
	case OpJumpIfFalse, OpJumpIfTrue, OpExpect:
		ins.A = mapReg(m, ins.A)
	case OpLoad:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
	case OpSave:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
		ins.D = mapReg(m, ins.D)
	case OpEval:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
	case OpFetch:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
		ins.C = mapReg(m, ins.C)
	case OpMakeClosure:
		ins.A = mapReg(m, ins.A)
		ins.D = mapReg(m, ins.D)
	case OpIterPrep:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
	case OpSort:
		ins.A = mapReg(m, ins.A)
		ins.B = mapReg(m, ins.B)
	case OpNow, OpJump:
		// no registers to rewrite
	}
	return ins
}

// removeNoopMoves drops OpMove instructions that copy a register to itself.
func removeNoopMoves(fn *Function) bool {
	changed := false
	out := fn.Code[:0]
	for _, ins := range fn.Code {
		if ins.Op == OpMove && ins.A == ins.B {
			changed = true
			continue
		}
		out = append(out, ins)
	}
	if changed {
		fn.Code = append([]Instr(nil), out...)
	}
	return changed
}
