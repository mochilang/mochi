package vm

// liveSuccs returns the successor PCs for an instruction.
func liveSuccs(ins Instr, pc, n int) []int {
	switch ins.Op {
	case OpJump:
		return []int{ins.A}
	case OpJumpIfFalse, OpJumpIfTrue:
		return []int{pc + 1, ins.B}
	case OpReturn:
		return nil
	default:
		if pc+1 < n {
			return []int{pc + 1}
		}
		return nil
	}
}

func makeRange(start, count int) []int {
	regs := make([]int, count)
	for i := 0; i < count; i++ {
		regs[i] = start + i
	}
	return regs
}

// uses returns the source registers read by the instruction.
func uses(ins Instr) []int {
	switch ins.Op {
	case OpMove, OpLen, OpStr, OpNot, OpNeg, OpNegInt, OpNegFloat,
		OpIterPrep, OpCount, OpExists, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast:
		return []int{ins.B}
	case OpJSON:
		return []int{ins.A}
	case OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpAppend, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		return []int{ins.B, ins.C}
	case OpIndex:
		return []int{ins.B, ins.C}
	case OpSlice:
		return []int{ins.B, ins.C, ins.D}
	case OpSetIndex:
		return []int{ins.A, ins.B, ins.C}
	case OpMakeList:
		return makeRange(ins.C, ins.B)
	case OpMakeMap:
		return makeRange(ins.C, ins.B*2)
	case OpPrint:
		return []int{ins.A}
	case OpPrint2:
		return []int{ins.A, ins.B}
	case OpPrintN:
		return makeRange(ins.C, ins.B)
	case OpCall2:
		return []int{ins.C, ins.D}
	case OpCall:
		return makeRange(ins.D, ins.C)
	case OpCallV:
		r := makeRange(ins.D, ins.C)
		return append([]int{ins.B}, r...)
	case OpReturn:
		return []int{ins.A}
	case OpLoad:
		return []int{ins.B, ins.C}
	case OpSave:
		return []int{ins.B, ins.C, ins.D}
	case OpEval:
		return []int{ins.B}
	case OpFetch:
		return []int{ins.B, ins.C}
	case OpMakeClosure:
		return makeRange(ins.D, ins.C)
	case OpJumpIfFalse, OpJumpIfTrue, OpExpect:
		return []int{ins.A}
	default:
		return nil
	}
}

// defReg returns the destination register of ins if it writes one.
func defReg(ins Instr) (int, bool) {
	switch ins.Op {
	case OpConst, OpMove, OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpEqual, OpNotEqual, OpLess, OpLessEq, OpIn,
		OpLen, OpIndex, OpSlice, OpMakeList, OpMakeMap,
		OpCall, OpCall2, OpCallV, OpReturn, OpNot,
		OpNow, OpJSON, OpAppend, OpStr, OpInput,
		OpCount, OpExists, OpAvg, OpSum, OpMin, OpMax,
		OpValues, OpCast, OpIterPrep, OpLoad, OpSave,
		OpEval, OpFetch, OpMakeClosure,
		OpAddInt, OpAddFloat, OpSubInt, OpSubFloat,
		OpMulInt, OpMulFloat, OpDivInt, OpDivFloat,
		OpModInt, OpModFloat, OpEqualInt, OpEqualFloat,
		OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpNeg, OpNegInt, OpNegFloat, OpUnionAll, OpUnion,
		OpExcept, OpIntersect, OpSort:
		return ins.A, true
	default:
		return -1, false
	}
}

// sideEffect reports whether ins has side effects beyond producing a value.
func sideEffect(ins Instr) bool {
	switch ins.Op {
	case OpPrint, OpPrint2, OpPrintN, OpSetIndex, OpJump, OpJumpIfFalse,
		OpJumpIfTrue, OpSave, OpLoad, OpEval, OpFetch, OpReturn, OpJSON,
		OpInput, OpNow, OpCall, OpCall2, OpCallV:
		return true
	default:
		return false
	}
}

// regsUsed returns all registers referenced by ins (for max register counting).
func regsUsed(ins Instr) []int {
	regs := uses(ins)
	if r, ok := defReg(ins); ok {
		regs = append(regs, r)
	}
	return regs
}
