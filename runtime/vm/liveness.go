package vm

// LiveInfo stores liveness sets for each instruction.
type LiveInfo struct {
	In  [][]bool
	Out [][]bool
}

// Liveness performs a backward dataflow analysis computing live registers at
// entry and exit of each instruction.
func Liveness(fn *Function) *LiveInfo {
	n := len(fn.Code)
	in := make([][]bool, n)
	out := make([][]bool, n)
	for i := 0; i < n; i++ {
		in[i] = make([]bool, fn.NumRegs)
		out[i] = make([]bool, fn.NumRegs)
	}
	changed := true
	for changed {
		changed = false
		for pc := n - 1; pc >= 0; pc-- {
			// compute out set
			outSet := make([]bool, fn.NumRegs)
			for _, succ := range successors(fn.Code[pc], pc, n) {
				if succ < 0 || succ >= n {
					continue
				}
				orSet(outSet, in[succ])
			}
			use, def := useDef(fn.Code[pc], fn.NumRegs)
			inSet := make([]bool, fn.NumRegs)
			for r := 0; r < fn.NumRegs; r++ {
				inSet[r] = use[r] || (outSet[r] && !def[r])
			}
			if !equalSet(outSet, out[pc]) || !equalSet(inSet, in[pc]) {
				out[pc] = outSet
				in[pc] = inSet
				changed = true
			}
		}
	}
	return &LiveInfo{In: in, Out: out}
}

func orSet(dst, src []bool) {
	for i, v := range src {
		if v {
			dst[i] = true
		}
	}
}

func equalSet(a, b []bool) bool {
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// useDef returns the used and defined registers for an instruction.
func useDef(ins Instr, n int) (use, def []bool) {
	use = make([]bool, n)
	def = make([]bool, n)
	addUse := func(r int) {
		if r >= 0 && r < n {
			use[r] = true
		}
	}
	addDef := func(r int) {
		if r >= 0 && r < n {
			def[r] = true
		}
	}
	switch ins.Op {
	case OpConst:
		addDef(ins.A)
	case OpMove:
		addDef(ins.A)
		addUse(ins.B)
	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpFirst, OpExists,
		OpLen, OpCount, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast, OpIterPrep, OpNow, OpMem:
		addDef(ins.A)
		addUse(ins.B)
	case OpAppend:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpIndex:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpSlice:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	case OpSetIndex:
		addUse(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpMakeList:
		addDef(ins.A)
		for i := 0; i < ins.B; i++ {
			addUse(ins.C + i)
		}
	case OpMakeMap:
		addDef(ins.A)
		for i := 0; i < ins.B*2; i++ {
			addUse(ins.C + i)
		}
	case OpCall2:
		addDef(ins.A)
		addUse(ins.C)
		addUse(ins.D)
	case OpCall:
		addDef(ins.A)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpCallV:
		addDef(ins.A)
		addUse(ins.B)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpMakeClosure:
		addDef(ins.A)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpLoad:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpSave:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	case OpEval:
		addDef(ins.A)
		addUse(ins.B)
	case OpFetch:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpReturn:
		addUse(ins.A)
	case OpJumpIfFalse, OpJumpIfTrue, OpExpect:
		addUse(ins.A)
	case OpPrint, OpPrint2, OpJSON:
		addUse(ins.A)
		addUse(ins.B)
	case OpPrintN:
		for i := 0; i < ins.B; i++ {
			addUse(ins.C + i)
		}
	default:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	}
	return
}

func defRegs(ins Instr) []int {
	switch ins.Op {
	case OpConst, OpMove, OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpFirst, OpExists,
		OpLen, OpIndex, OpSlice, OpMakeList, OpMakeMap,
		OpCount, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast, OpIterPrep, OpNow, OpMem, OpAppend, OpUnionAll, OpUnion,
		OpExcept, OpIntersect, OpSort, OpCall2, OpCall, OpCallV,
		OpMakeClosure, OpLoad, OpSave, OpEval, OpFetch:
		return []int{ins.A}
	case OpInput:
		return []int{ins.A}
	default:
		return nil
	}
}

// Optimize runs the structural passes that are currently enabled:
// constant folding, redundant-jump pruning, jump-chain folding, and
// dead-block elimination. Register-level peephole and live-range
// passes are intentionally disabled (their previous implementations
// reused registers across branches incorrectly); the analysis lives in
// liveness.go for callers that need it directly.
func Optimize(fn *Function) {
	for {
		changed := constFold(fn)
		pruneRedundantJumps(fn)
		if foldJumpChains(fn) {
			changed = true
		}
		unreachable := removeUnreachable(fn)
		if unreachable {
			changed = true
		}
		if !changed {
			break
		}
	}
}

// removeUnreachable deletes instructions that can never be executed based on
// control flow analysis.
func removeUnreachable(fn *Function) bool {
	analysis := Infer(fn)
	if analysis == nil || len(analysis.Dead) == 0 {
		return false
	}
	removed := false
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	for pc, ins := range fn.Code {
		if pc < len(analysis.Dead) && analysis.Dead[pc] {
			pcMap[pc] = -1
			removed = true
			continue
		}
		pcMap[pc] = len(newCode)
		newCode = append(newCode, ins)
	}
	if !removed {
		return false
	}
	// map removed PCs to next valid instruction for jump patching
	next := len(newCode)
	for i := len(pcMap) - 1; i >= 0; i-- {
		if pcMap[i] == -1 {
			pcMap[i] = next
		} else {
			next = pcMap[i]
		}
	}
	for i := range newCode {
		ins := &newCode[i]
		switch ins.Op {
		case OpJump:
			if ins.A >= 0 && ins.A < len(pcMap) {
				ins.A = pcMap[ins.A]
			}
		case OpJumpIfFalse, OpJumpIfTrue:
			if ins.B >= 0 && ins.B < len(pcMap) {
				ins.B = pcMap[ins.B]
			}
		}
	}
	fn.Code = newCode
	return true
}
