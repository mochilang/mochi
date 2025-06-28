package vm

// OptimizeWithProfile removes cold instructions with zero execution count.
// Only pure instructions that are not jump targets are eliminated.
func OptimizeWithProfile(fn *Function, counts []int) {
	if len(counts) == 0 || len(fn.Code) == 0 {
		return
	}
	jumpTargets := map[int]bool{}
	for pc, ins := range fn.Code {
		switch ins.Op {
		case OpJump:
			if ins.A >= 0 && ins.A < len(fn.Code) {
				jumpTargets[ins.A] = true
			}
		case OpJumpIfFalse, OpJumpIfTrue:
			if ins.B >= 0 && ins.B < len(fn.Code) {
				jumpTargets[ins.B] = true
			}
		}
		_ = pc
	}
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	changed := false
	for pc, ins := range fn.Code {
		remove := false
		if pc < len(counts) && counts[pc] == 0 && isPure(ins.Op) && !jumpTargets[pc] {
			remove = true
			changed = true
			pcMap[pc] = -1
		}
		if !remove {
			pcMap[pc] = len(newCode)
			newCode = append(newCode, ins)
		}
	}
	if !changed {
		return
	}
	// map removed PCs to next valid instruction
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
}

// OptimizeProgramWithProfile runs OptimizeWithProfile on all functions of prog.
func OptimizeProgramWithProfile(prog *Program, prof *Profile) {
	if prof == nil {
		return
	}
	for i := range prog.Funcs {
		if i < len(prof.Funcs) {
			OptimizeWithProfile(&prog.Funcs[i], prof.Funcs[i])
		}
	}
}
