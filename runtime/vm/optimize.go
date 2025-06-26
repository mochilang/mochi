package vm

// optimizeFunction removes dead register writes from fn.
func optimizeFunction(fn *Function) {
	for {
		changed := removeDeadOnce(fn)
		if !changed {
			break
		}
	}
	// shrink register count
	max := fn.NumParams
	for _, ins := range fn.Code {
		for _, r := range regsUsed(ins) {
			if r+1 > max {
				max = r + 1
			}
		}
	}
	fn.NumRegs = max
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
