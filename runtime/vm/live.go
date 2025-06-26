package vm

// Liveness holds live-in and live-out sets for each instruction of a function.
type Liveness struct {
	In  [][]bool
	Out [][]bool
}

// Live performs liveness analysis for registers in fn.
func Live(fn *Function) *Liveness {
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
			ins := fn.Code[pc]
			// compute out[pc]
			newOut := make([]bool, fn.NumRegs)
			for _, succ := range liveSuccs(ins, pc, n) {
				if succ < 0 || succ >= n {
					continue
				}
				for r, v := range in[succ] {
					if v {
						newOut[r] = true
					}
				}
			}

			// compute in[pc]
			newIn := make([]bool, fn.NumRegs)
			uses := uses(ins)
			for _, r := range uses {
				if r >= 0 && r < fn.NumRegs {
					newIn[r] = true
				}
			}
			def, hasDef := defReg(ins)
			for r, v := range newOut {
				if v {
					if !hasDef || r != def {
						newIn[r] = true
					}
				}
			}

			if !equalBoolSlice(newOut, out[pc]) || !equalBoolSlice(newIn, in[pc]) {
				changed = true
				out[pc] = newOut
				in[pc] = newIn
			}
		}
	}

	return &Liveness{In: in, Out: out}
}

func equalBoolSlice(a, b []bool) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
