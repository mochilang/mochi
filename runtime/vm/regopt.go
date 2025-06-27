package vm

import "strings"

// Lifetime represents the start and end instruction index for a register.
type Lifetime struct {
	Start int // instruction index where register is first used or defined
	End   int // instruction index of last use
}

// RegUsage returns a histogram of register usage counts for fn.
func RegUsage(fn *Function) []int {
	usage := make([]int, fn.NumRegs)
	for _, ins := range fn.Code {
		for _, r := range regsForInstr(ins) {
			if r >= 0 && r < fn.NumRegs {
				usage[r]++
			}
		}
	}
	return usage
}

// RegLifetimes returns the lifetime for each register in fn.
func RegLifetimes(fn *Function) []Lifetime {
	life := make([]Lifetime, fn.NumRegs)
	for i := range life {
		life[i] = Lifetime{Start: -1, End: -1}
	}
	for pc, ins := range fn.Code {
		for _, r := range regsForInstr(ins) {
			if r < 0 || r >= fn.NumRegs {
				continue
			}
			lt := &life[r]
			if lt.Start == -1 {
				lt.Start = pc
			}
			lt.End = pc
		}
	}
	return life
}

// InterferenceGraph builds a simple interference graph for the given lifetimes.
// The result is an adjacency matrix g where g[i][j] is true if lifetimes i and j overlap.
func InterferenceGraph(lifetimes []Lifetime) [][]bool {
	n := len(lifetimes)
	g := make([][]bool, n)
	for i := range g {
		g[i] = make([]bool, n)
	}
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if lifetimesOverlap(lifetimes[i], lifetimes[j]) {
				g[i][j] = true
				g[j][i] = true
			}
		}
	}
	return g
}

// VisualizeRegUsage returns an ASCII visualization of register lifetimes.
// Each register is shown on its own line with a bar spanning from start to end.
func VisualizeRegUsage(fn *Function) string {
	life := RegLifetimes(fn)
	var out strings.Builder
	for r, lt := range life {
		out.WriteString(formatReg(r))
		out.WriteString(": ")
		for i := 0; i < len(fn.Code); i++ {
			if i >= lt.Start && i <= lt.End && lt.Start != -1 {
				out.WriteRune('#')
			} else {
				out.WriteRune('.')
			}
		}
		out.WriteByte('\n')
	}
	return out.String()
}

// lifetimesOverlap returns true if a and b overlap.
func lifetimesOverlap(a, b Lifetime) bool {
	if a.Start == -1 || b.Start == -1 {
		return false
	}
	return a.Start <= b.End && b.Start <= a.End
}

// regsForInstr returns all register indices referenced by ins.
func regsForInstr(ins Instr) []int {
	switch ins.Op {
	case OpJump:
		// Jump uses only A as target label, not a register
		return nil
	}
	regs := []int{}
	if ins.A >= 0 {
		regs = append(regs, ins.A)
	}
	if ins.B >= 0 {
		regs = append(regs, ins.B)
	}
	if ins.C >= 0 {
		regs = append(regs, ins.C)
	}
	if ins.D >= 0 {
		regs = append(regs, ins.D)
	}
	return regs
}
