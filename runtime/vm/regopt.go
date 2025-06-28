package vm

import (
	"fmt"
	"strings"
)

// Lifetime represents the active range of a register.
type Lifetime struct {
	Start int // first instruction index where the register is live
	End   int // last instruction index where the register is live
}

// RegUsage returns a histogram of how many times each register is used or defined in fn.
func RegUsage(fn *Function) []int {
	usage := make([]int, fn.NumRegs)
	for pc := 0; pc < len(fn.Code); pc++ {
		use, def := useDef(fn.Code[pc], fn.NumRegs)
		for r, u := range use {
			if u {
				usage[r]++
			}
		}
		for r, d := range def {
			if d {
				usage[r]++
			}
		}
	}
	return usage
}

// RegLifetime computes the lifetime of each register in fn.
// The lifetime starts at the first definition or use and ends at the last use.
func RegLifetime(fn *Function) []Lifetime {
	lt := make([]Lifetime, fn.NumRegs)
	for i := range lt {
		lt[i].Start = -1
		lt[i].End = -1
	}
	for pc := 0; pc < len(fn.Code); pc++ {
		use, def := useDef(fn.Code[pc], fn.NumRegs)
		for r, d := range def {
			if d {
				if lt[r].Start == -1 {
					lt[r].Start = pc
				}
				if pc > lt[r].End {
					lt[r].End = pc
				}
			}
		}
		for r, u := range use {
			if u {
				if lt[r].Start == -1 {
					lt[r].Start = pc
				}
				if pc > lt[r].End {
					lt[r].End = pc
				}
			}
		}
	}
	return lt
}

// InterferenceGraph builds a graph where an edge exists if two registers are live at the same time.
func InterferenceGraph(fn *Function) map[int]map[int]bool {
	info := Liveness(fn)
	g := make(map[int]map[int]bool)
	for r := 0; r < fn.NumRegs; r++ {
		g[r] = make(map[int]bool)
	}
	for pc := 0; pc < len(fn.Code); pc++ {
		live := info.In[pc]
		for i := 0; i < fn.NumRegs; i++ {
			if !live[i] {
				continue
			}
			for j := i + 1; j < fn.NumRegs; j++ {
				if live[j] {
					g[i][j] = true
					g[j][i] = true
				}
			}
		}
	}
	return g
}

// VisualizeUsage returns an ASCII chart of register lifetimes.
func VisualizeUsage(fn *Function) string {
	lt := RegLifetime(fn)
	usage := RegUsage(fn)
	var b strings.Builder
	fmt.Fprintf(&b, "pc    ")
	for pc := 0; pc < len(fn.Code); pc++ {
		fmt.Fprintf(&b, "%d", pc%10)
	}
	b.WriteByte('\n')
	for r := 0; r < fn.NumRegs; r++ {
		fmt.Fprintf(&b, "r%-3d ", r)
		start := lt[r].Start
		end := lt[r].End
		for pc := 0; pc < len(fn.Code); pc++ {
			if start == -1 {
				b.WriteByte(' ')
				continue
			}
			if pc < start || pc > end {
				b.WriteByte(' ')
			} else if pc == start || pc == end {
				b.WriteByte('|')
			} else {
				b.WriteByte('-')
			}
		}
		fmt.Fprintf(&b, " %d\n", usage[r])
	}
	return b.String()
}

// replaceReg rewrites all register references to old with new in ins.
func replaceReg(ins *Instr, old, new int) {
	if ins.A == old {
		ins.A = new
	}
	if ins.B == old {
		ins.B = new
	}
	if ins.C == old {
		ins.C = new
	}
	if ins.D == old {
		ins.D = new
	}
}

// regUsedInRange reports if r is referenced as part of a contiguous
// register range like those used by MakeList or Call instructions.
func regUsedInRange(ins Instr, r int) bool {
	switch ins.Op {
	case OpMakeList, OpPrintN:
		return r >= ins.C && r < ins.C+ins.B
	case OpMakeMap:
		return r >= ins.C && r < ins.C+ins.B*2
	case OpCall:
		return r >= ins.D && r < ins.D+ins.C
	case OpCallV, OpMakeClosure:
		if r >= ins.D && r < ins.D+ins.C {
			return true
		}
	}
	return false
}

// coalesceMoves attempts to merge registers involved in simple move
// operations when their lifetimes do not overlap. It returns true if any
// instructions were modified.
func coalesceMoves(fn *Function, info *LiveInfo) bool {
	lt := RegLifetime(fn)
	changed := false
	for pc := 0; pc < len(fn.Code); pc++ {
		ins := fn.Code[pc]
		if ins.Op != OpMove || ins.A == ins.B {
			continue
		}
		dst, src := ins.A, ins.B
		if info.In[pc][dst] || info.Out[pc][src] {
			continue
		}
		end := lt[dst].End
		safe := true
		for i := pc + 1; i <= end && i < len(fn.Code); i++ {
			if regUsedInRange(fn.Code[i], dst) {
				safe = false
				break
			}
		}
		if !safe {
			continue
		}
		for i := pc + 1; i < len(fn.Code); i++ {
			replaceReg(&fn.Code[i], dst, src)
		}
		fn.Code = append(fn.Code[:pc], fn.Code[pc+1:]...)
		changed = true
		pc--
	}
	return changed
}
