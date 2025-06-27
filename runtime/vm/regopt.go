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
