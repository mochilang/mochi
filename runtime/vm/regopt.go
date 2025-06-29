package vm

import (
	"fmt"
	"sort"
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

// hasMakeMap reports whether fn contains any MakeMap instructions. Register
// compaction needs to know this to avoid remapping the contiguous key/value
// register ranges used by MakeMap.
func hasMakeMap(fn *Function) bool {
	for _, ins := range fn.Code {
		if ins.Op == OpMakeMap {
			return true
		}
	}
	return false
}

// CompactRegisters remaps registers to reduce the overall count by reusing
// registers whose lifetimes do not overlap. Parameters keep their original
// numbers to avoid changing the calling convention.
func CompactRegisters(fn *Function) {
	lt := RegLifetime(fn)
	type regInfo struct{ r, start, end int }
	regs := make([]regInfo, 0, fn.NumRegs)
	for r := 0; r < fn.NumRegs; r++ {
		if lt[r].Start == -1 {
			continue
		}
		regs = append(regs, regInfo{r: r, start: lt[r].Start, end: lt[r].End})
	}
	sort.Slice(regs, func(i, j int) bool { return regs[i].start < regs[j].start })

	mapping := make([]int, fn.NumRegs)
	for i := range mapping {
		mapping[i] = -1
	}

	type activeReg struct{ old, end, new int }
	var active []activeReg
	var free []int
	next := fn.NumParams

	// Reserve parameter registers
	for r := 0; r < fn.NumParams && r < len(mapping); r++ {
		mapping[r] = r
		active = append(active, activeReg{old: r, end: lt[r].End, new: r})
		if next <= r {
			next = r + 1
		}
	}

	for _, reg := range regs {
		if reg.r < fn.NumParams {
			continue
		}
		// expire old regs
		tmp := active[:0]
		for _, a := range active {
			if a.end < reg.start {
				free = append(free, a.new)
			} else {
				tmp = append(tmp, a)
			}
		}
		active = tmp

		// allocate new register
		var nr int
		if len(free) > 0 {
			nr = free[len(free)-1]
			free = free[:len(free)-1]
		} else {
			nr = next
			next++
		}
		mapping[reg.r] = nr
		active = append(active, activeReg{old: reg.r, end: reg.end, new: nr})
	}

	remap := func(r int) int {
		if r >= 0 && r < len(mapping) && mapping[r] >= 0 {
			return mapping[r]
		}
		return r
	}

	for i := range fn.Code {
		ins := &fn.Code[i]
		ins.A = remap(ins.A)
		ins.B = remap(ins.B)
		ins.C = remap(ins.C)
		ins.D = remap(ins.D)
	}
	fn.NumRegs = next
}
