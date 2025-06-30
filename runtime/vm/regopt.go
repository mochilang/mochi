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

// CompactRegisters remaps registers to reduce the overall count while keeping
// contiguous blocks used by instructions such as MakeList or MakeMap intact.
// It implements a simple linear-scan allocator extended to handle
// multi-register groups. Parameter registers keep their original numbers to
// preserve the calling convention.
func CompactRegisters(fn *Function) {
	lt := RegLifetime(fn)

	// build contiguity groups for non-parameter registers
	ufParent := make([]int, fn.NumRegs)
	for i := range ufParent {
		ufParent[i] = i
	}
	var find func(int) int
	find = func(x int) int {
		if ufParent[x] != x {
			ufParent[x] = find(ufParent[x])
		}
		return ufParent[x]
	}
	union := func(a, b int) {
		pa, pb := find(a), find(b)
		if pa != pb {
			ufParent[pb] = pa
		}
	}
	for _, ins := range fn.Code {
		switch ins.Op {
		case OpMakeList:
			for i := 0; i < ins.B-1; i++ {
				r1 := ins.C + i
				r2 := ins.C + i + 1
				if r1 >= fn.NumParams && r2 >= fn.NumParams {
					union(r1, r2)
				}
			}
		case OpMakeMap:
			n := ins.B * 2
			for i := 0; i < n-1; i++ {
				r1 := ins.C + i
				r2 := ins.C + i + 1
				if r1 >= fn.NumParams && r2 >= fn.NumParams {
					union(r1, r2)
				}
			}
		case OpCall, OpCallV, OpMakeClosure, OpPrintN:
			for i := 0; i < ins.C-1; i++ {
				r1 := ins.D + i
				r2 := ins.D + i + 1
				if r1 >= fn.NumParams && r2 >= fn.NumParams {
					union(r1, r2)
				}
			}
		}
	}

	groupsMap := map[int][]int{}
	for r := 0; r < fn.NumRegs; r++ {
		if r < fn.NumParams {
			continue
		}
		root := find(r)
		groupsMap[root] = append(groupsMap[root], r)
	}

	type group struct {
		regs  []int
		start int
		end   int
	}
	var groups []group
	for _, regs := range groupsMap {
		sort.Ints(regs)
		gs, ge := -1, -1
		for _, r := range regs {
			lt := lt[r]
			if gs == -1 || lt.Start < gs {
				gs = lt.Start
			}
			if lt.End > ge {
				ge = lt.End
			}
		}
		groups = append(groups, group{regs: regs, start: gs, end: ge})
	}

	sort.Slice(groups, func(i, j int) bool { return groups[i].start < groups[j].start })

	mapping := make([]int, fn.NumRegs)
	for i := range mapping {
		mapping[i] = -1
	}

	type activeGroup struct {
		end, start, size int
		regs             []int
	}
	var active []activeGroup
	type freeRange struct{ start, size int }
	var free []freeRange

	next := fn.NumParams
	// parameters keep their numbers
	for r := 0; r < fn.NumParams && r < len(mapping); r++ {
		mapping[r] = r
		if next <= r {
			next = r + 1
		}
	}

	for _, g := range groups {
		tmp := active[:0]
		for _, a := range active {
			if a.end < g.start {
				free = append(free, freeRange{start: a.start, size: a.size})
			} else {
				tmp = append(tmp, a)
			}
		}
		active = tmp

		base := -1
		idx := -1
		for i, fr := range free {
			if fr.size >= len(g.regs) {
				base = fr.start
				fr.start += len(g.regs)
				fr.size -= len(g.regs)
				if fr.size == 0 {
					idx = i
				} else {
					free[i] = fr
					idx = -1
				}
				break
			}
		}
		if base == -1 {
			base = next
			next += len(g.regs)
		} else if idx >= 0 {
			free = append(free[:idx], free[idx+1:]...)
		}

		for i, r := range g.regs {
			mapping[r] = base + i
		}
		active = append(active, activeGroup{end: g.end, start: base, size: len(g.regs), regs: g.regs})
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
		switch ins.Op {
		case OpMakeList:
			start := remap(ins.C)
			ins.C = start
			for j := 1; j < ins.B; j++ {
				_ = remap(ins.C + j)
			}
		case OpMakeMap:
			start := remap(ins.C)
			ins.C = start
			for j := 1; j < ins.B*2; j++ {
				_ = remap(ins.C + j)
			}
		case OpCall, OpCallV, OpMakeClosure, OpPrintN:
			start := remap(ins.D)
			ins.D = start
			for j := 0; j < ins.C; j++ {
				_ = remap(ins.D + j)
			}
		default:
			ins.C = remap(ins.C)
			ins.D = remap(ins.D)
		}
	}
	fn.NumRegs = next
}
