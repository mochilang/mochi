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

// renameReg replaces all references to old register with new register
// in instruction ins for simple register fields (A, B, C, D).
func renameReg(ins *Instr, old, new int) {
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

// canRename reports whether register reg can be safely renamed in ins without
// modifying compact register ranges used by some instructions.
func canRename(ins Instr, reg int) bool {
	switch ins.Op {
	case OpMakeList, OpPrintN:
		if reg >= ins.C && reg < ins.C+ins.B {
			return false
		}
	case OpMakeMap:
		if reg >= ins.C && reg < ins.C+ins.B*2 {
			return false
		}
	case OpCall, OpCallV, OpMakeClosure:
		if reg >= ins.D && reg < ins.D+ins.C {
			return false
		}
	}
	return true
}

// definesReg checks if instruction ins defines register r.
func definesReg(ins Instr, r int) bool {
	defs := defRegs(ins)
	for _, d := range defs {
		if d == r {
			return true
		}
	}
	return false
}

// coalesceMoves merges registers connected by a simple Move when their lifetimes
// do not overlap. It returns true if any change was made.
func coalesceMoves(fn *Function, info *LiveInfo) bool {
	removed := false
	remove := make([]bool, len(fn.Code))

	for pc := 0; pc < len(fn.Code); pc++ {
		ins := fn.Code[pc]
		if ins.Op != OpMove || ins.A == ins.B {
			continue
		}
		dst := ins.A
		src := ins.B
		if dst < 0 || dst >= fn.NumRegs || src < 0 || src >= fn.NumRegs {
			continue
		}
		// destination must not be live before this instruction and
		// source must be dead afterwards
		if info.In[pc][dst] || info.Out[pc][src] {
			continue
		}
		// ensure we can rename all uses until the next definition of dst
		ok := true
		for j := pc + 1; j < len(fn.Code); j++ {
			insj := fn.Code[j]
			if definesReg(insj, dst) {
				break
			}
			use, _ := useDef(insj, fn.NumRegs)
			if use[dst] {
				if !canRename(insj, dst) {
					ok = false
					break
				}
			}
		}
		if !ok {
			continue
		}
		// rename uses
		for j := pc + 1; j < len(fn.Code); j++ {
			insp := &fn.Code[j]
			if definesReg(*insp, dst) {
				break
			}
			renameReg(insp, dst, src)
		}
		remove[pc] = true
		removed = true
	}

	if !removed {
		return false
	}

	// rebuild instruction list without removed moves
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	for pc, ins := range fn.Code {
		if remove[pc] {
			pcMap[pc] = -1
			continue
		}
		pcMap[pc] = len(newCode)
		newCode = append(newCode, ins)
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

	// fix jumps
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
