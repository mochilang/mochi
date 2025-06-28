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

// replaceRegUse substitutes register references to "old" with "new" in ins.
// It returns false if the register appears in a variable-width operand that
// cannot be updated individually (eg. OpMakeList).
func replaceRegUse(ins *Instr, old, new int) bool {
	switch ins.Op {
	case OpMove:
		if ins.B == old {
			ins.B = new
		}
	case OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpAddInt, OpAddFloat, OpSubInt, OpSubFloat,
		OpMulInt, OpMulFloat, OpDivInt, OpDivFloat,
		OpModInt, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat,
		OpLessEqInt, OpLessEqFloat, OpIn,
		OpAppend, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
	case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpExists,
		OpCount, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast, OpIterPrep, OpNow, OpSort:
		if ins.B == old {
			ins.B = new
		}
	case OpIndex:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
	case OpSlice:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
		if ins.D == old {
			ins.D = new
		}
	case OpSetIndex:
		if ins.A == old {
			ins.A = new
		}
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
	case OpMakeList:
		if old >= ins.C && old < ins.C+ins.B {
			return false
		}
	case OpMakeMap:
		if old >= ins.C && old < ins.C+ins.B*2 {
			return false
		}
	case OpPrint2:
		if ins.A == old {
			ins.A = new
		}
		if ins.B == old {
			ins.B = new
		}
	case OpPrint:
		if ins.A == old {
			ins.A = new
		}
	case OpPrintN:
		if old >= ins.C && old < ins.C+ins.B {
			return false
		}
	case OpJumpIfFalse, OpJumpIfTrue, OpExpect:
		if ins.A == old {
			ins.A = new
		}
	case OpLen:
		if ins.A == old {
			ins.A = new
		}
		if ins.B == old {
			ins.B = new
		}
	case OpCall2:
		if ins.C == old {
			ins.C = new
		}
		if ins.D == old {
			ins.D = new
		}
	case OpCall:
		if old >= ins.D && old < ins.D+ins.C {
			return false
		}
	case OpCallV:
		if ins.B == old {
			ins.B = new
		}
		if old >= ins.D && old < ins.D+ins.C {
			return false
		}
	case OpMakeClosure:
		if old >= ins.D && old < ins.D+ins.C {
			return false
		}
	case OpLoad:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
	case OpSave:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
		if ins.D == old {
			ins.D = new
		}
	case OpEval:
		if ins.B == old {
			ins.B = new
		}
	case OpFetch:
		if ins.B == old {
			ins.B = new
		}
		if ins.C == old {
			ins.C = new
		}
	case OpReturn:
		if ins.A == old {
			ins.A = new
		}
	}
	return true
}

// coalesceRegisters merges virtual registers whose lifetimes do not overlap.
// It primarily removes redundant Move instructions.
func coalesceRegisters(fn *Function) bool {
	changed := false
	for {
		analysis := Liveness(fn)
		madeChange := false
		for pc := 0; pc < len(fn.Code); pc++ {
			ins := fn.Code[pc]
			if ins.Op != OpMove {
				continue
			}
			src := ins.B
			dst := ins.A
			if analysis.Out[pc][src] || analysis.In[pc][dst] {
				continue
			}
			ok := true
			for i := pc + 1; i < len(fn.Code); i++ {
				use, def := useDef(fn.Code[i], fn.NumRegs)
				if use[dst] {
					if !replaceRegUse(&fn.Code[i], dst, src) {
						ok = false
						break
					}
				}
				if def[dst] {
					break
				}
			}
			if ok {
				fn.Code = append(fn.Code[:pc], fn.Code[pc+1:]...)
				madeChange = true
				changed = true
				break
			}
		}
		if !madeChange {
			break
		}
	}

	maxReg := 0
	for _, ins := range fn.Code {
		regs := []int{ins.A, ins.B, ins.C, ins.D}
		for _, r := range regs {
			if r > maxReg {
				maxReg = r
			}
		}
		switch ins.Op {
		case OpMakeList:
			if v := ins.C + ins.B - 1; v > maxReg {
				maxReg = v
			}
		case OpMakeMap:
			if v := ins.C + ins.B*2 - 1; v > maxReg {
				maxReg = v
			}
		case OpPrintN:
			if v := ins.C + ins.B - 1; v > maxReg {
				maxReg = v
			}
		case OpCall, OpMakeClosure:
			if v := ins.D + ins.C - 1; v > maxReg {
				maxReg = v
			}
		case OpCallV:
			if v := ins.D + ins.C - 1; v > maxReg {
				maxReg = v
			}
		}
	}
	fn.NumRegs = maxReg + 1
	return changed
}
