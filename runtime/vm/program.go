package vm

import (
	"fmt"
	"strings"

	"mochi/types"
)

type Instr struct {
	Op Op
	// Quick is MEP-18 Phase C's bytecode rewriting slot. When non-zero
	// it overrides Op in the dispatch loop, letting MEP-19 quickening
	// patch a generic opcode (OpIndex) to a tag-specialized variant
	// (OpIndex_List) without touching the original Op. A site that
	// observes a polymorphic tag deopts by clearing Quick back to 0
	// and bumping the per-Function quickMisses counter; after
	// siteMaxMisses the site is bypassed for the lifetime of the run.
	Quick uint8
	A     int
	B     int
	C     int
	D     int
	Val   Value // inline constant for OpConst
	Line  int   // source line for disassembly
}

type Function struct {
	Code       []Instr
	NumRegs    int
	NumParams  int
	Name       string
	TypeParams []string
	Line       int // source line of function definition

	// callSites is MEP-19's per-instruction monomorphic call cache.
	// nil until the first OpCallV in this function executes; sized to
	// len(Code). Slots are also nil until first observation.
	callSites []*callSite `json:"-"`

	// quickMisses is MEP-19's per-instruction polymorphism counter for
	// quickened sites (OpIndex, etc). nil until the first quickenable
	// op runs; sized to len(Code). When a slot reaches siteMaxMisses
	// the site is bypassed and Quick is never re-set.
	quickMisses []uint8 `json:"-"`
}

type Program struct {
	Funcs      []Function
	Types      []types.Type
	File       string   `json:"file,omitempty"`
	Source     []string `json:"source,omitempty"`
	NumGlobals int      `json:"num_globals,omitempty"`
}

type closure struct {
	fn   int
	args []Value
}

func (p *Program) funcName(idx int) string {
	if idx < 0 || idx >= len(p.Funcs) {
		return fmt.Sprintf("%d", idx)
	}
	fn := p.Funcs[idx]
	name := fn.Name
	if name == "" {
		if idx == 0 {
			return "main"
		}
		return fmt.Sprintf("fn%d", idx)
	}
	return name + formatTypeParams(fn.TypeParams)
}

// Disassemble returns a human-readable listing of the program instructions.
// If src is provided, source lines are included as comments.
func (p *Program) Disassemble(src string) string {
	if src == "" && len(p.Source) > 0 {
		src = strings.Join(p.Source, "\n")
	}
	lines := strings.Split(src, "\n")
	var b strings.Builder
	for idx, fn := range p.Funcs {
		name := fn.Name
		if name == "" {
			if idx == 0 {
				name = "main"
			} else {
				name = fmt.Sprintf("fn%d", idx)
			}
		}

		// build label map for jump targets
		labels := map[int]string{}
		nextLabel := 0
		for i, ins := range fn.Code {
			switch ins.Op {
			case OpJump:
				if _, ok := labels[ins.A]; !ok {
					labels[ins.A] = fmt.Sprintf("L%d", nextLabel)
					nextLabel++
				}
			case OpJumpIfFalse, OpJumpIfTrue:
				if _, ok := labels[ins.B]; !ok {
					labels[ins.B] = fmt.Sprintf("L%d", nextLabel)
					nextLabel++
				}
			}
			_ = i
		}

		if fn.Line > 0 && fn.Line <= len(lines) {
			fmt.Fprintf(&b, "  // %s\n", strings.TrimSpace(lines[fn.Line-1]))
		}
		fmt.Fprintf(&b, "func %s%s (regs=%d)\n", name, formatTypeParams(fn.TypeParams), fn.NumRegs)
		lastLine := 0
		for pc, ins := range fn.Code {
			if lbl, ok := labels[pc]; ok {
				fmt.Fprintf(&b, "%s:\n", lbl)
			}
			if ins.Line != lastLine && ins.Line > 0 && ins.Line <= len(lines) {
				fmt.Fprintf(&b, "  // %s\n", strings.TrimSpace(lines[ins.Line-1]))
				lastLine = ins.Line
			}
			fmt.Fprintf(&b, "  %-12s ", ins.Op)
			switch ins.Op {
			case OpConst:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), valueToString(ins.Val))
			case OpMove:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpAdd, OpSub, OpMul, OpDiv, OpMod,
				OpAddInt, OpAddFloat, OpSubInt, OpSubFloat,
				OpMulInt, OpMulFloat, OpDivInt, OpDivFloat,
				OpModInt, OpModFloat, OpPow,
				OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
				OpLess, OpLessEq, OpLessInt, OpLessFloat,
				OpLessEqInt, OpLessEqFloat, OpIn:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpSetIndex:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpJump:
				if lbl, ok := labels[ins.A]; ok {
					fmt.Fprintf(&b, "%s", lbl)
				} else {
					fmt.Fprintf(&b, "%d", ins.A)
				}
			case OpJumpIfFalse, OpJumpIfTrue:
				if lbl, ok := labels[ins.B]; ok {
					fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), lbl)
				} else {
					fmt.Fprintf(&b, "%s, %d", formatReg(ins.A), ins.B)
				}
			case OpLen:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpIndex:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpSlice:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpMakeList:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpMakeMap:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpPrint:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpPrint2:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpPrintN:
				fmt.Fprintf(&b, "%s, %d, %s", formatReg(ins.A), ins.B, formatReg(ins.C))
			case OpNow:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpJSON:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpAppend:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpUnionAll, OpUnion, OpExcept, OpIntersect:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C))
			case OpSort:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpStr:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpUpper:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpLower:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpReverse:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpPadStart:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpInput:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpFirst:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpIterPrep:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpCount:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpExists:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpCast:
				typ := p.Types[ins.C]
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), formatReg(ins.B), typ)
			case OpAvg, OpSum, OpMin, OpMax:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			case OpExpect:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpMakeClosure:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), p.funcName(ins.B), ins.C, formatReg(ins.D))
			case OpCall2:
				fmt.Fprintf(&b, "%s, %s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatReg(ins.C), formatReg(ins.D))
			case OpCall:
				fmt.Fprintf(&b, "%s, %s, %s", formatReg(ins.A), p.funcName(ins.B), formatRegs(ins.D, ins.C))
			case OpCallV:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), ins.C, formatReg(ins.D))
			case OpGoCall, OpGoAutoCall:
				fmt.Fprintf(&b, "%s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), ins.C, formatReg(ins.D))
			case OpPyCall:
				fmt.Fprintf(&b, "%s, %s, %s, %d, %s", formatReg(ins.A), formatReg(ins.B), formatReg(ins.C), ins.Val.Int, formatReg(ins.D))
			case OpReturn:
				fmt.Fprintf(&b, "%s", formatReg(ins.A))
			case OpNot, OpNeg, OpNegInt, OpNegFloat:
				fmt.Fprintf(&b, "%s, %s", formatReg(ins.A), formatReg(ins.B))
			default:
				fmt.Fprintf(&b, "%d,%d,%d,%d", ins.A, ins.B, ins.C, ins.D)
			}
			b.WriteByte('\n')
		}
		b.WriteByte('\n')
	}
	// Normalize trailing newline so the disassembly matches
	// checked-in golden files for dataset queries.
	out := strings.TrimRight(b.String(), "\n")
	return out + "\n"
}

