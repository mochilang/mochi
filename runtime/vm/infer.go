package vm

// RegTag represents the inferred type of a register.
type RegTag = regTag

const (
	TagUnknown RegTag = tagUnknown
	TagInt     RegTag = tagInt
	TagFloat   RegTag = tagFloat
	TagBool    RegTag = tagBool
)

type Analysis struct {
	// In holds inferred register tags on entry to each instruction.
	In [][]RegTag
	// Dead marks unreachable instructions.
	Dead []bool
}

// Infer performs type inference across the control flow graph of fn.
func Infer(fn *Function) *Analysis {
	n := len(fn.Code)
	in := make([][]RegTag, n)
	visited := make([]bool, n)

	init := make([]RegTag, fn.NumRegs)
	for i := range init {
		init[i] = TagUnknown
	}
	if n > 0 {
		in[0] = init
	}

	work := []int{0}
	for len(work) > 0 {
		pc := work[len(work)-1]
		work = work[:len(work)-1]
		if pc < 0 || pc >= n {
			continue
		}
		state := in[pc]
		if state == nil {
			state = make([]RegTag, fn.NumRegs)
			for i := range state {
				state[i] = TagUnknown
			}
			in[pc] = state
		}
		visited[pc] = true

		out := make([]RegTag, fn.NumRegs)
		copy(out, state)
		applyTags(out, fn.Code[pc])

		for _, succ := range successors(fn.Code[pc], pc, n) {
			if succ < 0 || succ >= n {
				continue
			}
			if in[succ] == nil {
				tmp := make([]RegTag, fn.NumRegs)
				copy(tmp, out)
				in[succ] = tmp
				work = append(work, succ)
				continue
			}
			if mergeTags(in[succ], out) {
				work = append(work, succ)
			}
		}
	}

	dead := make([]bool, n)
	for i := 0; i < n; i++ {
		if !visited[i] {
			dead[i] = true
		}
	}
	return &Analysis{In: in, Dead: dead}
}

func successors(ins Instr, pc, n int) []int {
	switch ins.Op {
	case OpJump:
		return []int{ins.A}
	case OpJumpIfFalse, OpJumpIfTrue:
		return []int{pc + 1, ins.B}
	case OpReturn:
		return nil
	default:
		if pc+1 < n {
			return []int{pc + 1}
		}
		return nil
	}
}

func mergeTags(dst, src []RegTag) bool {
	changed := false
	for i := range dst {
		if dst[i] == TagUnknown {
			if src[i] != TagUnknown {
				dst[i] = src[i]
				changed = true
			}
		} else if src[i] != TagUnknown && src[i] != dst[i] {
			dst[i] = TagUnknown
			changed = true
		}
	}
	return changed
}

func applyTags(tags []RegTag, ins Instr) {
	switch ins.Op {
	case OpConst:
		tags[ins.A] = valTag(ins.Val)
	case OpMove:
		tags[ins.A] = tags[ins.B]
	case OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpNegInt:
		tags[ins.A] = TagInt
	case OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpNegFloat:
		tags[ins.A] = TagFloat
	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpNeg:
		tags[ins.A] = TagUnknown
	case OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpNot:
		tags[ins.A] = TagBool
	case OpLen, OpNow:
		tags[ins.A] = TagInt
	case OpJSON, OpPrint, OpPrint2, OpPrintN:
		// no result
	case OpInput, OpMakeList, OpIndex, OpSlice, OpSetIndex, OpCall, OpCall2, OpCallV,
		OpUnionAll, OpUnion, OpExcept, OpIntersect, OpSort, OpMakeClosure, OpExpect:
		tags[ins.A] = TagUnknown
	case OpIterPrep:
		tags[ins.A] = TagUnknown
	case OpCast:
		tags[ins.A] = TagUnknown
	case OpCount:
		tags[ins.A] = TagInt
	case OpAvg:
		tags[ins.A] = TagFloat
	case OpSum:
		tags[ins.A] = TagUnknown
	case OpSqrt:
		tags[ins.A] = TagFloat
	case OpMin, OpMax:
		tags[ins.A] = TagUnknown
	case OpValues:
		tags[ins.A] = TagUnknown
	}
}
