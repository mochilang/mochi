package vm

import (
	"math"

	"mochi/interpreter"
)

// Automatically generated liveness analysis and optimization

// LiveInfo stores liveness sets for each instruction.
type LiveInfo struct {
	In  [][]bool
	Out [][]bool
}

// Liveness performs a backward dataflow analysis computing live registers at
// entry and exit of each instruction.
func Liveness(fn *Function) *LiveInfo {
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
			// compute out set
			outSet := make([]bool, fn.NumRegs)
			for _, succ := range successors(fn.Code[pc], pc, n) {
				if succ < 0 || succ >= n {
					continue
				}
				orSet(outSet, in[succ])
			}
			use, def := useDef(fn.Code[pc], fn.NumRegs)
			inSet := make([]bool, fn.NumRegs)
			for r := 0; r < fn.NumRegs; r++ {
				inSet[r] = use[r] || (outSet[r] && !def[r])
			}
			if !equalSet(outSet, out[pc]) || !equalSet(inSet, in[pc]) {
				out[pc] = outSet
				in[pc] = inSet
				changed = true
			}
		}
	}
	return &LiveInfo{In: in, Out: out}
}

func orSet(dst, src []bool) {
	for i, v := range src {
		if v {
			dst[i] = true
		}
	}
}

func equalSet(a, b []bool) bool {
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// useDef returns the used and defined registers for an instruction.
func useDef(ins Instr, n int) (use, def []bool) {
	use = make([]bool, n)
	def = make([]bool, n)
	addUse := func(r int) {
		if r >= 0 && r < n {
			use[r] = true
		}
	}
	addDef := func(r int) {
		if r >= 0 && r < n {
			def[r] = true
		}
	}
	switch ins.Op {
	case OpConst:
		addDef(ins.A)
	case OpMove:
		addDef(ins.A)
		addUse(ins.B)
	case OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpLike, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpExists,
		OpLen, OpCount, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast, OpIterPrep, OpNow:
		addDef(ins.A)
		addUse(ins.B)
	case OpAppend:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpIndex:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpSlice:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	case OpSetIndex:
		addUse(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpMakeList:
		addDef(ins.A)
		for i := 0; i < ins.B; i++ {
			addUse(ins.C + i)
		}
	case OpMakeMap:
		addDef(ins.A)
		for i := 0; i < ins.B*2; i++ {
			addUse(ins.C + i)
		}
	case OpCall2:
		addDef(ins.A)
		addUse(ins.C)
		addUse(ins.D)
	case OpCall:
		addDef(ins.A)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpCallV:
		addDef(ins.A)
		addUse(ins.B)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpMakeClosure:
		addDef(ins.A)
		for i := 0; i < ins.C; i++ {
			addUse(ins.D + i)
		}
	case OpLoad:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpSave:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	case OpEval:
		addDef(ins.A)
		addUse(ins.B)
	case OpFetch:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpReturn:
		addUse(ins.A)
	case OpJumpIfFalse, OpJumpIfTrue, OpExpect:
		addUse(ins.A)
	case OpPrint, OpPrint2, OpJSON:
		addUse(ins.A)
		addUse(ins.B)
	case OpPrintN:
		for i := 0; i < ins.B; i++ {
			addUse(ins.C + i)
		}
	default:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
		addUse(ins.D)
	}
	return
}

func isPure(op Op) bool {
	switch op {
	case OpJump, OpJumpIfFalse, OpJumpIfTrue, OpReturn,
		OpPrint, OpPrint2, OpPrintN, OpJSON,
		OpSetIndex, OpLoad, OpSave, OpEval, OpFetch,
		OpCall, OpCall2, OpCallV, OpExpect:
		return false
	default:
		return true
	}
}

func defRegs(ins Instr) []int {
	switch ins.Op {
	case OpConst, OpMove, OpAdd, OpSub, OpMul, OpDiv, OpMod,
		OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
		OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
		OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpLike, OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpExists,
		OpLen, OpIndex, OpSlice, OpMakeList, OpMakeMap,
		OpCount, OpAvg, OpSum, OpMin, OpMax, OpValues,
		OpCast, OpIterPrep, OpNow, OpAppend, OpUnionAll, OpUnion,
		OpExcept, OpIntersect, OpSort, OpCall2, OpCall, OpCallV,
		OpMakeClosure, OpLoad, OpSave, OpEval, OpFetch:
		return []int{ins.A}
	case OpInput:
		return []int{ins.A}
	default:
		return nil
	}
}

// Optimize removes dead instructions with no side effects.
func Optimize(fn *Function) {
	for {
		changed := constFold(fn)
		pruneRedundantJumps(fn)
		analysis := Liveness(fn)
		removed := removeDead(fn, analysis)
		if !removed && !changed {
			break
		}
	}
}

// removeDead eliminates instructions that only define dead registers.
func removeDead(fn *Function, analysis *LiveInfo) bool {
	removed := false
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	for pc, ins := range fn.Code {
		canRemove := false
		if isPure(ins.Op) {
			defs := defRegs(ins)
			if len(defs) > 0 {
				allDead := true
				for _, r := range defs {
					if r >= 0 && r < fn.NumRegs && analysis.Out[pc][r] {
						allDead = false
						break
					}
				}
				if allDead {
					canRemove = true
					removed = true
					pcMap[pc] = -1
				}
			}
		}
		if !canRemove {
			pcMap[pc] = len(newCode)
			newCode = append(newCode, ins)
		}
	}
	if !removed {
		return false
	}
	// map removed PCs to the next valid instruction
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

// constFold performs simple constant folding and branch elimination.
// It returns true if any instruction was modified.
func constFold(fn *Function) bool {
	type cinfo struct {
		known bool
		val   Value
	}
	changed := false
	consts := make([]cinfo, fn.NumRegs)
	defCount := make([]int, fn.NumRegs)
	for _, ins := range fn.Code {
		for _, r := range defRegs(ins) {
			if r >= 0 && r < len(defCount) {
				defCount[r]++
			}
		}
	}
	for pc := 0; pc < len(fn.Code); pc++ {
		ins := fn.Code[pc]
		switch ins.Op {
		case OpConst:
			if defCount[ins.A] == 1 {
				consts[ins.A] = cinfo{true, ins.Val}
			} else {
				consts[ins.A] = cinfo{}
			}
		case OpMove:
			if v := consts[ins.B]; v.known {
				if defCount[ins.A] == 1 {
					consts[ins.A] = v
				} else {
					consts[ins.A] = cinfo{}
				}
			} else {
				consts[ins.A] = cinfo{}
			}
		case OpNeg, OpNegInt, OpNegFloat, OpNot:
			b := consts[ins.B]
			if b.known {
				if val, ok := evalUnaryConst(ins.Op, b.val); ok {
					fn.Code[pc] = Instr{Op: OpConst, A: ins.A, Val: val, Line: ins.Line}
					if defCount[ins.A] == 1 {
						consts[ins.A] = cinfo{true, val}
					} else {
						consts[ins.A] = cinfo{}
					}
					changed = true
					continue
				}
			}
			consts[ins.A] = cinfo{}
		case OpAdd, OpSub, OpMul, OpDiv, OpMod,
			OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt,
			OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat,
			OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
			OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat:
			b := consts[ins.B]
			c := consts[ins.C]
			if b.known && c.known {
				if val, ok := evalBinaryConst(ins.Op, b.val, c.val); ok {
					fn.Code[pc] = Instr{Op: OpConst, A: ins.A, Val: val, Line: ins.Line}
					if defCount[ins.A] == 1 {
						consts[ins.A] = cinfo{true, val}
					} else {
						consts[ins.A] = cinfo{}
					}
					changed = true
					continue
				}
			}
			consts[ins.A] = cinfo{}
		case OpJumpIfFalse, OpJumpIfTrue:
			cond := consts[ins.A]
			if cond.known && cond.val.Tag == interpreter.TagBool {
				take := (ins.Op == OpJumpIfFalse && !cond.val.Bool) ||
					(ins.Op == OpJumpIfTrue && cond.val.Bool)
				if take {
					fn.Code[pc].Op = OpJump
					fn.Code[pc].A = ins.B
				} else {
					fn.Code[pc].Op = OpJump
					fn.Code[pc].A = pc + 1
				}
				changed = true
			}
		default:
			for _, r := range defRegs(ins) {
				if r >= 0 && r < len(consts) {
					consts[r] = cinfo{}
				}
			}
		}
	}
	return changed
}

// pruneRedundantJumps removes unconditional jumps that target the next instruction.
func pruneRedundantJumps(fn *Function) bool {
	removed := false
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	for pc, ins := range fn.Code {
		if ins.Op == OpJump && ins.A == pc+1 {
			pcMap[pc] = -1
			removed = true
			continue
		}
		pcMap[pc] = len(newCode)
		newCode = append(newCode, ins)
	}
	if !removed {
		return false
	}
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

func evalUnaryConst(op Op, v Value) (Value, bool) {
	switch op {
	case OpNeg, OpNegInt:
		if v.Tag == interpreter.TagFloat {
			return Value{Tag: interpreter.TagFloat, Float: -toFloat(v)}, true
		}
		if v.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagInt, Int: -v.Int}, true
		}
	case OpNegFloat:
		if v.Tag == interpreter.TagFloat || v.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagFloat, Float: -toFloat(v)}, true
		}
	case OpNot:
		if v.Tag == interpreter.TagBool {
			return Value{Tag: interpreter.TagBool, Bool: !v.Bool}, true
		}
	}
	return Value{}, false
}

func evalBinaryConst(op Op, b, c Value) (Value, bool) {
	switch op {
	case OpAddInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagInt, Int: b.Int + c.Int}, true
		}
	case OpSubInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagInt, Int: b.Int - c.Int}, true
		}
	case OpMulInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagInt, Int: b.Int * c.Int}, true
		}
	case OpDivInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt && c.Int != 0 {
			return Value{Tag: interpreter.TagInt, Int: b.Int / c.Int}, true
		}
	case OpModInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt && c.Int != 0 {
			return Value{Tag: interpreter.TagInt, Int: b.Int % c.Int}, true
		}
	case OpAddFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagFloat, Float: toFloat(b) + toFloat(c)}, true
		}
	case OpSubFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagFloat, Float: toFloat(b) - toFloat(c)}, true
		}
	case OpMulFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagFloat, Float: toFloat(b) * toFloat(c)}, true
		}
	case OpDivFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) && toFloat(c) != 0 {
			return Value{Tag: interpreter.TagFloat, Float: toFloat(b) / toFloat(c)}, true
		}
	case OpModFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) && toFloat(c) != 0 {
			return Value{Tag: interpreter.TagFloat, Float: math.Mod(toFloat(b), toFloat(c))}, true
		}
	case OpAdd:
		if b.Tag == interpreter.TagStr && c.Tag == interpreter.TagStr {
			return Value{Tag: interpreter.TagStr, Str: b.Str + c.Str}, true
		}
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				return Value{Tag: interpreter.TagFloat, Float: toFloat(b) + toFloat(c)}, true
			}
			return Value{Tag: interpreter.TagInt, Int: b.Int + c.Int}, true
		}
	case OpSub:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				return Value{Tag: interpreter.TagFloat, Float: toFloat(b) - toFloat(c)}, true
			}
			return Value{Tag: interpreter.TagInt, Int: b.Int - c.Int}, true
		}
	case OpMul:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				return Value{Tag: interpreter.TagFloat, Float: toFloat(b) * toFloat(c)}, true
			}
			return Value{Tag: interpreter.TagInt, Int: b.Int * c.Int}, true
		}
	case OpDiv:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) && toFloat(c) != 0 {
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				return Value{Tag: interpreter.TagFloat, Float: toFloat(b) / toFloat(c)}, true
			}
			return Value{Tag: interpreter.TagInt, Int: b.Int / c.Int}, true
		}
	case OpMod:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) && toFloat(c) != 0 {
			if b.Tag == interpreter.TagFloat || c.Tag == interpreter.TagFloat {
				return Value{Tag: interpreter.TagFloat, Float: math.Mod(toFloat(b), toFloat(c))}, true
			}
			return Value{Tag: interpreter.TagInt, Int: b.Int % c.Int}, true
		}
	case OpEqualInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagBool, Bool: b.Int == c.Int}, true
		}
	case OpEqualFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagBool, Bool: toFloat(b) == toFloat(c)}, true
		}
	case OpEqual:
		if b.Tag == c.Tag {
			switch b.Tag {
			case interpreter.TagInt:
				return Value{Tag: interpreter.TagBool, Bool: b.Int == c.Int}, true
			case interpreter.TagFloat:
				return Value{Tag: interpreter.TagBool, Bool: b.Float == c.Float}, true
			case interpreter.TagStr:
				return Value{Tag: interpreter.TagBool, Bool: b.Str == c.Str}, true
			case interpreter.TagBool:
				return Value{Tag: interpreter.TagBool, Bool: b.Bool == c.Bool}, true
			}
		}
	case OpNotEqual:
		if v, ok := evalBinaryConst(OpEqual, b, c); ok {
			v.Bool = !v.Bool
			return v, true
		}
	case OpLessInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagBool, Bool: b.Int < c.Int}, true
		}
	case OpLessEqInt:
		if b.Tag == interpreter.TagInt && c.Tag == interpreter.TagInt {
			return Value{Tag: interpreter.TagBool, Bool: b.Int <= c.Int}, true
		}
	case OpLessFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagBool, Bool: toFloat(b) < toFloat(c)}, true
		}
	case OpLessEqFloat:
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagBool, Bool: toFloat(b) <= toFloat(c)}, true
		}
	case OpLess:
		if b.Tag == interpreter.TagStr && c.Tag == interpreter.TagStr {
			return Value{Tag: interpreter.TagBool, Bool: b.Str < c.Str}, true
		}
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagBool, Bool: toFloat(b) < toFloat(c)}, true
		}
	case OpLessEq:
		if b.Tag == interpreter.TagStr && c.Tag == interpreter.TagStr {
			return Value{Tag: interpreter.TagBool, Bool: b.Str <= c.Str}, true
		}
		if (b.Tag == interpreter.TagFloat || b.Tag == interpreter.TagInt) &&
			(c.Tag == interpreter.TagFloat || c.Tag == interpreter.TagInt) {
			return Value{Tag: interpreter.TagBool, Bool: toFloat(b) <= toFloat(c)}, true
		}
	}
	return Value{}, false
}
