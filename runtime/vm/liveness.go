package vm

import (
	"encoding/base64"
	"fmt"
	"math"
	"sort"
	"strings"
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
		OpIn, OpUnionAll, OpUnion, OpExcept, OpIntersect:
		addDef(ins.A)
		addUse(ins.B)
		addUse(ins.C)
	case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpFirst, OpExists,
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
		OpIn, OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpFirst, OpExists,
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

func usesReg(ins Instr, r int) bool {
	return ins.A == r || ins.B == r || ins.C == r || ins.D == r
}

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

// dedupConst replaces repeated Const instructions with moves from the first
// register holding the constant value.
func dedupConst(fn *Function) bool {
	// Disabled to avoid replacing constants defined inside conditional blocks
	// which may lead to uninitialized register values.
	return false
}

// Optimize removes dead instructions with no side effects.
func Optimize(fn *Function) {
	for {
		changed := constFold(fn)
		if dedupConst(fn) {
			changed = true
		}
		analysis := Liveness(fn)
		_ = analysis
		// Peephole optimization disabled to avoid incorrect register
		// propagation across branches.
		// if peephole(fn, analysis) {
		//         changed = true
		//         analysis = Liveness(fn)
		// }
		pruneRedundantJumps(fn)
		if foldJumpChains(fn) {
			changed = true
		}
		analysis = Liveness(fn)
		removed := false // disable dead code removal for now
		unreachable := removeUnreachable(fn)
		if unreachable {
			changed = true
		}
		if !removed && !changed && !unreachable {
			break
		}
	}
	// Compact registers to reduce the overall register count. The
	// allocator currently struggles with overlapping lifetimes when
	// loops are nested.  Disable register compaction for now to avoid
	// incorrect register reuse.
	// TODO: fix CompactRegisters and re-enable once the allocator
	// handles lifetimes correctly.
	// CompactRegisters(fn)
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

// removeUnreachable deletes instructions that can never be executed based on
// control flow analysis.
func removeUnreachable(fn *Function) bool {
	analysis := Infer(fn)
	if analysis == nil || len(analysis.Dead) == 0 {
		return false
	}
	removed := false
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))
	for pc, ins := range fn.Code {
		if pc < len(analysis.Dead) && analysis.Dead[pc] {
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
	// map removed PCs to next valid instruction for jump patching
	next := len(newCode)
	for i := len(pcMap) - 1; i >= 0; i-- {
		if pcMap[i] == -1 {
			pcMap[i] = next
		} else {
			next = pcMap[i]
		}
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

// peephole applies simple instruction-level optimizations like
// eliminating identity moves and folding const+move pairs.
func peephole(fn *Function, analysis *LiveInfo) bool {
	changed := false
	pcMap := make([]int, len(fn.Code))
	newCode := make([]Instr, 0, len(fn.Code))

	for pc := 0; pc < len(fn.Code); pc++ {
		ins := fn.Code[pc]

		// remove moves where src == dst
		if ins.Op == OpMove && ins.A == ins.B {
			changed = true
			pcMap[pc] = -1
			continue
		}

		// fold Const followed by Move when the source register is dead
		if ins.Op == OpConst && pc+1 < len(fn.Code) {
			next := fn.Code[pc+1]
			if next.Op == OpMove && next.B == ins.A && !analysis.Out[pc+1][ins.A] {
				newCode = append(newCode, Instr{Op: OpConst, A: next.A, Val: ins.Val, Line: next.Line})
				pcMap[pc] = len(newCode) - 1
				pcMap[pc+1] = -1
				changed = true
				pc++
				continue
			}
		}

		// collapse chained moves: Move r1,r2 ; Move r3,r1 -> Move r3,r2
		if ins.Op == OpMove && pc+1 < len(fn.Code) {
			next := fn.Code[pc+1]
			if next.Op == OpMove && next.B == ins.A && !analysis.Out[pc+1][ins.A] {
				next.B = ins.B
				fn.Code[pc+1] = next
				changed = true
				pcMap[pc] = -1
				continue
			}
			if next.Op == OpReturn && next.A == ins.A && !analysis.Out[pc+1][ins.A] {
				next.A = ins.B
				fn.Code[pc+1] = next
				changed = true
				pcMap[pc] = -1
				continue
			}
			// propagate move into the next instruction when the temp register
			// is not live afterwards. Avoid range-based ops where register order matters.
			switch next.Op {
			case OpMakeList, OpMakeMap, OpCall, OpCallV, OpMakeClosure, OpPrintN:
				// skip propagation for these
			default:
				if next.A != ins.A && usesReg(next, ins.A) && !analysis.Out[pc+1][ins.A] {
					replaceReg(&next, ins.A, ins.B)
					fn.Code[pc+1] = next
					changed = true
					pcMap[pc] = -1
					continue
				}
			}
		} else if pc+1 < len(fn.Code) {
			// eliminate x; Move rY,x -> x with destination rY when x defines a register
			next := fn.Code[pc+1]
			if next.Op == OpMove && next.B == ins.A && !analysis.Out[pc+1][ins.A] {
				use, _ := useDef(ins, fn.NumRegs)
				if !use[ins.A] {
					ins.A = next.A
					pcMap[pc] = len(newCode)
					newCode = append(newCode, ins)
					pcMap[pc+1] = -1
					changed = true
					pc++
					continue
				}
			}
		}

		// simplify operations with identity constants immediately preceding
		if pc > 0 {
			prev := fn.Code[pc-1]
			// helper returns true if prev defines the given reg and is dead after pc
			isConst := func(reg int, val Value) bool {
				if prev.Op != OpConst || prev.A != reg {
					return false
				}
				if prev.Val.Tag != val.Tag {
					return false
				}
				switch val.Tag {
				case ValueInt:
					if prev.Val.Int != val.Int {
						return false
					}
				case ValueFloat:
					if prev.Val.Float != val.Float {
						return false
					}
				case ValueBool:
					if prev.Val.Bool != val.Bool {
						return false
					}
				case ValueStr:
					if prev.Val.Str != val.Str {
						return false
					}
				default:
					return false
				}
				return !analysis.In[pc][reg] && !analysis.Out[pc][reg]
			}
			switch ins.Op {
			case OpAddInt, OpAddFloat, OpAdd:
				if isConst(ins.B, (Value{Tag: ValueInt, Int: 0})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.C, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				} else if isConst(ins.C, (Value{Tag: ValueInt, Int: 0})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				}
			case OpSubInt, OpSubFloat, OpSub:
				if isConst(ins.C, (Value{Tag: ValueInt, Int: 0})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				}
			case OpMulInt, OpMulFloat, OpMul:
				if isConst(ins.B, (Value{Tag: ValueInt, Int: 1})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.C, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				} else if isConst(ins.C, (Value{Tag: ValueInt, Int: 1})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				} else if isConst(ins.B, (Value{Tag: ValueInt, Int: 0})) || isConst(ins.C, (Value{Tag: ValueInt, Int: 0})) {
					ins = Instr{Op: OpConst, A: ins.A, Val: Value{Tag: ValueInt, Int: 0}, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				}
			case OpDivInt, OpDivFloat, OpDiv:
				if isConst(ins.C, (Value{Tag: ValueInt, Int: 1})) {
					ins = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
					changed = true
					pcMap[pc-1] = -1
				}
			}
		}

		pcMap[pc] = len(newCode)
		newCode = append(newCode, ins)
	}
	if !changed {
		return false
	}
	next := len(newCode)
	for i := len(pcMap) - 1; i >= 0; i-- {
		if pcMap[i] == -1 {
			pcMap[i] = next
		} else {
			next = pcMap[i]
		}
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
		case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpUpper, OpLower, OpB64Encode, OpB64Decode, OpFirst, OpLen,
			OpCount, OpExists, OpAvg, OpSum, OpMin, OpMax, OpValues, OpSort:
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
			OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
			OpAppend, OpUnionAll, OpUnion, OpExcept, OpIntersect,
			OpIndex:
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
		case OpSlice:
			src := consts[ins.B]
			start := consts[ins.C]
			end := consts[ins.D]
			if src.known && start.known && end.known {
				if val, ok := evalSliceConst(src.val, start.val, end.val); ok {
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
			if cond.known && cond.val.Tag == ValueBool {
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

// foldJumpChains rewrites jumps that target other jumps to jump directly to the
// final destination. Returns true if any jump was updated.
func foldJumpChains(fn *Function) bool {
	changed := false
	follow := func(pc int) int {
		seen := map[int]bool{}
		for pc >= 0 && pc < len(fn.Code) {
			if seen[pc] {
				break
			}
			seen[pc] = true
			ins := fn.Code[pc]
			if ins.Op != OpJump || ins.A == pc {
				break
			}
			pc = ins.A
		}
		return pc
	}
	for i := range fn.Code {
		ins := &fn.Code[i]
		switch ins.Op {
		case OpJump:
			dst := follow(ins.A)
			if dst != ins.A {
				ins.A = dst
				changed = true
			}
		case OpJumpIfFalse, OpJumpIfTrue:
			dst := follow(ins.B)
			if dst != ins.B {
				ins.B = dst
				changed = true
			}
		}
	}
	return changed
}

func evalUnaryConst(op Op, v Value) (Value, bool) {
	switch op {
	case OpNeg, OpNegInt:
		if v.Tag == ValueFloat {
			return Value{Tag: ValueFloat, Float: -toFloat(v)}, true
		}
		if v.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: -v.Int}, true
		}
	case OpNegFloat:
		if v.Tag == ValueFloat || v.Tag == ValueInt {
			return Value{Tag: ValueFloat, Float: -toFloat(v)}, true
		}
	case OpNot:
		if v.Tag == ValueBool {
			return Value{Tag: ValueBool, Bool: !v.Bool}, true
		}
	case OpStr:
		return Value{Tag: ValueStr, Str: fmt.Sprint(v.ToAny())}, true
	case OpUpper:
		if v.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: strings.ToUpper(v.Str)}, true
		}
	case OpLower:
		if v.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: strings.ToLower(v.Str)}, true
		}
		return Value{Tag: ValueStr, Str: strings.ToLower(fmt.Sprint(v.ToAny()))}, true
	case OpB64Encode:
		if v.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: base64.StdEncoding.EncodeToString([]byte(v.Str))}, true
		}
	case OpB64Decode:
		if v.Tag == ValueStr {
			data, err := base64.StdEncoding.DecodeString(v.Str)
			if err == nil {
				return Value{Tag: ValueStr, Str: string(data)}, true
			}
		}
	case OpFirst:
		if lst, ok := toList(v); ok {
			if len(lst) > 0 {
				return lst[0], true
			}
			return Value{Tag: ValueNull}, true
		}
	case OpLen:
		switch v.Tag {
		case ValueList:
			return Value{Tag: ValueInt, Int: len(v.List)}, true
		case ValueStr:
			return Value{Tag: ValueInt, Int: len([]rune(v.Str))}, true
		case ValueMap:
			return Value{Tag: ValueInt, Int: len(v.Map)}, true
		}
	case OpCount:
		if lst, ok := toList(v); ok {
			return Value{Tag: ValueInt, Int: len(lst)}, true
		}
		if v.Tag == ValueMap {
			return Value{Tag: ValueInt, Int: len(v.Map)}, true
		}
		if v.Tag == ValueStr {
			return Value{Tag: ValueInt, Int: len([]rune(v.Str))}, true
		}
	case OpExists:
		if lst, ok := toList(v); ok {
			return Value{Tag: ValueBool, Bool: len(lst) > 0}, true
		}
		switch v.Tag {
		case ValueList:
			return Value{Tag: ValueBool, Bool: len(v.List) > 0}, true
		case ValueMap:
			return Value{Tag: ValueBool, Bool: len(v.Map) > 0}, true
		case ValueStr:
			return Value{Tag: ValueBool, Bool: len(v.Str) > 0}, true
		}
	case OpAvg:
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			var sum float64
			for _, it := range lst {
				sum += toFloat(it)
			}
			return Value{Tag: ValueFloat, Float: sum / float64(len(lst))}, true
		}
	case OpSum:
		if lst, ok := toList(v); ok {
			allInt := true
			var sumF float64
			var sumI int
			for _, it := range lst {
				if it.Tag == ValueInt {
					sumI += it.Int
				} else {
					allInt = false
					sumF += toFloat(it)
				}
			}
			if allInt {
				return Value{Tag: ValueInt, Int: sumI}, true
			}
			sumF += float64(sumI)
			return Value{Tag: ValueFloat, Float: sumF}, true
		}
	case OpMin:
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			if lst[0].Tag == ValueStr {
				minStr := lst[0].Str
				for _, it := range lst[1:] {
					if it.Tag == ValueStr && it.Str < minStr {
						minStr = it.Str
					}
				}
				return Value{Tag: ValueStr, Str: minStr}, true
			}
			minVal := toFloat(lst[0])
			isFloat := lst[0].Tag == ValueFloat
			for _, it := range lst[1:] {
				if it.Tag == ValueFloat {
					isFloat = true
				}
				f := toFloat(it)
				if f < minVal {
					minVal = f
				}
			}
			if isFloat {
				return Value{Tag: ValueFloat, Float: minVal}, true
			}
			return Value{Tag: ValueInt, Int: int(minVal)}, true
		}
	case OpMax:
		if lst, ok := toList(v); ok {
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
			if lst[0].Tag == ValueStr {
				maxStr := lst[0].Str
				for _, it := range lst[1:] {
					if it.Tag == ValueStr && it.Str > maxStr {
						maxStr = it.Str
					}
				}
				return Value{Tag: ValueStr, Str: maxStr}, true
			}
			maxVal := toFloat(lst[0])
			isFloat := lst[0].Tag == ValueFloat
			for _, it := range lst[1:] {
				if it.Tag == ValueFloat {
					isFloat = true
				}
				f := toFloat(it)
				if f > maxVal {
					maxVal = f
				}
			}
			if isFloat {
				return Value{Tag: ValueFloat, Float: maxVal}, true
			}
			return Value{Tag: ValueInt, Int: int(maxVal)}, true
		}
	case OpValues:
		if v.Tag == ValueMap {
			keys := make([]string, 0, len(v.Map))
			for k := range v.Map {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			vals := make([]Value, len(keys))
			for i, k := range keys {
				vals[i] = v.Map[k]
			}
			return Value{Tag: ValueList, List: vals}, true
		}
	case OpSort:
		if v.Tag == ValueList {
			pairs := append([]Value(nil), v.List...)
			sort.SliceStable(pairs, func(i, j int) bool {
				return valueLess(pairs[i].List[0], pairs[j].List[0])
			})
			out := make([]Value, len(pairs))
			for i, p := range pairs {
				if len(p.List) > 1 {
					out[i] = p.List[1]
				} else {
					out[i] = Value{Tag: ValueNull}
				}
			}
			return Value{Tag: ValueList, List: out}, true
		}
	}
	return Value{}, false
}

func evalBinaryConst(op Op, b, c Value) (Value, bool) {
	switch op {
	case OpAddInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: b.Int + c.Int}, true
		}
	case OpSubInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: b.Int - c.Int}, true
		}
	case OpMulInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueInt, Int: b.Int * c.Int}, true
		}
	case OpDivInt:
		if b.Tag == ValueInt && c.Tag == ValueInt && c.Int != 0 {
			return Value{Tag: ValueInt, Int: b.Int / c.Int}, true
		}
	case OpModInt:
		if b.Tag == ValueInt && c.Tag == ValueInt && c.Int != 0 {
			return Value{Tag: ValueInt, Int: b.Int % c.Int}, true
		}
	case OpAddFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}, true
		}
	case OpSubFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}, true
		}
	case OpMulFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}, true
		}
	case OpDivFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) && toFloat(c) != 0 {
			return Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}, true
		}
	case OpModFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) && toFloat(c) != 0 {
			return Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}, true
		}
	case OpAdd:
		if b.Tag == ValueStr && c.Tag == ValueStr {
			return Value{Tag: ValueStr, Str: b.Str + c.Str}, true
		}
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				return Value{Tag: ValueFloat, Float: toFloat(b) + toFloat(c)}, true
			}
			return Value{Tag: ValueInt, Int: b.Int + c.Int}, true
		}
	case OpSub:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				return Value{Tag: ValueFloat, Float: toFloat(b) - toFloat(c)}, true
			}
			return Value{Tag: ValueInt, Int: b.Int - c.Int}, true
		}
	case OpMul:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				return Value{Tag: ValueFloat, Float: toFloat(b) * toFloat(c)}, true
			}
			return Value{Tag: ValueInt, Int: b.Int * c.Int}, true
		}
	case OpDiv:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) && toFloat(c) != 0 {
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				return Value{Tag: ValueFloat, Float: toFloat(b) / toFloat(c)}, true
			}
			return Value{Tag: ValueInt, Int: b.Int / c.Int}, true
		}
	case OpMod:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) && toFloat(c) != 0 {
			if b.Tag == ValueFloat || c.Tag == ValueFloat {
				return Value{Tag: ValueFloat, Float: math.Mod(toFloat(b), toFloat(c))}, true
			}
			return Value{Tag: ValueInt, Int: b.Int % c.Int}, true
		}
	case OpAppend:
		if b.Tag == ValueList {
			out := append(append([]Value(nil), b.List...), c)
			return Value{Tag: ValueList, List: out}, true
		}
	case OpUnionAll:
		if b.Tag == ValueList && c.Tag == ValueList {
			out := append(append([]Value(nil), b.List...), c.List...)
			return Value{Tag: ValueList, List: out}, true
		}
	case OpUnion:
		if b.Tag == ValueList && c.Tag == ValueList {
			seen := make(map[string]struct{}, len(b.List)+len(c.List))
			out := make([]Value, 0, len(b.List)+len(c.List))
			for _, v := range b.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			for _, v := range c.List {
				k := valueToString(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			return Value{Tag: ValueList, List: out}, true
		}
	case OpExcept:
		if b.Tag == ValueList && c.Tag == ValueList {
			set := make(map[string]struct{}, len(c.List))
			for _, v := range c.List {
				set[valueToString(v)] = struct{}{}
			}
			diff := make([]Value, 0, len(b.List))
			for _, v := range b.List {
				if _, ok := set[valueToString(v)]; !ok {
					diff = append(diff, v)
				}
			}
			return Value{Tag: ValueList, List: diff}, true
		}
	case OpIntersect:
		if b.Tag == ValueList && c.Tag == ValueList {
			setA := make(map[string]struct{}, len(b.List))
			for _, v := range b.List {
				setA[valueToString(v)] = struct{}{}
			}
			inter := []Value{}
			added := make(map[string]struct{}, len(c.List))
			for _, v := range c.List {
				k := valueToString(v)
				if _, ok := setA[k]; ok {
					if _, done := added[k]; !done {
						added[k] = struct{}{}
						inter = append(inter, v)
					}
				}
			}
			return Value{Tag: ValueList, List: inter}, true
		}
	case OpEqualInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueBool, Bool: b.Int == c.Int}, true
		}
	case OpEqualFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueBool, Bool: toFloat(b) == toFloat(c)}, true
		}
	case OpEqual:
		if b.Tag == c.Tag {
			switch b.Tag {
			case ValueInt:
				return Value{Tag: ValueBool, Bool: b.Int == c.Int}, true
			case ValueFloat:
				return Value{Tag: ValueBool, Bool: b.Float == c.Float}, true
			case ValueStr:
				return Value{Tag: ValueBool, Bool: b.Str == c.Str}, true
			case ValueBool:
				return Value{Tag: ValueBool, Bool: b.Bool == c.Bool}, true
			}
		}
	case OpNotEqual:
		if v, ok := evalBinaryConst(OpEqual, b, c); ok {
			v.Bool = !v.Bool
			return v, true
		}
	case OpLessInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueBool, Bool: b.Int < c.Int}, true
		}
	case OpLessEqInt:
		if b.Tag == ValueInt && c.Tag == ValueInt {
			return Value{Tag: ValueBool, Bool: b.Int <= c.Int}, true
		}
	case OpLessFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueBool, Bool: toFloat(b) < toFloat(c)}, true
		}
	case OpLessEqFloat:
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueBool, Bool: toFloat(b) <= toFloat(c)}, true
		}
	case OpLess:
		if b.Tag == ValueStr && c.Tag == ValueStr {
			return Value{Tag: ValueBool, Bool: b.Str < c.Str}, true
		}
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueBool, Bool: toFloat(b) < toFloat(c)}, true
		}
	case OpLessEq:
		if b.Tag == ValueStr && c.Tag == ValueStr {
			return Value{Tag: ValueBool, Bool: b.Str <= c.Str}, true
		}
		if (b.Tag == ValueFloat || b.Tag == ValueInt) &&
			(c.Tag == ValueFloat || c.Tag == ValueInt) {
			return Value{Tag: ValueBool, Bool: toFloat(b) <= toFloat(c)}, true
		}
	case OpIndex:
		return evalIndexConst(b, c)
	}
	return Value{}, false
}

func toList(v Value) ([]Value, bool) {
	switch v.Tag {
	case ValueList:
		return v.List, true
	case ValueMap:
		if flag, ok := v.Map["__group__"]; ok && flag.Tag == ValueBool && flag.Bool {
			items := v.Map["items"]
			if items.Tag == ValueList {
				return items.List, true
			}
		}
	}
	return nil, false
}

func evalIndexConst(container, idx Value) (Value, bool) {
	switch container.Tag {
	case ValueList:
		if idx.Tag != ValueInt {
			return Value{}, false
		}
		n := len(container.List)
		i := idx.Int
		if i < 0 {
			i += n
		}
		if i < 0 || i >= n {
			return Value{Tag: ValueNull}, true
		}
		return container.List[i], true
	case ValueMap:
		var key string
		switch idx.Tag {
		case ValueStr:
			key = idx.Str
		case ValueInt:
			key = fmt.Sprintf("%d", idx.Int)
		default:
			return Value{Tag: ValueNull}, true
		}
		if v, ok := container.Map[key]; ok {
			return v, true
		}
		return Value{Tag: ValueNull}, true
	case ValueStr:
		if idx.Tag != ValueInt {
			return Value{}, false
		}
		runes := []rune(container.Str)
		i := idx.Int
		if i < 0 {
			i += len(runes)
		}
		if i < 0 || i >= len(runes) {
			return Value{Tag: ValueNull}, true
		}
		return Value{Tag: ValueStr, Str: string(runes[i])}, true
	}
	return Value{}, false
}

func evalSliceConst(src, startVal, endVal Value) (Value, bool) {
	switch src.Tag {
	case ValueList:
		n := len(src.List)
		start := 0
		if startVal.Tag != ValueNull {
			if startVal.Tag != ValueInt {
				return Value{}, false
			}
			start = startVal.Int
			if start < 0 {
				start += n
			}
		}
		end := n
		if endVal.Tag != ValueNull {
			if endVal.Tag != ValueInt {
				return Value{}, false
			}
			end = endVal.Int
			if end < 0 {
				end += n
			}
		}
		if start < 0 || end > n || start > end {
			return Value{}, false
		}
		out := make([]Value, end-start)
		copy(out, src.List[start:end])
		return Value{Tag: ValueList, List: out}, true
	case ValueStr:
		runes := []rune(src.Str)
		n := len(runes)
		start := 0
		if startVal.Tag != ValueNull {
			if startVal.Tag != ValueInt {
				return Value{}, false
			}
			start = startVal.Int
			if start < 0 {
				start += n
			}
		}
		end := n
		if endVal.Tag != ValueNull {
			if endVal.Tag != ValueInt {
				return Value{}, false
			}
			end = endVal.Int
			if end < 0 {
				end += n
			}
		}
		if start < 0 || end > n || start > end {
			return Value{}, false
		}
		return Value{Tag: ValueStr, Str: string(runes[start:end])}, true
	}
	return Value{}, false
}
