package vm

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

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
		case OpNeg, OpNegInt, OpNegFloat, OpNot, OpStr, OpUpper, OpLower, OpFirst, OpLen,
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
		case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPow,
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
			} else {
				switch ins.Op {
				case OpAddInt, OpAddFloat, OpAdd:
					if b.known && isZeroValue(b.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.C, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.C]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
					if c.known && isZeroValue(c.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.B]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
				case OpSubInt, OpSubFloat, OpSub:
					if c.known && isZeroValue(c.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.B]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
					if b.known && isZeroValue(b.val) {
						var nop Op
						switch ins.Op {
						case OpSubInt:
							nop = OpNegInt
						case OpSubFloat:
							nop = OpNegFloat
						default:
							nop = OpNeg
						}
						fn.Code[pc] = Instr{Op: nop, A: ins.A, B: ins.C, Line: ins.Line}
						if val, ok := evalUnaryConst(nop, consts[ins.C].val); ok && defCount[ins.A] == 1 {
							consts[ins.A] = cinfo{true, val}
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
				case OpMulInt, OpMulFloat, OpMul:
					if b.known && isOneValue(b.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.C, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.C]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
					if c.known && isOneValue(c.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.B]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
					if (b.known && isZeroValue(b.val)) || (c.known && isZeroValue(c.val)) {
						zero := b.val
						if !isZeroValue(zero) {
							zero = c.val
						}
						fn.Code[pc] = Instr{Op: OpConst, A: ins.A, Val: zero, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = cinfo{true, zero}
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
				case OpDivInt, OpDivFloat, OpDiv:
					if c.known && isOneValue(c.val) {
						fn.Code[pc] = Instr{Op: OpMove, A: ins.A, B: ins.B, Line: ins.Line}
						if defCount[ins.A] == 1 {
							consts[ins.A] = consts[ins.B]
						} else {
							consts[ins.A] = cinfo{}
						}
						changed = true
						continue
					}
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
			if len(lst) == 0 {
				return Value{Tag: ValueInt, Int: 0}, true
			}
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
		if b.Tag == ValueList && len(b.List) == 0 {
			if c.Tag == ValueList {
				out := append([]Value(nil), c.List...)
				return Value{Tag: ValueList, List: out}, true
			}
		}
		if c.Tag == ValueList && len(c.List) == 0 {
			if b.Tag == ValueList {
				out := append([]Value(nil), b.List...)
				return Value{Tag: ValueList, List: out}, true
			}
		}
		if b.Tag == ValueList && c.Tag == ValueList {
			out := append(append([]Value(nil), b.List...), c.List...)
			return Value{Tag: ValueList, List: out}, true
		}
	case OpUnion:
		if b.Tag == ValueList && len(b.List) == 0 {
			if c.Tag == ValueList {
				out := append([]Value(nil), c.List...)
				return Value{Tag: ValueList, List: out}, true
			}
		}
		if c.Tag == ValueList && len(c.List) == 0 {
			if b.Tag == ValueList {
				out := append([]Value(nil), b.List...)
				return Value{Tag: ValueList, List: out}, true
			}
		}
		if b.Tag == ValueList && c.Tag == ValueList {
			seen := make(map[uint64]struct{}, len(b.List)+len(c.List))
			out := make([]Value, 0, len(b.List)+len(c.List))
			for _, v := range b.List {
				k := valueHash(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			for _, v := range c.List {
				k := valueHash(v)
				if _, ok := seen[k]; !ok {
					seen[k] = struct{}{}
					out = append(out, v)
				}
			}
			return Value{Tag: ValueList, List: out}, true
		}
	case OpExcept:
		if b.Tag == ValueList && len(b.List) == 0 {
			return Value{Tag: ValueList, List: []Value{}}, true
		}
		if c.Tag == ValueList && len(c.List) == 0 {
			if b.Tag == ValueList {
				out := append([]Value(nil), b.List...)
				return Value{Tag: ValueList, List: out}, true
			}
		}
		if b.Tag == ValueList && c.Tag == ValueList {
			set := make(map[uint64]struct{}, len(c.List))
			for _, v := range c.List {
				set[valueHash(v)] = struct{}{}
			}
			diff := make([]Value, 0, len(b.List))
			for _, v := range b.List {
				if _, ok := set[valueHash(v)]; !ok {
					diff = append(diff, v)
				}
			}
			return Value{Tag: ValueList, List: diff}, true
		}
	case OpIntersect:
		if (b.Tag == ValueList && len(b.List) == 0) || (c.Tag == ValueList && len(c.List) == 0) {
			return Value{Tag: ValueList, List: []Value{}}, true
		}
		if b.Tag == ValueList && c.Tag == ValueList {
			setA := make(map[uint64]struct{}, len(b.List))
			for _, v := range b.List {
				setA[valueHash(v)] = struct{}{}
			}
			inter := []Value{}
			added := make(map[uint64]struct{}, len(c.List))
			for _, v := range c.List {
				k := valueHash(v)
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

func isZeroValue(v Value) bool {
	switch v.Tag {
	case ValueInt:
		return v.Int == 0
	case ValueFloat:
		return v.Float == 0
	default:
		return false
	}
}

func isOneValue(v Value) bool {
	switch v.Tag {
	case ValueInt:
		return v.Int == 1
	case ValueFloat:
		return v.Float == 1
	default:
		return false
	}
}
