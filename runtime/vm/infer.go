package vm

// Type inference across a function's control flow graph. It propagates simple
// register type tags beyond basic blocks to enable later optimizations.

// typeInfo holds type data for each program point. In[i] is the register tags
// at the start of instruction i, Out[i] after executing i.
type typeInfo struct {
	In  [][]regTag
	Out [][]regTag
}

// inferTypes performs a forward data flow analysis to determine register type
// tags for the given function.
func inferTypes(fn *Function) *typeInfo {
	n := len(fn.Code)
	info := &typeInfo{
		In:  make([][]regTag, n),
		Out: make([][]regTag, n),
	}

	// helper functions
	copyTags := func(tags []regTag) []regTag {
		out := make([]regTag, len(tags))
		copy(out, tags)
		return out
	}

	mergeInto := func(dst []regTag, src []regTag) bool {
		changed := false
		for i := range dst {
			s := src[i]
			d := dst[i]
			if s == tagUnknown || s == d {
				continue
			}
			if d == tagUnknown {
				dst[i] = s
				changed = true
			} else if d != s {
				dst[i] = tagUnknown
				changed = true
			}
		}
		return changed
	}

	work := []int{0}
	info.In[0] = make([]regTag, fn.NumRegs)

	for len(work) > 0 {
		pc := work[0]
		work = work[1:]
		in := info.In[pc]
		out := applyInstr(in, fn.Code[pc])
		if info.Out[pc] == nil || mergeInto(info.Out[pc], out) {
			info.Out[pc] = copyTags(out)
		}

		addEdge := func(next int, tags []regTag) {
			if next < 0 || next >= n {
				return
			}
			if info.In[next] == nil {
				info.In[next] = copyTags(tags)
				work = append(work, next)
				return
			}
			if mergeInto(info.In[next], tags) {
				work = append(work, next)
			}
		}

		ins := fn.Code[pc]
		switch ins.Op {
		case OpJump:
			addEdge(ins.A, out)
		case OpJumpIfFalse, OpJumpIfTrue:
			addEdge(ins.B, out)
			addEdge(pc+1, out)
		case OpReturn:
			// no successor
		default:
			addEdge(pc+1, out)
		}
	}

	return info
}

// applyInstr updates register tags according to the given instruction.
func applyInstr(in []regTag, ins Instr) []regTag {
	out := make([]regTag, len(in))
	copy(out, in)
	switch ins.Op {
	case OpConst:
		out[ins.A] = valTag(ins.Val)
	case OpMove:
		out[ins.A] = in[ins.B]
	case OpAddInt, OpSubInt, OpMulInt, OpDivInt, OpModInt:
		out[ins.A] = tagInt
	case OpAddFloat, OpSubFloat, OpMulFloat, OpDivFloat, OpModFloat:
		out[ins.A] = tagFloat
	case OpAdd, OpSub, OpMul, OpDiv, OpMod:
		if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
			out[ins.A] = tagFloat
		} else if in[ins.B] == tagInt && in[ins.C] == tagInt {
			out[ins.A] = tagInt
		} else {
			out[ins.A] = tagUnknown
		}
	case OpEqual, OpNotEqual, OpEqualInt, OpEqualFloat,
		OpLess, OpLessEq, OpLessInt, OpLessFloat, OpLessEqInt, OpLessEqFloat,
		OpIn, OpNot:
		out[ins.A] = tagBool
	case OpLen, OpNow:
		out[ins.A] = tagInt
	}
	return out
}

// optimizeFunc rewrites generic operations with specialized ones based on the
// inferred type information.
func optimizeFunc(fn *Function, info *typeInfo) {
	for pc, ins := range fn.Code {
		in := info.In[pc]
		switch ins.Op {
		case OpAdd:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpAddInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpAddFloat
			}
		case OpSub:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpSubInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpSubFloat
			}
		case OpMul:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpMulInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpMulFloat
			}
		case OpDiv:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpDivInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpDivFloat
			}
		case OpMod:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpModInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpModFloat
			}
		case OpEqual:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpEqualInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpEqualFloat
			}
		case OpLess:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpLessInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpLessFloat
			}
		case OpLessEq:
			if in[ins.B] == tagInt && in[ins.C] == tagInt {
				fn.Code[pc].Op = OpLessEqInt
			} else if in[ins.B] == tagFloat || in[ins.C] == tagFloat {
				fn.Code[pc].Op = OpLessEqFloat
			}
		}
	}
}

// optimizeProgram performs type inference and rewrites on all functions in the program.
func optimizeProgram(p *Program) {
	for i := range p.Funcs {
		info := inferTypes(&p.Funcs[i])
		optimizeFunc(&p.Funcs[i], info)
	}
}
