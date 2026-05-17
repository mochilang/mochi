package tieredjit

import "mochi/runtime/jit/tmpljit"

// optimize rewrites a tmpljit.Program into an optProgram with
// MovImm-then-{Add,Mul} pairs folded into AddImm / ShlImm /
// MulImm. Branch targets are recomputed: every original bytecode
// instruction either maps to one optProgram instr (1:1, simple
// translation) or two original instructions collapse into one
// (the MovImm is elided). The optimization keeps a parallel
// "kept[]" table so OpJnz immediates can be rewritten to land at
// the correct new index.
//
// Conservative dead-MovImm test: a MovImm into register r is dead
// if (a) the very next instruction consumes r as a source operand
// and (b) no subsequent instruction in the program reads r without
// first writing it. For the FillSumProgram pattern register r4
// gets reused as a scratch immediate slot across the loop, so each
// MovImm-Add or MovImm-Mul pair fits this shape exactly. Anything
// outside this shape falls through to the unoptimised path.
func optimize(p tmpljit.Program) optProgram {
	n := len(p)
	used := func(reg uint8, from int) bool {
		// Look forward from `from` until reg is written; if any read
		// occurs first, the MovImm is live.
		for i := from; i < n; i++ {
			ins := p[i]
			// Reads.
			switch ins.Op {
			case tmpljit.OpAdd, tmpljit.OpMul, tmpljit.OpLt:
				if ins.A == reg || ins.B == reg {
					return true
				}
			case tmpljit.OpJnz:
				if ins.A == reg {
					return true
				}
			case tmpljit.OpRet:
				if ins.A == reg {
					return true
				}
			}
			// Writes (kill the def).
			switch ins.Op {
			case tmpljit.OpMovImm, tmpljit.OpAdd, tmpljit.OpMul, tmpljit.OpLt:
				if ins.Dst == reg {
					return false
				}
			}
		}
		return false
	}

	// newIdx[i] = position in opt[] where original instruction i
	// begins (or where its consumer absorbed it, for folded
	// MovImms). Used to rewrite Jnz offsets.
	newIdx := make([]int, n+1)
	var opt optProgram
	skipNext := false
	for i := 0; i < n; i++ {
		newIdx[i] = len(opt)
		if skipNext {
			skipNext = false
			continue
		}
		ins := p[i]
		// Try to fold MovImm + (Add|Mul) at i, i+1.
		if ins.Op == tmpljit.OpMovImm && i+1 < n {
			nxt := p[i+1]
			c := ins.Imm
			r := ins.Dst
			// MovImm r, c ; Add d, a, b where (a == r XOR b == r).
			isAddPair := nxt.Op == tmpljit.OpAdd && ((nxt.A == r) != (nxt.B == r))
			isMulPair := nxt.Op == tmpljit.OpMul && ((nxt.A == r) != (nxt.B == r))
			pairOK := (isAddPair || isMulPair) && !used(r, i+2)
			if pairOK {
				// Identify the non-constant source.
				var src uint8
				if nxt.A == r {
					src = nxt.B
				} else {
					src = nxt.A
				}
				if isAddPair && c >= 0 && c <= 4095 {
					opt = append(opt, instr{op: t2AddImm, dst: nxt.Dst, a: src, imm: c})
					skipNext = true
					continue
				}
				if isMulPair {
					if c > 0 && (c&(c-1)) == 0 {
						// Power of two: emit a shift.
						sh := log2u32(uint32(c))
						opt = append(opt, instr{op: t2ShlImm, dst: nxt.Dst, a: src, imm: int32(sh)})
						skipNext = true
						continue
					}
					// Non power-of-two: still saves the MovImm by
					// using the existing mul, but we'd need a
					// reg-resident immediate; skip the fold so
					// codegen stays simple. (Production tier 2
					// would materialise the constant once per
					// function in a callee-saved reg.)
				}
			}
		}
		// 1:1 translation.
		switch ins.Op {
		case tmpljit.OpMovImm:
			opt = append(opt, instr{op: t2MovImm, dst: ins.Dst, imm: ins.Imm})
		case tmpljit.OpAdd:
			opt = append(opt, instr{op: t2Add, dst: ins.Dst, a: ins.A, b: ins.B})
		case tmpljit.OpMul:
			opt = append(opt, instr{op: t2Mul, dst: ins.Dst, a: ins.A, b: ins.B})
		case tmpljit.OpLt:
			opt = append(opt, instr{op: t2Lt, dst: ins.Dst, a: ins.A, b: ins.B})
		case tmpljit.OpJnz:
			// Placeholder; Imm rewritten in a second pass once
			// newIdx is complete.
			opt = append(opt, instr{op: t2Jnz, a: ins.A, imm: ins.Imm})
		case tmpljit.OpRet:
			opt = append(opt, instr{op: t2Ret, a: ins.A})
		}
	}
	newIdx[n] = len(opt)

	// Patch Jnz offsets: original was relative to the instruction
	// after the Jnz; rewrite as a delta in opt-space.
	for i, ins := range p {
		if ins.Op != tmpljit.OpJnz {
			continue
		}
		newJnzPos := newIdx[i]
		newTargetBC := i + 1 + int(ins.Imm)
		newTargetPos := newIdx[newTargetBC]
		off := int32(newTargetPos - (newJnzPos + 1))
		// Locate the t2Jnz in opt[] (it's at newJnzPos).
		opt[newJnzPos].imm = off
	}
	return opt
}

func log2u32(x uint32) uint32 {
	// caller guarantees x is a power of two and > 0.
	var n uint32
	for x > 1 {
		x >>= 1
		n++
	}
	return n
}
