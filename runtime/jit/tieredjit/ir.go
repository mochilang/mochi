package tieredjit

import "mochi/runtime/jit/tmpljit"

// Tier-2 IR. A small superset of the tmpljit bytecode with three
// extra opcodes that take an immediate operand directly. The
// optimizer rewrites the input Program into a sequence of these
// nodes; the backend emits one AArch64 instruction per node (modulo
// MovImm, which can still need two halfwords).
type opTier2 uint8

const (
	t2MovImm opTier2 = iota // dst = imm    (same as tmpljit)
	t2Add                   // dst = a + b  (same as tmpljit)
	t2Mul                   // dst = a * b  (same as tmpljit, kept for non-foldable cases)
	t2Lt                    // dst = (a < b) ? 1 : 0
	t2Jnz                   // pc += imm if regs[a] != 0
	t2Ret                   // return regs[a]

	// Tier-2 specific opcodes. The optimizer emits these in place
	// of MovImm-then-Add/Mul pairs that MEP-30 cannot fold.
	t2AddImm // dst = a + imm    (AArch64 ADD (immediate), 1 instr)
	t2ShlImm // dst = a << imm   (AArch64 LSL #n, 1 instr)
	t2MulImm // dst = a * imm    (still 1 mul instr, but no MovImm setup)
)

type instr struct {
	op       opTier2
	dst, a, b uint8
	imm      int32
}

// optProgram is the tier-2 IR: a flat instruction stream like
// tmpljit.Program but typed with opTier2.
type optProgram []instr

// lowerForReference converts an optProgram back into the same
// register-machine semantics as tmpljit, for testing.
func (op optProgram) interp(arg int64) int64 {
	var regs [tmpljit.NumRegs]int64
	regs[0] = arg
	pc := 0
	for {
		ins := op[pc]
		next := pc + 1
		switch ins.op {
		case t2MovImm:
			regs[ins.dst] = int64(ins.imm)
		case t2Add:
			regs[ins.dst] = regs[ins.a] + regs[ins.b]
		case t2Mul:
			regs[ins.dst] = regs[ins.a] * regs[ins.b]
		case t2Lt:
			if regs[ins.a] < regs[ins.b] {
				regs[ins.dst] = 1
			} else {
				regs[ins.dst] = 0
			}
		case t2Jnz:
			if regs[ins.a] != 0 {
				next = pc + 1 + int(ins.imm)
			}
		case t2Ret:
			return regs[ins.a]
		case t2AddImm:
			regs[ins.dst] = regs[ins.a] + int64(ins.imm)
		case t2ShlImm:
			regs[ins.dst] = regs[ins.a] << uint(ins.imm)
		case t2MulImm:
			regs[ins.dst] = regs[ins.a] * int64(ins.imm)
		}
		pc = next
	}
}
