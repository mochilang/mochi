package tmpljit

// Interp runs Program p with arg as the value in register 0 and
// returns whatever register OpRet selects.
func Interp(p Program, arg int64) int64 {
	var regs [NumRegs]int64
	regs[0] = arg
	pc := 0
	for {
		ins := p[pc]
		pc++
		switch ins.Op {
		case OpMovImm:
			regs[ins.Dst] = int64(ins.Imm)
		case OpAdd:
			regs[ins.Dst] = regs[ins.A] + regs[ins.B]
		case OpMul:
			regs[ins.Dst] = regs[ins.A] * regs[ins.B]
		case OpLt:
			if regs[ins.A] < regs[ins.B] {
				regs[ins.Dst] = 1
			} else {
				regs[ins.Dst] = 0
			}
		case OpJnz:
			if regs[ins.A] != 0 {
				pc += int(ins.Imm)
			}
		case OpRet:
			return regs[ins.A]
		}
	}
}
