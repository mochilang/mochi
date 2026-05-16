package tracejit

import (
	"fmt"

	"mochi/runtime/jit/tmpljit"
)

// recordIteration steps the interpreter starting at headerPC for
// exactly one loop iteration and returns the recorded body. The
// iteration ends at the first OpJnz whose target equals headerPC
// (a back-edge closing the loop). Any other control-flow (forward
// jumps, OpRet) before the back-edge is treated as an abort.
//
// The recorder runs in lockstep with the real interpreter so the
// captured Body is exactly the instructions that executed for this
// iteration; no speculation, no inlining. For the FillSumProgram
// shape (a straight-line do-while), this is the entire body.
func recordIteration(p tmpljit.Program, regs *[tmpljit.NumRegs]int64, headerPC int) (*Trace, error) {
	pc := headerPC
	var body []tmpljit.Instr
	for {
		if pc < 0 || pc >= len(p) {
			return nil, fmt.Errorf("tracejit: recorder ran off program at pc=%d", pc)
		}
		ins := p[pc]
		body = append(body, ins)
		nextPC := pc + 1
		switch ins.Op {
		case tmpljit.OpMovImm:
			regs[ins.Dst] = int64(ins.Imm)
		case tmpljit.OpAdd:
			regs[ins.Dst] = regs[ins.A] + regs[ins.B]
		case tmpljit.OpMul:
			regs[ins.Dst] = regs[ins.A] * regs[ins.B]
		case tmpljit.OpLt:
			if regs[ins.A] < regs[ins.B] {
				regs[ins.Dst] = 1
			} else {
				regs[ins.Dst] = 0
			}
		case tmpljit.OpJnz:
			target := nextPC + int(ins.Imm)
			taken := regs[ins.A] != 0
			if taken && target == headerPC {
				// Loop-closing back-edge: trace is complete.
				return &Trace{
					HeaderPC: headerPC,
					ExitPC:   nextPC, // where interp resumes on guard miss
					Body:     body,
					GuardReg: ins.A,
				}, nil
			}
			if taken && target != headerPC {
				return nil, fmt.Errorf("tracejit: inner back-edge to %d (not header %d) not supported", target, headerPC)
			}
			// Not taken: the loop would have exited mid-trace.
			// Treat as a recording abort; the simple prototype
			// only records iterations where the back-edge fires.
			return nil, fmt.Errorf("tracejit: back-edge not taken during recording")
		case tmpljit.OpRet:
			return nil, fmt.Errorf("tracejit: OpRet during recording at pc=%d", pc)
		default:
			return nil, fmt.Errorf("tracejit: unknown opcode %d during recording", ins.Op)
		}
		pc = nextPC
	}
}
