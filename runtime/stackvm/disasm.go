package stackvm

import (
	"fmt"
	"strings"
)

// Disassemble returns a human readable listing of the program instructions.
// It is modeled after runtime/vm.Program.Disassemble but supports the
// simplified stack based instruction set used by this VM.
func (p *Program) Disassemble(src string) string {
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
		fmt.Fprintf(&b, "func %s (vars=%d)\n", name, fn.NumVars)

		labels := map[int]string{}
		next := 0
		for pc, ins := range fn.Code {
			switch ins.Op {
			case OpJump, OpJumpIfFalse:
				if _, ok := labels[ins.A]; !ok {
					labels[ins.A] = fmt.Sprintf("L%d", next)
					next++
				}
			}
			_ = pc
		}

		for pc, ins := range fn.Code {
			if lbl, ok := labels[pc]; ok {
				fmt.Fprintf(&b, "%s:\n", lbl)
			}
			fmt.Fprintf(&b, "  %-12s ", ins.Op)
			switch ins.Op {
			case OpPushConst:
				fmt.Fprintf(&b, "%s", formatValue(ins.Val))
			case OpLoad, OpStore, OpJump, OpReturn:
				fmt.Fprintf(&b, "%d", ins.A)
			case OpJumpIfFalse:
				if lbl, ok := labels[ins.A]; ok {
					fmt.Fprintf(&b, "%s", lbl)
				} else {
					fmt.Fprintf(&b, "%d", ins.A)
				}
			case OpCall:
				if ins.A < 0 {
					builtin := ""
					switch -ins.A {
					case 1:
						builtin = "print"
					case 2:
						builtin = "len"
					}
					if builtin != "" {
						fmt.Fprintf(&b, "%s %d", builtin, ins.B)
					} else {
						fmt.Fprintf(&b, "%d %d", ins.A, ins.B)
					}
				} else {
					callee := p.Funcs[ins.A].Name
					if callee == "" {
						if ins.A == 0 {
							callee = "main"
						} else {
							callee = fmt.Sprintf("fn%d", ins.A)
						}
					}
					fmt.Fprintf(&b, "%s %d", callee, ins.B)
				}
			case OpMakeList:
				fmt.Fprintf(&b, "%d", ins.A)
			case OpPop:
				// no args
			case OpPrint:
				// no args
			default:
				// ops like Add/Sub/etc have no operands
			}
			b.WriteByte('\n')
			_ = pc
		}
		b.WriteByte('\n')
	}
	out := b.String()
	out = strings.TrimRight(out, "\n")
	out += "\n"
	return out
}
