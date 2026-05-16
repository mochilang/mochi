package vm2

import "errors"

// Run executes Program.Main and returns the final Cell. Objects is
// reset before dispatch; callers that pre-populate it (e.g. tests
// loading boxed wide ints) must do so after Run starts via a hook in
// a later step.
func (vm *VM) Run() (Cell, error) {
	vm.Objects = vm.Objects[:0]
	main := vm.Program.Funcs[vm.Program.Main]
	fr := vm.acquireFrame(main)
	var ret Cell
	for {
		ins := &fr.Fn.Code[fr.IP]
		fr.IP++
		switch ins.Op {
		case OpLoadConstI:
			fr.Regs[ins.A] = fr.Fn.Consts[ins.B]
		case OpMove:
			fr.Regs[ins.A] = fr.Regs[ins.B]
		case OpAddI64:
			fr.Regs[ins.A] = CInt(fr.Regs[ins.B].Int() + fr.Regs[ins.C].Int())
		case OpSubI64:
			fr.Regs[ins.A] = CInt(fr.Regs[ins.B].Int() - fr.Regs[ins.C].Int())
		case OpMulI64:
			fr.Regs[ins.A] = CInt(fr.Regs[ins.B].Int() * fr.Regs[ins.C].Int())
		case OpDivI64:
			d := fr.Regs[ins.C].Int()
			if d == 0 {
				return ret, errors.New("vm2: division by zero")
			}
			fr.Regs[ins.A] = CInt(fr.Regs[ins.B].Int() / d)
		case OpModI64:
			d := fr.Regs[ins.C].Int()
			if d == 0 {
				return ret, errors.New("vm2: mod by zero")
			}
			fr.Regs[ins.A] = CInt(fr.Regs[ins.B].Int() % d)
		case OpLessI64:
			fr.Regs[ins.A] = CBool(fr.Regs[ins.B].Int() < fr.Regs[ins.C].Int())
		case OpLessEqI64:
			fr.Regs[ins.A] = CBool(fr.Regs[ins.B].Int() <= fr.Regs[ins.C].Int())
		case OpEqualI64:
			fr.Regs[ins.A] = CBool(fr.Regs[ins.B].Int() == fr.Regs[ins.C].Int())
		case OpJump:
			fr.IP = int(ins.A)
		case OpJumpIfFalse:
			if !fr.Regs[ins.A].Bool() {
				fr.IP = int(ins.B)
			}
		case OpCall:
			callee := vm.Program.Funcs[ins.B]
			newFr := vm.acquireFrame(callee)
			n := int(ins.D)
			copy(newFr.Regs[:n], fr.Regs[ins.C:int(ins.C)+n])
			newFr.RetReg = ins.A
			newFr.Parent = fr
			fr = newFr
		case OpTailCall:
			callee := vm.Program.Funcs[ins.A]
			n := int(ins.C)
			// Snapshot args before overwriting any register that
			// might alias the destination slots.
			var buf [16]Cell
			var args []Cell
			if n <= len(buf) {
				args = buf[:n]
			} else {
				args = make([]Cell, n)
			}
			copy(args, fr.Regs[ins.B:int(ins.B)+n])
			if callee == fr.Fn {
				// Same function: reuse frame in place.
				copy(fr.Regs[:n], args)
				fr.IP = 0
			} else {
				// Different function: swap frame contents.
				parent := fr.Parent
				retReg := fr.RetReg
				vm.releaseFrame(fr)
				newFr := vm.acquireFrame(callee)
				copy(newFr.Regs[:n], args)
				newFr.RetReg = retReg
				newFr.Parent = parent
				fr = newFr
			}
		case OpReturn:
			ret = fr.Regs[ins.A]
			retReg := fr.RetReg
			parent := fr.Parent
			vm.releaseFrame(fr)
			if parent == nil {
				return ret, nil
			}
			parent.Regs[retReg] = ret
			fr = parent
		case OpHalt:
			return ret, errors.New("vm2: OpHalt reached")
		default:
			return ret, errors.New("vm2: unknown opcode")
		}
	}
}
