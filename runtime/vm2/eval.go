package vm2

import "errors"

// Run executes Program.Main and returns the final Cell. Objects is
// reset before dispatch; callers that pre-populate it (e.g. tests
// loading boxed wide ints) must do so after Run starts via a hook in
// a later step.
//
// Hot dispatch state — the current frame's instruction stream, register
// slab, and IP — is hoisted into locals so the inner loop reads them
// from CPU registers rather than chasing fr.* through memory on every
// op. The locals are re-synced on frame transitions (Call / TailCall /
// Return). This is worth ~5–15% on tight integer loops because the
// alternative (`fr.Fn.Code[fr.IP]` + `fr.Regs[...]`) costs an extra
// pointer chase per opcode for every register touch.
func (vm *VM) Run() (Cell, error) {
	vm.Objects = vm.Objects[:0]
	main := vm.Program.Funcs[vm.Program.Main]
	fr := vm.acquireFrame(main)
	code := fr.Fn.Code
	regs := fr.Regs
	ip := 0
	var ret Cell
	for {
		ins := &code[ip]
		ip++
		switch ins.Op {
		case OpLoadConstI:
			regs[ins.A] = fr.Fn.Consts[ins.B]
		case OpMove:
			regs[ins.A] = regs[ins.B]
		case OpAddI64:
			regs[ins.A] = CInt(regs[ins.B].Int() + regs[ins.C].Int())
		case OpAddI64K:
			regs[ins.A] = CInt(regs[ins.B].Int() + int64(ins.C))
		case OpSubI64:
			regs[ins.A] = CInt(regs[ins.B].Int() - regs[ins.C].Int())
		case OpMulI64:
			regs[ins.A] = CInt(regs[ins.B].Int() * regs[ins.C].Int())
		case OpDivI64:
			d := regs[ins.C].Int()
			if d == 0 {
				fr.IP = ip
				return ret, errors.New("vm2: division by zero")
			}
			regs[ins.A] = CInt(regs[ins.B].Int() / d)
		case OpModI64:
			d := regs[ins.C].Int()
			if d == 0 {
				fr.IP = ip
				return ret, errors.New("vm2: mod by zero")
			}
			regs[ins.A] = CInt(regs[ins.B].Int() % d)
		case OpLessI64:
			regs[ins.A] = CBool(regs[ins.B].Int() < regs[ins.C].Int())
		case OpLessEqI64:
			regs[ins.A] = CBool(regs[ins.B].Int() <= regs[ins.C].Int())
		case OpEqualI64:
			regs[ins.A] = CBool(regs[ins.B].Int() == regs[ins.C].Int())
		case OpJump:
			ip = int(ins.A)
		case OpJumpIfFalse:
			if !regs[ins.A].Bool() {
				ip = int(ins.B)
			}
		case OpJumpIfLessI64:
			if regs[ins.A].Int() < regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpJumpIfLessEqI64:
			if regs[ins.A].Int() <= regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpJumpIfGreaterI64:
			if regs[ins.A].Int() > regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpJumpIfGreaterEqI64:
			if regs[ins.A].Int() >= regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpJumpIfEqualI64:
			if regs[ins.A].Int() == regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpJumpIfNotEqualI64:
			if regs[ins.A].Int() != regs[ins.B].Int() {
				ip = int(ins.C)
			}
		case OpCall:
			callee := vm.Program.Funcs[ins.B]
			newFr := vm.acquireFrame(callee)
			n := int(ins.D)
			copy(newFr.Regs[:n], regs[ins.C:int(ins.C)+n])
			newFr.RetReg = ins.A
			newFr.Parent = fr
			fr.IP = ip
			fr = newFr
			code = fr.Fn.Code
			regs = fr.Regs
			ip = 0
		case OpTailCallSelf:
			ip = 0
		case OpTailCall:
			callee := vm.Program.Funcs[ins.A]
			n := int(ins.C)
			// Args at regs[B..B+n) never alias params [0..n)
			// because emit.go always picks B = callBase = np +
			// ra.NumRegs, so B >= np >= n. Direct forward copy is
			// safe; no snapshot needed.
			if callee == fr.Fn {
				copy(regs[:n], regs[ins.B:int(ins.B)+n])
				ip = 0
			} else {
				parent := fr.Parent
				retReg := fr.RetReg
				newFr := vm.acquireFrame(callee)
				copy(newFr.Regs[:n], regs[ins.B:int(ins.B)+n])
				vm.releaseFrame(fr)
				newFr.RetReg = retReg
				newFr.Parent = parent
				fr = newFr
				code = fr.Fn.Code
				regs = fr.Regs
				ip = 0
			}
		case OpReturn:
			ret = regs[ins.A]
			retReg := fr.RetReg
			parent := fr.Parent
			vm.releaseFrame(fr)
			if parent == nil {
				return ret, nil
			}
			fr = parent
			code = fr.Fn.Code
			regs = fr.Regs
			ip = fr.IP
			regs[retReg] = ret
		case OpHalt:
			fr.IP = ip
			return ret, errors.New("vm2: OpHalt reached")
		default:
			fr.IP = ip
			return ret, errors.New("vm2: unknown opcode")
		}
	}
}
