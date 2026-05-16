package vm2

import "errors"

// Run executes Program.Main and returns the final Cell.
//
// Dispatch state (code, regs, ip) is hoisted into locals across the
// inner loop so the inner loop reads them from CPU registers rather
// than chasing the frame pointer through memory on every op. Locals
// are re-synced on every frame transition (Call / TailCall non-self /
// Return); same-fn tail calls only need an IP rewind. The stack-based
// frame model in vm.go means call/return are two integer bumps + a
// reslice — no sync.Pool round trips, which were 56% of fib_rec CPU
// before this refactor.
func (vm *VM) Run() (Cell, error) {
	vm.Objects = vm.Objects[:0]
	vm.Stack = vm.Stack[:0]
	vm.Frames = vm.Frames[:0]

	main := vm.Program.Funcs[vm.Program.Main]
	frIdx, base := vm.pushFrame(main, 0)
	fr := &vm.Frames[frIdx]
	code := fr.Fn.Code
	regs := vm.Stack[base:]
	consts := fr.Fn.Consts
	ip := 0
	var ret Cell

	// reload syncs hot locals from the current top frame after any
	// operation that swapped frames or grew vm.Stack (which can
	// invalidate the prior `regs` slice header).
	reload := func() {
		fr = &vm.Frames[len(vm.Frames)-1]
		code = fr.Fn.Code
		regs = vm.Stack[fr.RegsBase:]
		consts = fr.Fn.Consts
		ip = fr.IP
	}
	_ = reload

	for {
		ins := &code[ip]
		ip++
		switch ins.Op {
		case OpLoadConstI:
			regs[ins.A] = consts[ins.B]
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
			n := int(ins.D)
			// Save caller IP before pushFrame in case the push grows
			// vm.Stack and invalidates regs.
			fr.IP = ip
			// Snapshot args before the push: pushFrame can grow
			// vm.Stack and move the backing array, so we cannot read
			// the source slice through the old regs view afterwards.
			argSrc := int(ins.C)
			var argBuf [16]Cell
			var args []Cell
			if n <= len(argBuf) {
				args = argBuf[:n]
			} else {
				args = make([]Cell, n)
			}
			copy(args, regs[argSrc:argSrc+n])
			_, newBase := vm.pushFrame(callee, ins.A)
			copy(vm.Stack[newBase:newBase+n], args)
			fr = &vm.Frames[len(vm.Frames)-1]
			code = fr.Fn.Code
			regs = vm.Stack[newBase:]
			consts = fr.Fn.Consts
			ip = 0
		case OpTailCallSelf:
			ip = 0
		case OpTailCall:
			callee := vm.Program.Funcs[ins.A]
			n := int(ins.C)
			// Snapshot args before any stack mutation. Cross-fn tail
			// call: replace the current frame in place by shrinking
			// to its base, then pushing the new callee. RetReg and
			// parent are preserved by keeping len(Frames) unchanged
			// across the pop/push.
			argSrc := int(ins.B)
			var argBuf [16]Cell
			var args []Cell
			if n <= len(argBuf) {
				args = argBuf[:n]
			} else {
				args = make([]Cell, n)
			}
			copy(args, regs[argSrc:argSrc+n])
			retReg := fr.RetReg
			// Pop current frame's stack window (without popping the
			// Frames slot — we'll overwrite it). No zero-out: vm2 has
			// no ptr-tagged Cells yet; once boxed objects land this
			// must zero ptr slots before reuse.
			base := fr.RegsBase
			vm.Stack = vm.Stack[:base]
			// Grow if necessary for the callee.
			need := base + callee.NumRegs
			if cap(vm.Stack) < need {
				newCap := 2 * cap(vm.Stack)
				if newCap < need {
					newCap = need
				}
				ns := make([]Cell, len(vm.Stack), newCap)
				copy(ns, vm.Stack)
				vm.Stack = ns
			}
			vm.Stack = vm.Stack[:need]
			copy(vm.Stack[base:base+n], args)
			top := len(vm.Frames) - 1
			vm.Frames[top] = frame{Fn: callee, RegsBase: base, RetReg: retReg}
			fr = &vm.Frames[top]
			code = callee.Code
			regs = vm.Stack[base:]
			consts = callee.Consts
			ip = 0
		case OpReturn:
			ret = regs[ins.A]
			retReg := fr.RetReg
			vm.popFrame()
			if len(vm.Frames) == 0 {
				return ret, nil
			}
			fr = &vm.Frames[len(vm.Frames)-1]
			code = fr.Fn.Code
			regs = vm.Stack[fr.RegsBase:]
			consts = fr.Fn.Consts
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
