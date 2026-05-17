package vm2

import (
	"errors"
	"io"
	"math"
	"unsafe"
)

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
	vm.Stack = vm.Stack[:0]
	vm.Frames = vm.Frames[:0]

	// Materialize string-constant Cells per function. Short literals
	// pack inline; long literals allocate a *vmString once and the
	// resulting Cell is cached so OpLoadStrK is a slice read on the hot
	// path. The Go GC traces the pointee through Cell.Obj.
	for _, fn := range vm.Program.Funcs {
		if len(fn.StrCells) != len(fn.StrConsts) {
			fn.StrCells = make([]Cell, len(fn.StrConsts))
		}
		for i, b := range fn.StrConsts {
			if len(b) <= MaxInlineStr {
				fn.StrCells[i] = CSStr(b)
			} else {
				fn.StrCells[i] = vm.newString(b)
			}
		}
	}

	main := vm.Program.Funcs[vm.Program.Main]
	vm.pushFrame(main, 0)
	return vm.runLoop(0)
}

// RunTopFrame resumes the interpreter at the current top frame's IP and
// runs until that frame pops (i.e., until len(vm.Frames) drops to its
// pre-call value). Used by the vm2jit deopt-resume wrapper: when a JIT
// function exits with a deopt sentinel, the wrapper pushes the JIT
// function's frame, sets IP to the deopt PC, copies the JIT register
// state back into vm.Stack, then calls RunTopFrame to finish the
// function on the interpreter. The returned Cell is the value of the
// function's OpReturn.
func (vm *VM) RunTopFrame() (Cell, error) {
	return vm.runLoop(len(vm.Frames) - 1)
}

// runLoop is the interpreter's inner dispatch. It runs until the frame
// indexed by target pops (i.e., until len(vm.Frames) <= target). For
// Run() target is 0 (no frames left means main returned); for
// RunTopFrame the target is the depth at which the JIT-resumed frame
// sits, so the loop returns the moment that specific frame's OpReturn
// fires.
func (vm *VM) runLoop(target int) (Cell, error) {
	fr := &vm.Frames[len(vm.Frames)-1]
	code := fr.Fn.Code
	regs := vm.Stack[fr.RegsBase:]
	consts := fr.Fn.Consts
	ip := fr.IP
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
			fr.IP = ip
			argSrc := int(ins.C)
			// Route to JIT when available. JITCallFn handles the full
			// frame protocol; on return we reload hot locals because
			// vm.Stack may have grown during the call.
			if callee.JITCode != nil && JITCallFn != nil {
				regs[ins.A] = JITCallFn(vm, callee, fr.RegsBase+argSrc, n, ins.A)
				reload()
				break
			}
			// Snapshot args before the push: pushFrame can grow
			// vm.Stack and move the backing array, so we cannot read
			// the source slice through the old regs view afterwards.
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
			if len(vm.Frames) <= target {
				return ret, nil
			}
			fr = &vm.Frames[len(vm.Frames)-1]
			code = fr.Fn.Code
			regs = vm.Stack[fr.RegsBase:]
			consts = fr.Fn.Consts
			ip = fr.IP
			regs[retReg] = ret
		case OpLoadStrK:
			regs[ins.A] = fr.Fn.StrCells[ins.B]
		case OpConcatStr:
			ca, cb := regs[ins.B], regs[ins.C]
			// Inline + inline with combined len <= 5: pack directly,
			// no allocation. Concat_loop hits this for the first few
			// iterations and stays in inline space for all results
			// whose final length fits.
			if ca.IsSStr() && cb.IsSStr() {
				la, lb := ca.SStrLen(), cb.SStrLen()
				if la+lb <= MaxInlineStr {
					pa := ca.Bits & sstrByteMask
					pb := cb.Bits & sstrByteMask
					packed := pa | (pb << (uint(la) * 8))
					regs[ins.A] = Cell{Bits: tagSStr | uint64(la+lb)<<sstrLenShift | packed}
					break
				}
			}
			var abuf, bbuf [MaxInlineStr]byte
			ab := vm.strBytes(ca, &abuf)
			bb := vm.strBytes(cb, &bbuf)
			out := make([]byte, len(ab)+len(bb))
			copy(out, ab)
			copy(out[len(ab):], bb)
			regs[ins.A] = vm.makeStr(out)
		case OpLenStr:
			regs[ins.A] = CInt(int64(vm.strLen(regs[ins.B])))
		case OpIndexStr:
			c := regs[ins.B]
			i := regs[ins.C].Int()
			n := int64(vm.strLen(c))
			if i < 0 || i >= n {
				fr.IP = ip
				return ret, errors.New("vm2: string index out of range")
			}
			var buf [MaxInlineStr]byte
			bs := vm.strBytes(c, &buf)
			regs[ins.A] = CSStr([]byte{bs[i]})
		case OpEqualStr:
			regs[ins.A] = CBool(vm.strEqualCell(regs[ins.B], regs[ins.C]))
		case OpHashStr:
			regs[ins.A] = CInt(int64(vm.strHashCell(regs[ins.B])))
		case OpNewList:
			regs[ins.A] = vm.newList(int(ins.B))
		case OpListLen:
			regs[ins.A] = CInt(int64(len(vm.listAt(regs[ins.B]).data)))
		case OpListGet:
			l := vm.listAt(regs[ins.B])
			i := regs[ins.C].Int()
			if i < 0 || i >= int64(len(l.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: list index out of range")
			}
			regs[ins.A] = l.data[i]
		case OpListSet:
			l := vm.listAt(regs[ins.A])
			i := regs[ins.B].Int()
			if i < 0 || i >= int64(len(l.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: list index out of range")
			}
			l.data[i] = regs[ins.C]
		case OpListPush:
			l := vm.listAt(regs[ins.A])
			l.data = append(l.data, regs[ins.B])
		case OpListAppend:
			// Functional append. emit sets InstrFlagBLastUse when the
			// source list operand is statically dead after this read; in
			// that case we mutate the source and reuse its pointer,
			// skipping the per-call backing-array copy. Otherwise we
			// allocate a fresh *vmList so callers that still observe the
			// source see it unchanged.
			if ins.Flags&InstrFlagBLastUse != 0 {
				src := regs[ins.B]
				l := vm.listAt(src)
				l.data = append(l.data, regs[ins.C])
				regs[ins.A] = src
			} else {
				regs[ins.A] = vm.listAppendCopy(regs[ins.B], regs[ins.C])
			}
		case OpNewMap:
			regs[ins.A] = vm.newMap()
		case OpMapLen:
			regs[ins.A] = CInt(int64(len(vm.mapAt(regs[ins.B]).entries)))
		case OpMapGet:
			m := vm.mapAt(regs[ins.B])
			if v, ok := m.entries[vm.mapKeyOf(regs[ins.C])]; ok {
				regs[ins.A] = v
			} else {
				regs[ins.A] = CNull()
			}
		case OpMapHas:
			m := vm.mapAt(regs[ins.B])
			_, ok := m.entries[vm.mapKeyOf(regs[ins.C])]
			regs[ins.A] = CBool(ok)
		case OpMapSet:
			m := vm.mapAt(regs[ins.A])
			m.entries[vm.mapKeyOf(regs[ins.B])] = regs[ins.C]
		case OpMapDel:
			m := vm.mapAt(regs[ins.A])
			delete(m.entries, vm.mapKeyOf(regs[ins.B]))
		case OpLoadConstF:
			regs[ins.A] = consts[ins.B]
		case OpAddF64:
			regs[ins.A] = CFloat(regs[ins.B].Float() + regs[ins.C].Float())
		case OpSubF64:
			regs[ins.A] = CFloat(regs[ins.B].Float() - regs[ins.C].Float())
		case OpMulF64:
			regs[ins.A] = CFloat(regs[ins.B].Float() * regs[ins.C].Float())
		case OpDivF64:
			regs[ins.A] = CFloat(regs[ins.B].Float() / regs[ins.C].Float())
		case OpNegF64:
			regs[ins.A] = CFloat(-regs[ins.B].Float())
		case OpAbsF64:
			regs[ins.A] = CFloat(math.Abs(regs[ins.B].Float()))
		case OpSqrtF64:
			regs[ins.A] = CFloat(math.Sqrt(regs[ins.B].Float()))
		case OpLessF64:
			regs[ins.A] = CBool(regs[ins.B].Float() < regs[ins.C].Float())
		case OpLessEqF64:
			regs[ins.A] = CBool(regs[ins.B].Float() <= regs[ins.C].Float())
		case OpEqualF64:
			regs[ins.A] = CBool(regs[ins.B].Float() == regs[ins.C].Float())
		case OpFmaF64:
			regs[ins.A] = CFloat(math.FMA(regs[ins.B].Float(), regs[ins.C].Float(), regs[ins.D].Float()))
		case OpI64ToF64:
			regs[ins.A] = CFloat(float64(regs[ins.B].Int()))
		case OpF64ToI64:
			regs[ins.A] = CInt(int64(regs[ins.B].Float()))
		case OpNewF64Array:
			regs[ins.A] = vm.newF64Array(int(regs[ins.B].Int()))
		case OpF64ArrLen:
			regs[ins.A] = CInt(int64(len(vm.f64ArrAt(regs[ins.B]).data)))
		case OpF64ArrGet:
			a := vm.f64ArrAt(regs[ins.B])
			i := regs[ins.C].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: f64 array index out of range")
			}
			regs[ins.A] = CFloat(a.data[i])
		case OpF64ArrSet:
			a := vm.f64ArrAt(regs[ins.A])
			i := regs[ins.B].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: f64 array index out of range")
			}
			a.data[i] = regs[ins.C].Float()
		case OpNewI64Array:
			regs[ins.A] = vm.newI64Array(int(regs[ins.B].Int()))
		case OpI64ArrLen:
			regs[ins.A] = CInt(int64(len(vm.i64ArrAt(regs[ins.B]).data)))
		case OpI64ArrGet:
			a := vm.i64ArrAt(regs[ins.B])
			i := regs[ins.C].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: i64 array index out of range")
			}
			regs[ins.A] = CInt(a.data[i])
		case OpI64ArrSet:
			a := vm.i64ArrAt(regs[ins.A])
			i := regs[ins.B].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: i64 array index out of range")
			}
			a.data[i] = regs[ins.C].Int()
		case OpNewU8Array:
			regs[ins.A] = vm.newU8Array(int(regs[ins.B].Int()))
		case OpU8ArrLen:
			regs[ins.A] = CInt(int64(len(vm.u8ArrAt(regs[ins.B]).data)))
		case OpU8ArrGet:
			a := vm.u8ArrAt(regs[ins.B])
			i := regs[ins.C].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: u8 array index out of range")
			}
			regs[ins.A] = CInt(int64(a.data[i]))
		case OpU8ArrSet:
			a := vm.u8ArrAt(regs[ins.A])
			i := regs[ins.B].Int()
			if i < 0 || i >= int64(len(a.data)) {
				fr.IP = ip
				return ret, errors.New("vm2: u8 array index out of range")
			}
			a.data[i] = byte(regs[ins.C].Int())
		case OpNewPair:
			regs[ins.A] = vm.newPair(regs[ins.B], regs[ins.C])
		case OpPairFst:
			regs[ins.A] = vm.pairAt(regs[ins.B]).a
		case OpPairSnd:
			regs[ins.A] = vm.pairAt(regs[ins.B]).b
		case OpBytesNew:
			regs[ins.A] = vm.newBytes(int(regs[ins.B].Int()))
		case OpBytesLen:
			regs[ins.A] = CInt(int64(vm.bytesAt(regs[ins.B]).n))
		case OpBytesGet:
			b := vm.bytesAt(regs[ins.B])
			i := regs[ins.C].Int()
			if i < 0 || i >= int64(b.n) {
				fr.IP = ip
				return ret, errors.New("vm2: bytes index out of range")
			}
			regs[ins.A] = CInt(int64(b.buf[b.off+int(i)]))
		case OpBytesSet:
			b := vm.bytesAt(regs[ins.A])
			if !b.owns {
				fr.IP = ip
				return ret, errors.New("vm2: bytes write to non-owning view")
			}
			i := regs[ins.B].Int()
			if i < 0 || i >= int64(b.n) {
				fr.IP = ip
				return ret, errors.New("vm2: bytes index out of range")
			}
			b.buf[b.off+int(i)] = byte(regs[ins.C].Int())
		case OpBytesSlice:
			src := vm.bytesAt(regs[ins.B])
			off := regs[ins.C].Int()
			n := regs[ins.D].Int()
			if off < 0 || n < 0 || off+n > int64(src.n) {
				fr.IP = ip
				return ret, errors.New("vm2: bytes slice out of range")
			}
			v := &vmBytes{buf: src.buf, off: src.off + int(off), n: int(n), owns: false}
			regs[ins.A] = Cell{Bits: tagPtr, Obj: unsafe.Pointer(v)}
		case OpBytesEqual:
			regs[ins.A] = vm.bytesEqual(regs[ins.B], regs[ins.C])
		case OpBytesHash:
			regs[ins.A] = vm.bytesHash(regs[ins.B])
		case OpBytesFromU8Array:
			regs[ins.A] = vm.bytesFromU8Array(regs[ins.B])
		case OpBytesFromStr:
			regs[ins.A] = vm.bytesFromStr(regs[ins.B])
		case OpStdoutWriteBytes:
			b := vm.bytesAt(regs[ins.A])
			if _, err := vm.Stdout.Write(b.buf[b.off : b.off+b.n]); err != nil {
				fr.IP = ip
				return ret, err
			}
		case OpStdinReadAll:
			data, err := io.ReadAll(vm.Stdin)
			if err != nil {
				fr.IP = ip
				return ret, err
			}
			bv := &vmBytes{buf: data, off: 0, n: len(data), owns: true}
			regs[ins.A] = Cell{Bits: tagPtr, Obj: unsafe.Pointer(bv)}
		case OpHalt:
			fr.IP = ip
			return ret, errors.New("vm2: OpHalt reached")
		default:
			fr.IP = ip
			return ret, errors.New("vm2: unknown opcode")
		}
	}
}
