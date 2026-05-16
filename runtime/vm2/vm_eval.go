package vm2

import (
	"fmt"
	"io"
)

// Run executes function 0 of the program with no arguments. The
// dispatch loop uses an explicit *frame stack so OpCall/OpCallV push
// a new frame and OpReturn pops; no Go-level recursion through the
// interpreter. That keeps frame pooling effective and avoids Go
// stack growth on deeply recursive Mochi programs.
func (m *VM) Run() (Value, error) {
	return m.RunWith(nil)
}

// RunWith executes function 0 with the given args.
func (m *VM) RunWith(args []Value) (Value, error) {
	fn := &m.Prog.Funcs[0]
	f := m.acquireFrame(fn)
	off := m.copyGlobals(f)
	for i := 0; i < len(args) && off+i < len(f.Regs); i++ {
		f.Regs[off+i] = args[i]
	}
	return m.run(f)
}

// copyGlobals seeds the frame's first NumGlobals registers with the
// VM's globals and returns the offset where parameter regs begin.
func (m *VM) copyGlobals(f *frame) int {
	for i := 0; i < m.Prog.NumGlobals && i < len(f.Regs); i++ {
		f.Regs[i] = m.Globals[i]
	}
	return m.Prog.NumGlobals
}

// callReturn is a pending-return record on the dispatch stack:
// when the callee finishes its OpReturn, the returned value is
// written into the caller's frame at RetReg, the caller's IP is
// already advanced past the call, and the callee frame is recycled.
type callReturn struct {
	CallerFrame *frame
	RetReg      int32
}

// run is the dispatch loop. It owns a *frame stack and a parallel
// callReturn stack. Each OpCall(V) pushes both; each OpReturn pops.
func (m *VM) run(initial *frame) (Value, error) {
	stack := []*frame{initial}
	rets := []callReturn{{CallerFrame: nil, RetReg: 0}}

	var fr *frame
	var fn *Function
	var code []Instr

	loadTop := func() {
		fr = stack[len(stack)-1]
		fn = fr.Fn
		code = fn.Code
	}
	loadTop()

	for {
		ip := fr.IP
		if ip >= len(code) {
			// Implicit return at end of function: yield null.
			ret := VNull
			if rets[len(rets)-1].CallerFrame == nil {
				m.releaseFrame(fr)
				return ret, nil
			}
			r := rets[len(rets)-1]
			rets = rets[:len(rets)-1]
			m.releaseFrame(fr)
			stack = stack[:len(stack)-1]
			loadTop()
			fr.Regs[r.RetReg] = ret
			continue
		}
		ins := code[ip]
		fr.IP = ip + 1
		op := ins.Op
		if q := ins.Quick; q != 0 {
			op = Op(q)
		}
		switch op {
		case OpHalt:
			m.releaseFrame(fr)
			return VNull, nil
		case OpConst:
			fr.Regs[ins.A] = ins.Val
		case OpMove:
			fr.Regs[ins.A] = fr.Regs[ins.B]
		case OpAdd:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag == TagInt && c.Tag == TagInt {
				if r, ok := addOverflow(b.Num, c.Num); ok {
					fr.Regs[ins.A] = VInt64(r)
					tryQuicken(fn, ip, OpAdd_II)
					break
				}
			}
			if b.Tag == TagFloat || c.Tag == TagFloat {
				fr.Regs[ins.A] = VFloat(toFloat(b) + toFloat(c))
				break
			}
			if b.Tag == TagStr && c.Tag == TagStr {
				fr.Regs[ins.A] = VStr(b.AsStr() + c.AsStr())
				break
			}
			return VNull, fmt.Errorf("vm2: bad operands to Add")
		case OpAdd_II:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag != TagInt || c.Tag != TagInt {
				deoptQuicken(fn, ip)
				fr.IP = ip
				continue
			}
			if r, ok := addOverflow(b.Num, c.Num); ok {
				fr.Regs[ins.A] = VInt64(r)
				break
			}
			// Overflow: deopt to generic which knows the slow path.
			deoptQuicken(fn, ip)
			fr.IP = ip
			continue
		case OpAddInt:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			r, _ := addOverflow(b.Num, c.Num)
			fr.Regs[ins.A] = VInt64(r)
		case OpSub:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag == TagInt && c.Tag == TagInt {
				if r, ok := subOverflow(b.Num, c.Num); ok {
					fr.Regs[ins.A] = VInt64(r)
					tryQuicken(fn, ip, OpSub_II)
					break
				}
			}
			if b.Tag == TagFloat || c.Tag == TagFloat {
				fr.Regs[ins.A] = VFloat(toFloat(b) - toFloat(c))
				break
			}
			return VNull, fmt.Errorf("vm2: bad operands to Sub")
		case OpSub_II:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag != TagInt || c.Tag != TagInt {
				deoptQuicken(fn, ip)
				fr.IP = ip
				continue
			}
			if r, ok := subOverflow(b.Num, c.Num); ok {
				fr.Regs[ins.A] = VInt64(r)
				break
			}
			deoptQuicken(fn, ip)
			fr.IP = ip
			continue
		case OpSubInt:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			r, _ := subOverflow(b.Num, c.Num)
			fr.Regs[ins.A] = VInt64(r)
		case OpMul:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag == TagInt && c.Tag == TagInt {
				if r, ok := mulOverflow(b.Num, c.Num); ok {
					fr.Regs[ins.A] = VInt64(r)
					tryQuicken(fn, ip, OpMul_II)
					break
				}
			}
			if b.Tag == TagFloat || c.Tag == TagFloat {
				fr.Regs[ins.A] = VFloat(toFloat(b) * toFloat(c))
				break
			}
			return VNull, fmt.Errorf("vm2: bad operands to Mul")
		case OpMul_II:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag != TagInt || c.Tag != TagInt {
				deoptQuicken(fn, ip)
				fr.IP = ip
				continue
			}
			if r, ok := mulOverflow(b.Num, c.Num); ok {
				fr.Regs[ins.A] = VInt64(r)
				break
			}
			deoptQuicken(fn, ip)
			fr.IP = ip
			continue
		case OpMulInt:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			r, _ := mulOverflow(b.Num, c.Num)
			fr.Regs[ins.A] = VInt64(r)
		case OpLess:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag == TagInt && c.Tag == TagInt {
				fr.Regs[ins.A] = VBool(b.Num < c.Num)
				tryQuicken(fn, ip, OpLess_II)
				break
			}
			fr.Regs[ins.A] = VBool(toFloat(b) < toFloat(c))
		case OpLess_II:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag != TagInt || c.Tag != TagInt {
				deoptQuicken(fn, ip)
				fr.IP = ip
				continue
			}
			fr.Regs[ins.A] = VBool(b.Num < c.Num)
		case OpLessInt:
			fr.Regs[ins.A] = VBool(fr.Regs[ins.B].Num < fr.Regs[ins.C].Num)
		case OpLessEq:
			b := fr.Regs[ins.B]
			c := fr.Regs[ins.C]
			if b.Tag == TagInt && c.Tag == TagInt {
				fr.Regs[ins.A] = VBool(b.Num <= c.Num)
				break
			}
			fr.Regs[ins.A] = VBool(toFloat(b) <= toFloat(c))
		case OpEqual:
			fr.Regs[ins.A] = VBool(fr.Regs[ins.B].Equal(fr.Regs[ins.C]))
		case OpEqualInt:
			fr.Regs[ins.A] = VBool(fr.Regs[ins.B].Num == fr.Regs[ins.C].Num)
		case OpJump:
			fr.IP = int(ins.A)
		case OpJumpIfFalse:
			if !fr.Regs[ins.B].Truthy() {
				fr.IP = int(ins.A)
			}
		case OpJumpIfTrue:
			if fr.Regs[ins.B].Truthy() {
				fr.IP = int(ins.A)
			}
		case OpPrint:
			fmt.Fprintln(m.Writer, fr.Regs[ins.A].String())
		case OpMakeList:
			n := int(ins.B)
			start := int(ins.C)
			list := make([]Value, n)
			copy(list, fr.Regs[start:start+n])
			fr.Regs[ins.A] = VList(list)
		case OpAppend:
			xs := fr.Regs[ins.B].AsList()
			out := append(xs, fr.Regs[ins.C])
			fr.Regs[ins.A] = VList(out)
		case OpIndex:
			src := fr.Regs[ins.B]
			idx := fr.Regs[ins.C]
			if src.Tag == TagList {
				tryQuicken(fn, ip, OpIndex_List)
				xs := src.AsList()
				i := int(idx.Num)
				if i < 0 {
					i += len(xs)
				}
				if i < 0 || i >= len(xs) {
					return VNull, fmt.Errorf("vm2: list index out of range")
				}
				fr.Regs[ins.A] = xs[i]
				break
			}
			return VNull, fmt.Errorf("vm2: bad index source")
		case OpIndex_List:
			src := fr.Regs[ins.B]
			if src.Tag != TagList {
				deoptQuicken(fn, ip)
				fr.IP = ip
				continue
			}
			xs := src.AsList()
			i := int(fr.Regs[ins.C].Num)
			if i < 0 {
				i += len(xs)
			}
			if i < 0 || i >= len(xs) {
				return VNull, fmt.Errorf("vm2: list index out of range")
			}
			fr.Regs[ins.A] = xs[i]
		case OpLen:
			src := fr.Regs[ins.B]
			switch src.Tag {
			case TagList:
				fr.Regs[ins.A] = VInt(len(src.AsList()))
			case TagStr:
				fr.Regs[ins.A] = VInt(len(src.AsStr()))
			case TagMap:
				fr.Regs[ins.A] = VInt(len(src.AsMap()))
			default:
				return VNull, fmt.Errorf("vm2: bad operand to Len")
			}
		case OpMakeClosure:
			n := int(ins.C)
			caps := make([]Value, n)
			copy(caps, fr.Regs[ins.D:int(ins.D)+n])
			cl := &Closure{Fn: int(ins.B), Args: caps}
			fr.Regs[ins.A] = VFunc(cl)
		case OpCall:
			n := int(ins.C)
			callee := &m.Prog.Funcs[ins.B]
			newFr := m.acquireFrame(callee)
			off := m.copyGlobals(newFr)
			copy(newFr.Regs[off:off+n], fr.Regs[ins.D:int(ins.D)+n])
			rets = append(rets, callReturn{CallerFrame: fr, RetReg: ins.A})
			stack = append(stack, newFr)
			loadTop()
			continue
		case OpCallV:
			fnVal := fr.Regs[ins.B]
			if fnVal.Tag != TagFunc {
				return VNull, fmt.Errorf("vm2: CallV on non-func")
			}
			cl := fnVal.AsFunc()
			cs := siteFor(fn, ip)
			n := int(ins.C)
			captureLen, hit := cs.lookup(cl)
			if !hit {
				cs.observe(cl)
				captureLen = len(cl.Args)
			}
			callee := &m.Prog.Funcs[cl.Fn]
			newFr := m.acquireFrame(callee)
			off := m.copyGlobals(newFr)
			if captureLen > 0 {
				copy(newFr.Regs[off:off+captureLen], cl.Args)
			}
			copy(newFr.Regs[off+captureLen:off+captureLen+n], fr.Regs[ins.D:int(ins.D)+n])
			rets = append(rets, callReturn{CallerFrame: fr, RetReg: ins.A})
			stack = append(stack, newFr)
			loadTop()
			continue
		case OpReturn:
			ret := fr.Regs[ins.A]
			r := rets[len(rets)-1]
			rets = rets[:len(rets)-1]
			m.releaseFrame(fr)
			stack = stack[:len(stack)-1]
			if r.CallerFrame == nil {
				return ret, nil
			}
			loadTop()
			fr.Regs[r.RetReg] = ret
		default:
			return VNull, fmt.Errorf("vm2: unknown op %s", op)
		}
	}
}

// toFloat coerces v to a Go float64 for cross-typed arithmetic.
func toFloat(v Value) float64 {
	switch v.Tag {
	case TagInt:
		return float64(v.Num)
	case TagFloat:
		return v.AsFloat()
	case TagBool:
		if v.Num != 0 {
			return 1
		}
		return 0
	default:
		return 0
	}
}

// Discard is a convenience io.Writer for benchmarks that drop output.
func Discard() io.Writer { return discardWriter{} }

type discardWriter struct{}

func (discardWriter) Write(p []byte) (int, error) { return len(p), nil }
