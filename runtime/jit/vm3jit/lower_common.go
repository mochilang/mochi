package vm3jit

import "mochi/runtime/vm3"

// Status codes the JIT writes through *(status word ptr) on a deopt
// path. Caller checks this word after a trampoline.CallStatus return
// and routes to the corresponding vm3 error if non-zero.
const (
	StatusOK        int64 = 0
	StatusDivByZero int64 = 1
)

// popcount32 counts set bits in v. Used to size OpCallI64 spill loops
// on every backend.
func popcount32(v uint32) int {
	v = v - ((v >> 1) & 0x55555555)
	v = (v & 0x33333333) + ((v >> 2) & 0x33333333)
	v = (v + (v >> 4)) & 0x0F0F0F0F
	return int((v * 0x01010101) >> 24)
}

// liveSuccUnion is the live-in union across the successors of pc i.
// Branch opcodes have two successors (fall-through and op.C target);
// Jump has one (op.C); Return* have none; everything else falls
// through to i+1. Used by both lower_arm64 and lower_amd64 backward
// liveness passes.
func liveSuccUnion(fn *vm3.Function, i int, liveIn []uint32) uint32 {
	op := fn.Code[i]
	switch op.Code {
	case vm3.OpReturnI64, vm3.OpReturnConstK, vm3.OpReturnF64:
		return 0
	case vm3.OpJump:
		t := int(uint16(op.C))
		if t < 0 || t > len(fn.Code) {
			return 0
		}
		return liveIn[t]
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br,
		vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		t := int(uint16(op.C))
		if t < 0 || t > len(fn.Code) {
			return liveIn[i+1]
		}
		return liveIn[i+1] | liveIn[t]
	default:
		if i+1 <= len(fn.Code) {
			return liveIn[i+1]
		}
		return 0
	}
}

// defUseI64 returns (defs, uses) bitmasks for op over fn's i64 register
// bank. Bit r is set iff register r is defined/used. For OpCallI64 the
// uses cover op.B..op.B+nArgs-1.
func defUseI64(fn *vm3.Function, op vm3.Op) (uint32, uint32) {
	a := uint32(1) << op.A
	b := uint32(1) << op.B
	c := uint32(1) << uint16(op.C)
	switch op.Code {
	case vm3.OpConstI64K, vm3.OpConstI64KW:
		return a, 0
	case vm3.OpMovI64:
		return a, b
	case vm3.OpAddI64, vm3.OpSubI64, vm3.OpMulI64,
		vm3.OpDivI64, vm3.OpModI64:
		return a, b | c
	case vm3.OpNegI64:
		return a, b
	case vm3.OpAddI64K, vm3.OpSubI64K, vm3.OpMulI64K,
		vm3.OpDivI64K, vm3.OpModI64K:
		return a, b
	case vm3.OpCmpEqI64Br, vm3.OpCmpNeI64Br,
		vm3.OpCmpLtI64Br, vm3.OpCmpLeI64Br,
		vm3.OpCmpGtI64Br, vm3.OpCmpGeI64Br:
		return 0, a | b
	case vm3.OpCmpEqI64KBr, vm3.OpCmpNeI64KBr,
		vm3.OpCmpLtI64KBr, vm3.OpCmpLeI64KBr,
		vm3.OpCmpGtI64KBr, vm3.OpCmpGeI64KBr:
		return 0, a
	case vm3.OpJump:
		return 0, 0
	case vm3.OpReturnI64:
		return 0, a
	case vm3.OpReturnConstK:
		return 0, 0
	case vm3.OpCallI64:
		var uses uint32
		n := fn.NumI64Params()
		for k := 0; k < n; k++ {
			uses |= 1 << (op.B + uint16(k))
		}
		return a, uses
	}
	return 0, 0
}
