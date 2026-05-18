package vm3

// OpCode is the vm3 bytecode opcode. The bank is encoded in the opcode
// mnemonic itself (e.g. OpAddI64 vs OpAddF64), so the interpreter
// never decides at runtime which bank to read.
//
// Phase 2 covers the subset needed by math kernels:
//   - typed arithmetic (i64, f64; both reg-reg and K-form)
//   - compare-and-branch (i64; both reg-reg and K-form)
//   - jump, move, const
//   - call, tail-call, return
//   - deopt sentinel (placeholder, JIT side fills in Phase 5)
type OpCode uint8

const (
	OpNop OpCode = iota

	// Mov / load const.
	OpMovI64
	OpMovF64
	OpConstI64K  // regsI64[A] = sign-extend(C as int16)
	OpConstF64K  // regsF64[A] = Function.Consts[C].Float()
	OpConstI64KW // regsI64[A] = Function.Consts[C].Int() (wide; pool lookup)

	// I64 arithmetic.
	OpAddI64   // regsI64[A] = regsI64[B] + regsI64[uint16(C)]
	OpSubI64   // regsI64[A] = regsI64[B] - regsI64[uint16(C)]
	OpMulI64   // regsI64[A] = regsI64[B] * regsI64[uint16(C)]
	OpDivI64   // regsI64[A] = regsI64[B] / regsI64[uint16(C)] (panics on /0)
	OpModI64   // regsI64[A] = regsI64[B] % regsI64[uint16(C)]
	OpNegI64   // regsI64[A] = -regsI64[B]
	OpAddI64K  // regsI64[A] = regsI64[B] + int16(C)
	OpSubI64K  // regsI64[A] = regsI64[B] - int16(C)
	OpMulI64K  // regsI64[A] = regsI64[B] * int16(C)
	OpDivI64K  // regsI64[A] = regsI64[B] / int16(C)
	OpModI64K  // regsI64[A] = regsI64[B] % int16(C)

	// F64 arithmetic.
	OpAddF64 // regsF64[A] = regsF64[B] + regsF64[uint16(C)]
	OpSubF64
	OpMulF64
	OpDivF64
	OpNegF64

	// I64 compare-and-branch. C carries target PC (uint16).
	OpCmpEqI64Br // if regsI64[A] == regsI64[B] jump to uint16(C)
	OpCmpNeI64Br
	OpCmpLtI64Br
	OpCmpLeI64Br
	OpCmpGtI64Br
	OpCmpGeI64Br

	// I64 compare-and-branch K-form. B carries the immediate (int16),
	// C carries the target PC (uint16). Encodes "compare reg to small
	// constant and branch" in one op.
	OpCmpEqI64KBr // if regsI64[A] == int16(B) jump to uint16(C)
	OpCmpNeI64KBr
	OpCmpLtI64KBr
	OpCmpLeI64KBr
	OpCmpGtI64KBr
	OpCmpGeI64KBr

	// Unconditional jump. C carries target PC (uint16).
	OpJump

	// Calls / returns. A = dst reg in caller, B = first arg reg in
	// caller, C = function index in Program.Funcs.
	OpCallI64       // dst is regsI64[A]
	OpCallF64       // dst is regsF64[A]
	OpCallCell      // dst is regsCell[A]
	OpTailCallI64   // tail call to Funcs[C]; new args from regsI64[B..]
	OpReturnI64     // return regsI64[A]
	OpReturnF64     // return regsF64[A]
	OpReturnCell    // return regsCell[A]
	OpReturnConstK  // return sign-extend(int16(C))

	// Deopt sentinel: causes Run to return EncodeDeopt(PC). Phase 5
	// JIT uses this to bail back to the interpreter at exact PC.
	OpDeopt

	// Phase 3 (placeholder; bodies arrive when Phase 3 lands).
	OpListGetI64
	OpListGetF64
	OpListGetCell
	OpListSetI64
	OpListSetF64
	OpListSetCell
)

// Op is a single 8-byte vm3 bytecode word. Layout:
//
//	byte 0  : OpCode (uint8)
//	byte 1  : BankFlags (reserved; Phase 4 uses for cross-bank moves)
//	bytes 2-3: register A (uint16)
//	bytes 4-5: register B (uint16) OR immediate (int16, sign-extended)
//	bytes 6-7: register C (uint16) OR immediate (int16) OR target PC (uint16)
//
// Specific opcodes pick the meaning of B/C per their definition above.
type Op struct {
	Code      OpCode
	BankFlags uint8
	A         uint16
	B         uint16
	C         int16
}

// MakeOp builds an Op with the given fields. Provided for compiler3
// emit-side ergonomics.
func MakeOp(code OpCode, a uint16, b uint16, c int16) Op {
	return Op{Code: code, A: a, B: b, C: c}
}
