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

	// Strings (Phase 3.1).
	OpConstStrKW // regsCell[A] = Function.Consts[uint16(C)] (a Cell-tagged string)
	OpLenStr     // regsI64[A] = len(string at regsCell[B])
	OpConcatStr  // regsCell[A] = regsCell[B] ++ regsCell[uint16(C)]

	// Mixed-bank calls (Phase 3.1). Args live at regs<bank>[B + k] in
	// the caller for each param k whose bank is given by callee
	// ParamBanks[k]. Callee receives them at regs<bank>[k]. Slots in
	// banks other than ParamBanks[k] at position B+k are unused. A
	// names the caller's return slot in retBank (BankFlags low 2 bits).
	// C carries callee Function index.
	OpCallMixed
	// OpTailCallMixed: like OpCallMixed but reuses the current frame.
	// No retReg / retBank fields are read; the existing frame's retReg
	// and retBank are preserved.
	OpTailCallMixed

	// Lists (Phase 3.2).
	OpNewList     // regsCell[A] = arenas.AllocList(elemType=0, capHint=0)
	OpListLenI64  // regsI64[A] = arenas.ListLen(regsCell[B])
	OpListPushI64 // arenas list at regsCell[A] gets CInt(regsI64[B])
	OpListGetI64  // regsI64[A] = arenas.ListGet(regsCell[B], regsI64[uint16(C)]).Int()
	OpListSetI64  // arenas list at regsCell[A] at regsI64[uint16(C)] = CInt(regsI64[B])

	// Maps (Phase 3.3). i64-keyed open-addressed maps. A is the map
	// reg in the caller's Cell bank; B is the key reg in I64 bank; C
	// is the value reg in I64 bank (or dst reg for Get).
	OpNewMap        // regsCell[A] = arenas.AllocMap(capHint=int(uint16(C)))
	OpMapSetI64I64  // arenas.MapSetI64(regsCell[A], regsI64[B], regsI64[uint16(C)])
	OpMapGetI64I64  // regsI64[A] = arenas.MapGetI64(regsCell[B], regsI64[uint16(C)])

	// F64 list ops (Phase 6.3.4.j.1). Mirror OpListGetI64/OpListSetI64
	// but exchange the f64 register bank with the Cell-encoded payload
	// (CFloat round-trip). Used by n_body to keep position/velocity
	// arrays of f64 values out of the i64 register pressure.
	OpListGetF64 // regsF64[A] = arenas.Lists[idx].cells[regsI64[uint16(C)]].Float()
	OpListSetF64 // arenas list at regsCell[A] at regsI64[uint16(C)] = CFloat(regsF64[B])

	// Cell list ops (Phase 3.6 placeholder). Bodies land when the
	// struct/closure/set lowering needs untyped Cell payloads.
	OpListGetCell
	OpListSetCell

	// F64 compare-and-branch (Phase 6.2b). C carries target PC (uint16).
	// IEEE 754 unordered comparisons: NaN forces the branch condition to
	// false for ordered predicates (Lt/Le/Gt/Ge), so a NaN operand never
	// branches under those. Eq compares as unordered-false (NaN != NaN);
	// Ne returns true when either operand is NaN.
	OpCmpEqF64Br // if regsF64[A] == regsF64[B] jump to uint16(C)
	OpCmpNeF64Br
	OpCmpLtF64Br
	OpCmpLeF64Br
	OpCmpGtF64Br
	OpCmpGeF64Br

	// Bank casts (Phase 6.2b). Used by FP kernels that thread an i64
	// loop counter through an f64 accumulator.
	OpI64ToF64 // regsF64[A] = float64(regsI64[B])
	OpF64ToI64 // regsI64[A] = int64(regsF64[B]) (truncating, Go semantics)

	// Fused multiply-add (Phase 6.3.4.h). 3-source f64:
	//   regsF64[A] = math.FMA(regsF64[B], regsF64[uint16(C)&0xFF],
	//                         regsF64[(uint16(C)>>8)&0xFF])
	// C packs two 8-bit f64 register indices (mul2 in low byte, addend
	// in high byte). 8 bits is sufficient because MaxF64Regs is 8 on
	// both ARM64 and AMD64. On ARM64 this lowers to a single FMADD
	// instruction (one rounding step, IEEE 754-2008 fused). Bit-identical
	// to Go's math.FMA, so corpus kernels that thread accumulators
	// through `nz = math.FMA(a*b, c, d)` (mandelbrot, n_body) can be
	// ported bit-for-bit.
	OpFmaF64

	// Switch-statement lookup-table optimization (Phase 6.4, port of
	// Go CL 756340 / golang/go#78203). regsI64[A] = fn.I64Tables[uint16(C)]
	// [regsI64[B]]. I64Tables is a Go-owned [][]int64 living on the
	// Function, so the load is one indexed read with no arena
	// resolution and no Cell boxing. The op is unchecked: the caller
	// must have emitted a prior OpCmpGeI64KBr (or proven the index in
	// range) so the load cannot run past the table. A naive switch
	// lowers to a chain of OpCmpEqI64KBr; the table form lowers to one
	// OpCmpGeI64KBr + one OpLookupI64KW + the case-body return. See
	// compiler3/corpus/switch_lookup.go for the bench shape.
	OpLookupI64KW

	// Scalar f64 square root (Phase 6.3.4.j prep). 2-source f64:
	//   regsF64[A] = math.Sqrt(regsF64[B])
	// IEEE 754 correctly-rounded, bit-identical to Go's math.Sqrt
	// (which on arm64 is a single FSQRT). On ARM64 lowers to one
	// FSQRT Dd, Dn (0x1E61C000 | (B<<5) | A). On AMD64 lowers to
	// SQRTSD xmmA, xmmB. Prereq for n_body's inner-loop distance
	// computation (sqrt(dx*dx + dy*dy + dz*dz)).
	OpSqrtF64
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
