package vm2

import "unsafe"

// JITCallFn is installed by runtime/jit/vm2jit on package init.
// When non-nil and a callee has a non-nil JITCode, OpCall routes
// through this hook instead of pushing an interpreter frame.
// Signature: (vm *VM, callee *Function, argBase, nArgs int, retReg int32) Cell
var JITCallFn func(*VM, *Function, int, int, int32) Cell

// Instr is a single bytecode instruction. Fixed 20 bytes (one cache
// line holds three). Variable-length encoding is a later MEP-21 v2
// item; the fixed form keeps step 3 minimal.
//
// Flags carries operand-level annotations consumed by container ops on
// their fast paths. Today only the last-use bits are defined (MEP-36
// Phase 3c §3.5): a 1 in InstrFlagBLastUse / InstrFlagCLastUse means
// the corresponding operand register's read is its final read within
// the function, so the op may treat the receiver as exclusively owned.
// Ops that don't care about the bits ignore them; the encoding is
// forward-compatible with future per-operand attributes.
type Instr struct {
	Op      Op
	Flags   uint8
	_       [2]byte
	A, B, C int32
	D       int32
}

// Operand-flag bits carried in Instr.Flags. Each container op documents
// which operand positions it inspects; setting a flag for an operand
// the op does not consult is a no-op (the dispatcher ignores it).
const (
	InstrFlagBLastUse uint8 = 1 << 0 // operand B is at last use after this op
	InstrFlagCLastUse uint8 = 1 << 1 // operand C is at last use after this op
)

// Function is a compiled top-level function or method body.
type Function struct {
	Name      string
	NumParams int
	NumRegs   int
	Code      []Instr
	Consts    []Cell
	// StrConsts holds the raw bytes of string-typed constants. The
	// emit-time index in StrConsts is the operand B value of an
	// OpLoadStrK. Carrying bytes (rather than pre-allocated *vmStrings)
	// keeps Function trivially serializable later.
	StrConsts [][]byte
	// StrCells is the per-Cell view of StrConsts used by the dispatch
	// loop. Entries with len <= MaxInlineStr are packed inline at
	// runtime materialization (no allocation); longer entries are
	// allocated once into the running VM's Objects table and the Cell
	// is cached here. This field is populated by VM.Run; not part of
	// the on-disk Program shape.
	StrCells []Cell
	// JITCode is set by runtime/jit/vm2jit when this function has been
	// compiled to native code. A nil value means the interpreter is used.
	// The field holds an arch-specific function pointer; vm2jit installs
	// JITCallFn to call it correctly. vm2 itself treats this as opaque.
	JITCode unsafe.Pointer
	// HasContainerSlots is true when any register in this function's
	// window may hold a typed pointer Cell (list, map, string, ptr) at
	// some point during execution. MEP-36 Phase 3: popFrame skips the
	// clear(stack[base:]) sweep when this is false, restoring int-only
	// programs (fib_rec, fact_rec, fib_iter, mul_loop) to their Phase 1
	// numbers (see MEP-36 Appendix C.3). emit computes the flag from the
	// IR value-type stream.
	HasContainerSlots bool

	// Leaf-shape callee shortcut (MEP-39 §6.10 iter 3). Set during emit
	// when this function's entry matches:
	//
	//   pc=0: OpJumpIfNotEqualI64K guardReg, K, 2
	//   pc=1: OpReturnI64K Vint     OR     OpReturnNewPairKK _, Pa, Pb
	//
	// i.e. "if param_guard != K then continue; else return constant".
	// At the call site, dispatch handlers test the cached guard against
	// the incoming argument and materialize the constant return without
	// pushing a frame. LeafKind == LeafKindNone disables the shortcut.
	LeafKind     LeafKind
	LeafGuardReg int32 // index of the param to test (callee-side)
	LeafGuardK   int32 // sign-extended K to compare against
	LeafReturnA  int32 // OpReturnI64K: the constant return value
	LeafReturnB  int32 // OpReturnNewPairKK: pair.fst
	LeafReturnC  int32 // OpReturnNewPairKK: pair.snd

	// BranchEntryIP is the ip to use when entering this function on the
	// branch path. For leaf-shape callees the entry guard at pc=0 jumps
	// to pc=2 when the arg fails the guard test; if the caller already
	// performed the guard test inline (via the leaf shortcut) and saw a
	// mismatch, the frame can start directly at pc=2 to skip the guard
	// dispatch. MEP-39 §6.10 iter 5: set to 2 by AnalyzeLeafShape when
	// the leaf-shape match succeeds, else 0 (default entry).
	BranchEntryIP int32
}

// LeafKind classifies the cached leaf-return shape (see Function.LeafKind).
type LeafKind uint8

const (
	LeafKindNone           LeafKind = iota
	LeafKindReturnI64K              // ret = CInt(LeafReturnA)
	LeafKindReturnNewPairKK         // ret = newPair(CInt(LeafReturnB), CInt(LeafReturnC))
)

// Program is the unit of execution. Main names the entry function.
type Program struct {
	Funcs []*Function
	Main  int
}
