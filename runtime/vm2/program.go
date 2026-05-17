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
type Instr struct {
	Op      Op
	_       [3]byte
	A, B, C int32
	D       int32
}

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
}

// Program is the unit of execution. Main names the entry function.
type Program struct {
	Funcs []*Function
	Main  int
}
