package vm3

import "unsafe"

// Frame is one activation record. Each frame holds a base index into
// each of the VM's three typed register stacks; the live window for
// this activation is stack[base : base + fn.NumRegs*]. Storing only
// indices keeps the Frame record small and lets the call path avoid
// per-call register-slice allocation, which dominates recursive
// workloads.
//
// marks / freeMarks snapshot every arena slab's len() and free-list
// len() at pushFrame. On an unboxed Return* the interpreter calls
// arenas.truncateToMarks to slice each slab back to its mark and drop
// any free-list entries pointing above the mark. This is Layer A of
// the §6.7 memory plan: per-call region reclamation, no trace.
type Frame struct {
	fn *Function
	pc int

	baseI64  int
	baseF64  int
	baseCell int

	// retReg names the caller register that receives this frame's
	// return value. Encoded in the call op's A field.
	retReg uint16
	// retBank tags which bank retReg lives in.
	retBank Bank

	marks     [numArenaTags]uint32
	freeMarks [numArenaTags]uint32
}

// numArenaTags is one past the last ArenaTag enumerator. Sized to fit
// every tag so a Frame can hold one mark per arena without indirection.
const numArenaTags = 12

// Function is a compiled vm3 function. Each activation reserves
// NumRegs* slots in each typed register stack.
type Function struct {
	Name   string
	Code   []Op
	Consts []Cell

	NumRegsI64  uint16
	NumRegsF64  uint16
	NumRegsCell uint16

	ParamBanks []Bank
	ResultBank Bank

	// I64Tables holds compile-time constant i64 lookup tables (Phase 6.4,
	// port of Go CL 756340). OpLookupI64KW indexes I64Tables[uint16(C)]
	// directly: no arena resolution, no Cell boxing. Each table is a
	// plain Go-owned []int64 slice that lives as long as the Function.
	// Tables are emitted by compiler3's switch-to-lookup pass; sites
	// with sparse case ranges fall back to the cmp-chain lowering. The
	// JIT bakes &I64Tables[i][0] as an immediate so the lookup is a
	// single LDR after a (separate) bounds check.
	I64Tables [][]int64

	// JIT slots. Populated by runtime/jit/vm3jit.CompileAndCache when
	// the function lowers cleanly on the host arch; vm3 reads them in
	// OpCallI64 dispatch and routes through JITCallFn when JITCode is
	// non-nil. JITCompiled is the sticky negative-cache flag: once a
	// compile attempt has run, we do not retry on every call. The
	// CompiledFunc that owns the mmap'd page (and keeps JITCode valid)
	// is held by the caller of CompileAndCache, typically a
	// vm3runner-style harness or a test. JITHasF64 selects the
	// 4-argument trampoline (CallStatusFF) for f64-touching kernels.
	//
	// JITPreAllocList is set by the JIT when fn.Code[0] is an OpNewList
	// whose lowered body was skipped on the JIT side; vm3jit.jitCall
	// pre-allocates the list on the Go side before entering the
	// trampoline so the JIT can drop the inline allocation. Used to
	// admit `lists_fill_sum` main without growing the JIT into the
	// arena slab fast path (Phase 6.2d.2.b step 2).
	JITCode         unsafe.Pointer
	JITCompiled     bool
	JITHasF64       bool
	JITPreAllocList bool
}

// Bank identifies one of the three typed register banks.
type Bank uint8

const (
	BankI64 Bank = iota
	BankF64
	BankCell
)

// NumI64Params reports how many of f's parameters are in regsI64.
func (f *Function) NumI64Params() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankI64 {
			n++
		}
	}
	return n
}

// NumF64Params reports how many of f's parameters are in regsF64.
func (f *Function) NumF64Params() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankF64 {
			n++
		}
	}
	return n
}

// NumCellParams reports how many of f's parameters are in regsCell.
func (f *Function) NumCellParams() int {
	n := 0
	for _, b := range f.ParamBanks {
		if b == BankCell {
			n++
		}
	}
	return n
}
