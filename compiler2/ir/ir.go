// Package ir is the typed SSA IR produced by compiler2's front end and
// consumed by the optimization, register allocation, and emit packages.
//
// Contract: MEP-21 v2. Values are typed. Blocks end with one of the
// terminator ops (OpRet, OpBr, OpCondBr). Phi nodes appear only at the
// head of a block.
package ir

// Type is the IR-level type. The set is intentionally small for step 4
// and grows with the typed-op table in MEP-21 v2 (float, string, list,
// map, struct, closure).
type Type uint8

const (
	TUnit Type = iota
	TI64
	TF64
	TBool
	TPtr
)

func (t Type) String() string {
	switch t {
	case TUnit:
		return "unit"
	case TI64:
		return "i64"
	case TF64:
		return "f64"
	case TBool:
		return "bool"
	case TPtr:
		return "ptr"
	}
	return "?"
}

// ValueID identifies an SSA value within a Function.
type ValueID int32

// BlockID identifies a basic block within a Function.
type BlockID int32

// Op enumerates IR opcodes. Order is significant only as a stable
// debugging aid.
type Op uint8

const (
	OpInvalid Op = iota

	// Pure: produce a typed value, no side effects.
	OpParam     // Aux = parameter index
	OpConstI64  // Aux = constant value
	OpConstBool // Aux = 0 or 1
	OpAddI64
	OpSubI64
	OpMulI64
	OpLessI64
	OpLessEqI64
	OpEqualI64

	// Call: Aux = function index, Args = arg values. May have effects;
	// optimizers must treat as opaque.
	OpCall

	// Terminators: must appear once and only at end of block.
	OpRet    // Args[0] = value (or none for TUnit)
	OpBr     // AuxBlocks[0] = target
	OpCondBr // Args[0] = cond, AuxBlocks[0] = then, AuxBlocks[1] = else

	// Phi: must appear at head of block. Args[i] pairs with AuxBlocks[i].
	OpPhi
)

// Inst is a single SSA instruction. Args/AuxBlocks are op-specific.
type Inst struct {
	Op        Op
	Type      Type
	Args      []ValueID
	AuxBlocks []BlockID
	Aux       int64
}

// IsTerminator reports whether the op ends a block.
func (op Op) IsTerminator() bool {
	switch op {
	case OpRet, OpBr, OpCondBr:
		return true
	}
	return false
}

// Block is a basic block in SSA form.
type Block struct {
	ID    BlockID
	Insts []ValueID // value IDs in execution order; last one is the terminator
}

// Function is a compiled function in IR form.
type Function struct {
	Name       string
	Params     []Type
	RetType    Type
	Blocks     []*Block
	Entry      BlockID
	Values     []Inst // indexed by ValueID
	ValueBlock []BlockID
}

// NumValues returns the count of SSA values defined in the function.
func (f *Function) NumValues() int { return len(f.Values) }

// Module is a collection of functions referenced by index.
type Module struct {
	Funcs []*Function
	Main  int
}
