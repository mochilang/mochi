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
	TStr
	TList
	TMap
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
	case TStr:
		return "str"
	case TList:
		return "list"
	case TMap:
		return "map"
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
	OpDivI64
	OpModI64
	OpLessI64
	OpLessEqI64
	OpEqualI64

	// String subsystem (MEP-24 §2). OpConstStr's Aux is the index into
	// the function-local string constant pool the builder maintains; the
	// emitter merges those into Function.StrConsts. OpIndexStr may trap
	// on OOB so it is not treated as pure by DCE.
	OpConstStr  // Aux = idx into Function.Strings
	OpConcatStr // Args[0] ++ Args[1]
	OpLenStr    // len(Args[0])
	OpIndexStr  // Args[0][Args[1]]
	OpEqualStr  // Args[0] == Args[1]
	OpHashStr   // hash(Args[0])

	// List subsystem (MEP-24 §3). Lists are reference-typed; the
	// allocating ops produce a fresh TList value. ListGet may trap on
	// OOB (not pure); ListSet/ListPush mutate and are never pure.
	OpNewList  // Aux = capacity hint
	OpListLen  // len(Args[0])
	OpListGet  // Args[0][Args[1]]
	OpListSet  // Args[0][Args[1]] = Args[2]; result type TUnit
	OpListPush // Args[0].push(Args[1]); result type TUnit
	// OpListAppend models Mochi's functional append: result is a new
	// TList equal to Args[0] with Args[1] pushed; Args[0] is unchanged.
	// Without copy-elision the implementation allocates a fresh backing
	// array. The emitter consults the regalloc last-use map: when
	// Args[0] is at last use, it sets the MEP-36 Phase 3c last-use bit
	// on operand B and the runtime mutates the source in place, returning
	// the same pointer. This is the IR's anchor for the spec's "fluent
	// chain captures in-place" claim ([§3.5](#35-compile-time-last-use-bit)).
	OpListAppend

	// Map subsystem (MEP-24 §4). Maps are reference-typed; OpNewMap
	// produces a fresh TMap value. OpMapGet returns the value type (or
	// TUnit/null sentinel when callers do not care); OpMapSet/OpMapDel
	// mutate and have TUnit result.
	OpNewMap // (no args)
	OpMapLen // len(Args[0])
	OpMapGet // Args[0][Args[1]]; missing key -> null cell
	OpMapHas // bool(present(Args[0], Args[1]))
	OpMapSet // Args[0][Args[1]] = Args[2]; TUnit
	OpMapDel // delete Args[0][Args[1]]; TUnit

	// Call: Aux = function index, Args = arg values. May have effects;
	// optimizers must treat as opaque.
	OpCall

	// Terminators: must appear once and only at end of block.
	OpRet      // Args[0] = value (or none for TUnit)
	OpBr       // AuxBlocks[0] = target
	OpCondBr   // Args[0] = cond, AuxBlocks[0] = then, AuxBlocks[1] = else
	OpTailCall // tail call. Aux = function index, Args = arg values; transfers control without growing the frame stack.

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
	case OpRet, OpBr, OpCondBr, OpTailCall:
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
	// Strings is the per-function string pool referenced by OpConstStr.
	// Aux on an OpConstStr is an index into this slice. The pool is
	// deduplicated by the builder; emit forwards it to vm2.Function.StrConsts.
	Strings []string
}

// NumValues returns the count of SSA values defined in the function.
func (f *Function) NumValues() int { return len(f.Values) }

// Module is a collection of functions referenced by index.
type Module struct {
	Funcs []*Function
	Main  int
}
