package ir

// Type is the static type tag attached to every SSA Value.
type Type uint8

const (
	TypeInvalid Type = iota
	TypeI64
	TypeF64
	TypeBool
	TypeStr
	TypeList
	TypeMap
	TypeSet
	TypeStruct
	TypeClosure
	TypeBignum
	TypeBytes
	TypePair
	TypeF64Arr
	TypeI64Arr
	TypeU8Arr
	TypeAny
	TypeUnit
)

// String renders a Type's short name. Used by Validate's error
// messages and by fixture tests for IR-dump diffs.
func (t Type) String() string {
	switch t {
	case TypeInvalid:
		return "invalid"
	case TypeI64:
		return "i64"
	case TypeF64:
		return "f64"
	case TypeBool:
		return "bool"
	case TypeStr:
		return "str"
	case TypeList:
		return "list"
	case TypeMap:
		return "map"
	case TypeSet:
		return "set"
	case TypeStruct:
		return "struct"
	case TypeClosure:
		return "closure"
	case TypeBignum:
		return "bignum"
	case TypeBytes:
		return "bytes"
	case TypePair:
		return "pair"
	case TypeF64Arr:
		return "f64arr"
	case TypeI64Arr:
		return "i64arr"
	case TypeU8Arr:
		return "u8arr"
	case TypeAny:
		return "any"
	case TypeUnit:
		return "unit"
	}
	return "?"
}

// OpCode identifies which IR operation a Value represents. The set
// is rich enough to express every kernel in compiler3/corpus; Phase
// 4.6 admission may add more ops as BG programs require them.
type OpCode uint8

const (
	OpInvalid OpCode = iota

	// Source-of-value ops.
	OpParam // Value comes from the caller; Args is empty.
	OpConst // Value carries Const (sign-extended for i64, bit-cast for f64, 0/1 for bool).
	OpPhi   // Join at block entry; Args is (predBlockID, srcValueID) pairs.

	// i64 arithmetic.
	OpAddI64
	OpSubI64
	OpMulI64
	OpDivI64
	OpModI64
	OpNegI64

	// i64 arithmetic, immediate right operand (Const carries the imm).
	OpAddI64Imm
	OpSubI64Imm
	OpMulI64Imm
	OpDivI64Imm
	OpModI64Imm

	// f64 arithmetic.
	OpAddF64
	OpSubF64
	OpMulF64
	OpDivF64
	OpNegF64

	// i64 comparisons (result Type == TypeBool).
	OpCmpEqI64
	OpCmpNeI64
	OpCmpLtI64
	OpCmpLeI64
	OpCmpGtI64
	OpCmpGeI64

	// i64 comparisons against immediate right operand.
	OpCmpEqI64Imm
	OpCmpNeI64Imm
	OpCmpLtI64Imm
	OpCmpLeI64Imm
	OpCmpGtI64Imm
	OpCmpGeI64Imm

	// String ops (Phase 3.1 vm3 surface).
	OpLenStr
	OpConcatStr

	// List ops (Phase 3.2 vm3 surface).
	OpNewList
	OpListLenI64
	OpListPushI64
	OpListGetI64
	OpListSetI64

	// Map ops (Phase 3.3 vm3 surface).
	OpNewMap
	OpMapSetI64I64
	OpMapGetI64I64

	// Calls. Args[0..] are the argument values; the callee is named by
	// Value.Const (function index in the Function's owning Program).
	OpCall
	OpTailCall
)

// String renders an OpCode's short name. Used by Validate and IR
// dumps. Names match the Op* constants minus the OpCode prefix.
func (o OpCode) String() string {
	switch o {
	case OpInvalid:
		return "invalid"
	case OpParam:
		return "param"
	case OpConst:
		return "const"
	case OpPhi:
		return "phi"
	case OpAddI64:
		return "add.i64"
	case OpSubI64:
		return "sub.i64"
	case OpMulI64:
		return "mul.i64"
	case OpDivI64:
		return "div.i64"
	case OpModI64:
		return "mod.i64"
	case OpNegI64:
		return "neg.i64"
	case OpAddI64Imm:
		return "addk.i64"
	case OpSubI64Imm:
		return "subk.i64"
	case OpMulI64Imm:
		return "mulk.i64"
	case OpDivI64Imm:
		return "divk.i64"
	case OpModI64Imm:
		return "modk.i64"
	case OpAddF64:
		return "add.f64"
	case OpSubF64:
		return "sub.f64"
	case OpMulF64:
		return "mul.f64"
	case OpDivF64:
		return "div.f64"
	case OpNegF64:
		return "neg.f64"
	case OpCmpEqI64:
		return "cmp.eq.i64"
	case OpCmpNeI64:
		return "cmp.ne.i64"
	case OpCmpLtI64:
		return "cmp.lt.i64"
	case OpCmpLeI64:
		return "cmp.le.i64"
	case OpCmpGtI64:
		return "cmp.gt.i64"
	case OpCmpGeI64:
		return "cmp.ge.i64"
	case OpCmpEqI64Imm:
		return "cmp.eq.i64.imm"
	case OpCmpNeI64Imm:
		return "cmp.ne.i64.imm"
	case OpCmpLtI64Imm:
		return "cmp.lt.i64.imm"
	case OpCmpLeI64Imm:
		return "cmp.le.i64.imm"
	case OpCmpGtI64Imm:
		return "cmp.gt.i64.imm"
	case OpCmpGeI64Imm:
		return "cmp.ge.i64.imm"
	case OpLenStr:
		return "len.str"
	case OpConcatStr:
		return "concat.str"
	case OpNewList:
		return "newlist"
	case OpListLenI64:
		return "list.len.i64"
	case OpListPushI64:
		return "list.push.i64"
	case OpListGetI64:
		return "list.get.i64"
	case OpListSetI64:
		return "list.set.i64"
	case OpNewMap:
		return "newmap"
	case OpMapSetI64I64:
		return "map.set.i64.i64"
	case OpMapGetI64I64:
		return "map.get.i64.i64"
	case OpCall:
		return "call"
	case OpTailCall:
		return "tailcall"
	}
	return "?"
}

// Value is one SSA-form IR node. Type is mandatory; the type checker
// proves it before compiler3 sees the value. Const carries:
//   - the literal payload for OpConst (sign-extended i64, bit-cast
//     f64, 0/1 for bool)
//   - the immediate operand for Op*Imm
//   - the callee function index for OpCall / OpTailCall
type Value struct {
	ID       uint32
	Type     Type
	ElemType Type
	StructID uint32
	Op       OpCode
	Args     []uint32
	Const    int64
}

// TerminatorKind identifies the kind of block exit.
type TerminatorKind uint8

const (
	TermInvalid TerminatorKind = iota
	TermJump
	TermBranch
	TermReturn
)

// Terminator is a block's exit instruction. TermJump uses Target;
// TermBranch uses Value (the i1 / bool condition) plus IfTrue /
// IfFalse; TermReturn uses Value (the returned SSA value, or 0 for
// void returns where the function's Result is TypeUnit).
type Terminator struct {
	Kind    TerminatorKind
	Target  uint32
	IfTrue  uint32
	IfFalse uint32
	Value   uint32
}

// Block is a basic block in the function's CFG. Values lists the IDs
// of the SSA values produced by this block, in producer order. Phi
// values, if present, must appear at the head of Values.
type Block struct {
	ID     uint32
	Values []uint32
	Preds  []uint32
	Succs  []uint32
	Term   Terminator
}

// Function is the compilation unit handed from the type-aware build
// to the optimization pipeline. Values is indexed by ID. Blocks is
// indexed by Block.ID. Params lists the parameter Values in declared
// order (their Type is the param type; their Op is OpParam).
type Function struct {
	Name   string
	Params []uint32
	Result Type
	Blocks []Block
	Values []Value
}

// Builder helpers ----------------------------------------------------

// AddValue appends v to fn.Values, assigning v.ID, and returns the
// new ID. The value is not yet attached to any block; the caller
// must do that via Block.Values.
func (fn *Function) AddValue(v Value) uint32 {
	v.ID = uint32(len(fn.Values))
	fn.Values = append(fn.Values, v)
	return v.ID
}

// AddBlock appends a fresh Block and returns its ID. Callers must
// dereference via &fn.Blocks[id] (or fn.Block(id)) when mutating,
// because a subsequent AddBlock may reallocate the underlying slice
// and invalidate any *Block previously returned.
func (fn *Function) AddBlock() uint32 {
	id := uint32(len(fn.Blocks))
	fn.Blocks = append(fn.Blocks, Block{ID: id})
	return id
}

// Block returns a pointer to the block with the given ID. The pointer
// is valid until the next AddBlock call.
func (fn *Function) Block(id uint32) *Block {
	return &fn.Blocks[id]
}
