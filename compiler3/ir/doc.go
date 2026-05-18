// Package ir is compiler3's typed SSA intermediate representation.
//
// Every Value carries a static Type (i64, f64, bool, str, list, map,
// struct, ...). Passes preserve type; lowering picks the opcode by
// type. See MEP-40 §7.1.
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
)

// OpCode identifies which IR operation a Value represents. Phase 0
// declares only the placeholder set; Phase 2 expands.
type OpCode uint8

const (
	OpInvalid OpCode = iota
	OpConst
	OpParam
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpCmpEq
	OpCmpLt
	OpCall
	OpReturn
)

// Value is one SSA-form IR node. Type is mandatory; the type checker
// proves it before compiler3 sees the value.
type Value struct {
	ID       uint32
	Type     Type
	ElemType Type
	StructID uint32
	Op       OpCode
	Args     []uint32
	Const    int64
}

// Terminator is a block's exit instruction. Phase 0 declares the kinds
// the spec requires; bodies arrive in Phase 2.
type Terminator struct {
	Kind   TerminatorKind
	Target uint32
	IfTrue uint32
	IfFalse uint32
	Value  uint32
}

type TerminatorKind uint8

const (
	TermInvalid TerminatorKind = iota
	TermJump
	TermBranch
	TermReturn
)

// Block is a basic block in the function's CFG.
type Block struct {
	ID     uint32
	Values []uint32
	Preds  []uint32
	Succs  []uint32
	Term   Terminator
}

// Function is the compilation unit handed from the type-aware build to
// the optimization pipeline.
type Function struct {
	Name   string
	Params []Value
	Result Type
	Blocks []Block
	Values []Value
}
