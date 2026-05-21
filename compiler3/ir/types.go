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
	// F64 list ops (Phase 6.3.4.j.1 vm3 surface).
	OpListGetF64
	OpListSetF64

	// Map ops (Phase 3.3 vm3 surface).
	OpNewMap
	OpMapSetI64I64
	OpMapGetI64I64

	// Typed f64 array ops (Phase 6.3.4.j.5 vm3 surface). Flat
	// vmF64Array backing skips the 16-byte Cell payload (8 bytes
	// per element) used by OpListGetF64/SetF64.
	OpNewF64Array
	OpF64ArrayLenI64
	OpF64ArrayPushF64
	OpF64ArrayGetF64
	OpF64ArraySetF64

	// Calls. Args[0..] are the argument values; the callee is named by
	// Value.Const (function index in the Function's owning Program).
	OpCall
	OpTailCall

	// Function reference. Value.Const is the function index in the
	// owning Program; Value.Type is TypeClosure. Produced for use as
	// an argument to query ops; the emitter inlines the referenced
	// function's name at the consumer site, so an OpFnRef value never
	// lowers to a standalone Go expression.
	OpFnRef

	// Query algebra ops. These lower in the Go emitter to calls into
	// runtime/mochi/query, never inlined as algebra. Closure-shaped
	// arguments arrive as OpFnRef values whose Const names the
	// callee. Phase 5 covers the i64-everywhere subset (list ElemType
	// TypeI64, key TypeI64, output TypeI64); the Mochi frontend will
	// widen the supported element types in Phase 6.
	OpQueryFilter     // Args=[srcList, predFnRef]
	OpQueryMap        // Args=[srcList, mapFnRef]
	OpQuerySortBy     // Args=[srcList, keyFnRef]
	OpQuerySortByDesc // Args=[srcList, keyFnRef]
	OpQueryLimit      // Args=[srcList, nValue]
	OpQueryDistinct   // Args=[srcList]
	OpQueryGroupBy    // Args=[srcList, keyFnRef]; result Type is TypeAny (opaque to the IR until Phase 6 lands structured access)
	OpQueryJoin       // Args=[leftList, rightList, lkeyFnRef, rkeyFnRef, combineFnRef]
	OpQueryLeftJoin   // same shape as OpQueryJoin; combineFn signature is func(L, R, hasR bool) Out
	OpQueryOuterJoin  // same shape; combineFn signature is func(L, R, hasL, hasR bool) Out
	OpQueryCrossJoin  // Args=[leftList, rightList, combineFnRef]

	// OpCallGo invokes an imported Go symbol resolved at Mochi compile
	// time by compiler3/ffi/resolve. The Value.Const indexes into the
	// owning Function's GoBindings table; Args lists the SSA value IDs
	// of the call arguments in declared order. The emitter prints a
	// literal Go call (alias.Name(arg0, arg1, ...)) and registers the
	// binding's import path so the generated file has a normal Go
	// import. There is no Mochi-side reflection at the call site.
	OpCallGo
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
	case OpListGetF64:
		return "list.get.f64"
	case OpListSetF64:
		return "list.set.f64"
	case OpNewMap:
		return "newmap"
	case OpMapSetI64I64:
		return "map.set.i64.i64"
	case OpMapGetI64I64:
		return "map.get.i64.i64"
	case OpNewF64Array:
		return "newf64array"
	case OpF64ArrayLenI64:
		return "f64arr.len.i64"
	case OpF64ArrayPushF64:
		return "f64arr.push.f64"
	case OpF64ArrayGetF64:
		return "f64arr.get.f64"
	case OpF64ArraySetF64:
		return "f64arr.set.f64"
	case OpCall:
		return "call"
	case OpTailCall:
		return "tailcall"
	case OpFnRef:
		return "fnref"
	case OpQueryFilter:
		return "query.filter"
	case OpQueryMap:
		return "query.map"
	case OpQuerySortBy:
		return "query.sortby"
	case OpQuerySortByDesc:
		return "query.sortby.desc"
	case OpQueryLimit:
		return "query.limit"
	case OpQueryDistinct:
		return "query.distinct"
	case OpQueryGroupBy:
		return "query.groupby"
	case OpQueryJoin:
		return "query.join"
	case OpQueryLeftJoin:
		return "query.leftjoin"
	case OpQueryOuterJoin:
		return "query.outerjoin"
	case OpQueryCrossJoin:
		return "query.crossjoin"
	case OpCallGo:
		return "call.go"
	}
	return "?"
}

// GoBinding names an imported Go symbol resolved at Mochi compile
// time. Phase 6 attaches one GoBinding per FFI call site; the IR
// references it by index via OpCallGo.Const. ArgTypes and Result are
// captured so a future Phase 7 type-check pass can validate without
// re-querying the resolver.
type GoBinding struct {
	// Pkg is the Go import path, e.g. "strings", "math/big".
	Pkg string
	// Alias is the Go-source-level alias, e.g. "strings" or "big".
	// The default is the last path segment; Mochi `as` shadows it.
	Alias string
	// Name is the function or method name as it appears under the
	// alias in the emitted source.
	Name string
	// ArgTypes lists the static Go types of the call arguments in
	// declared order. Each entry is a Go-source-shape string the
	// emitter can paste verbatim (string, int64, []byte, ...).
	ArgTypes []string
	// Result is the Go-source-shape of the call's return type.
	// Empty means void; a comma-joined list (e.g. "string,error")
	// names a multi-value return, but Phase 6 sticks to single-value
	// signatures and leaves error handling to Phase 7.
	Result string
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
//
// SourceLine, when non-zero, is the Mochi-source line this block
// originates from. Phase 7 of MEP-43 uses it to emit `//line` directives
// at block boundaries so a `go build` error inside the generated file
// surfaces with the user's Mochi coordinates.
type Block struct {
	ID         uint32
	Values     []uint32
	Preds      []uint32
	Succs      []uint32
	Term       Terminator
	SourceLine uint32
}

// Function is the compilation unit handed from the type-aware build
// to the optimization pipeline. Values is indexed by ID. Blocks is
// indexed by Block.ID. Params lists the parameter Values in declared
// order (their Type is the param type; their Op is OpParam).
//
// GoBindings names the Go symbols this function calls via the
// `import go "path"` form; each OpCallGo Value indexes into this
// table via Value.Const. Empty when the function does no FFI.
//
// SourceFile, when non-empty, is the Mochi-source file path. Phase 7
// of MEP-43 pairs it with Block.SourceLine to emit `//line` directives
// at every block boundary so Go-toolchain diagnostics can be rewritten
// to point at the user's `.mochi` file.
type Function struct {
	Name       string
	Params     []uint32
	Result     Type
	Blocks     []Block
	Values     []Value
	GoBindings []GoBinding
	SourceFile string
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
