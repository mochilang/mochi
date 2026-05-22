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
	// TypeListAny is the heterogeneous self-referential list backing
	// for `list<any>` (Phase 4.3.15.1). In the binary_trees kernel every
	// `any` element is itself a `list<any>` (a tree node), so the IR
	// unifies the two surface types to one tag. C target backs it with
	// runtime/c/src/mochi_tree.{h,c}; Go target emits a recursive named
	// slice `type _MochiAny []_MochiAny`. The `t[i] as list<any>` cast
	// reduces to a same-type no-op.
	TypeListAny
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
	case TypeListAny:
		return "listany"
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
	OpCmpEqStr
	OpCmpNeStr
	// Scalar→str conversions (MEP-42 Phase 4.2.4). Surface form
	// `str(x)` where x is i64/f64/bool. Result owns a freshly
	// allocated string; the bool form returns a static "true"/"false"
	// literal so it does not allocate. Go emit calls strconv.FormatInt
	// / FormatFloat / FormatBool; C emit calls into mochi_str.h.
	OpI64ToStr
	OpF64ToStr
	OpBoolToStr

	// Bool comparisons (MEP-42 Phase 4.2.7). Result Type == TypeBool.
	// Bool values are int-sized in both targets, so the C emit lowers
	// to plain `==` / `!=` and the Go emit to the same. Distinct ops
	// (rather than reusing OpCmpEqI64) keep the IR type-precise.
	OpCmpEqBool
	OpCmpNeBool

	// Bool logical AND / OR (MEP-42 Phase 4.2.9). Result == TypeBool.
	// Both targets emit C99 `&&` / `||` (and Go's `&&` / `||`); these
	// ARE short-circuit operators in both languages, but the IR
	// already pre-evaluates both args, so the emitted operator only
	// performs the logical reduction. Side-effect-bearing operands
	// would observe eager evaluation; tracked as a known limitation.
	OpAndBool
	OpOrBool

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

	// i64 bitwise / shift ops (Phase 4.1.1 scalar-operator parity).
	// `OpShrI64` is the signed right shift (C99 `>>` on signed
	// int64_t, which all modern compilers lower to an arithmetic
	// shift); a future unsigned-shift variant would land as a
	// separate OpcUshrI64 if Mochi ever exposes uint64 to the IR.
	OpAndI64
	OpOrI64
	OpXorI64
	OpShlI64
	OpShrI64
	OpNotI64

	// f64 comparisons (result Type == TypeBool). NaN behavior matches
	// IEEE 754 ordered comparison ("unordered yields false"), which is
	// the C99 default for `==`/`!=`/`<`/`<=`/`>`/`>=` on `double`.
	OpCmpEqF64
	OpCmpNeF64
	OpCmpLtF64
	OpCmpLeF64
	OpCmpGtF64
	OpCmpGeF64

	// Logical NOT on bool. `&&` and `||` lower in the frontend to
	// short-circuiting TermBranch graphs (no IR op needed), but unary
	// `!` is a single operator with no fall-through so it carries its
	// own opcode.
	OpNotBool

	// Numeric type conversions (Phase 4.3.4). OpI64ToF64 is the Mochi
	// `x as float` cast on an i64 value (lossless on the int64 range);
	// OpF64ToI64 is `x as int` (C99 truncation toward zero, matching the
	// Go target's `int64(f64)` semantics).
	OpI64ToF64
	OpF64ToI64

	// Math builtins (Phase 4.3.5). OpSqrtF64 is the f64 square root,
	// lowered from `math.sqrt(x)` after the python-import + extern
	// declarations are accepted as no-op binding statements. The Go
	// target emits `math.Sqrt(v)` (auto-imports "math"); the C target
	// emits `sqrt(v)` (auto-includes <math.h> and links with -lm).
	OpSqrtF64

	// List / array concatenation (Phase 4.3.12). Surface form `xs + ys`
	// where both operands are the same list-shaped type. Returns a
	// fresh list (no aliasing with the operands). OpListConcatI64 is
	// the i64-cell list pair; OpF64ArrayConcat is the flat f64 array
	// pair. Go emit copies via append + spread; C emit calls into a
	// runtime concat helper.
	OpListConcatI64
	OpF64ArrayConcat

	// Bench-harness builtins (Phase 4.3.13). OpNow returns the current
	// wall-clock time in microseconds since the Unix epoch as i64.
	// Go emit: `time.Now().UnixMicro()`. C emit: `mochi_now_us()`,
	// which wraps POSIX `gettimeofday` to produce the same microsecond
	// granularity. The op takes no SSA args.
	OpNow

	// list<any> ops (Phase 4.3.15.1). The arena is the recursive
	// `mochi_tree` struct holding a growable array of child pointers,
	// every child being itself a `mochi_tree`. The ops shadow the
	// list<i64> set but operate on tree pointers instead of scalars;
	// because every payload is itself a tree pointer, the surface
	// `t[i] as list<any>` cast is an SSA-level no-op (same IR type).
	OpNewListAny
	OpListAnyLen
	OpListAnyPushAny
	OpListAnyGetAny

	// Bench-harness JSON sink (Phase 4.3.14). OpJsonI64Object is the
	// statement-level form `json({"k1": v1, ..., "kN": vN})` where every
	// key is a string literal and every value is an i64 expression. It
	// prints a single-line JSON object plus newline. Args carries the
	// N value SSA IDs in declared order; Const indexes into
	// fn.JsonObjects for the matching key list. Result is TypeUnit.
	// Go emit: `fmt.Printf("{\"k1\":%d,...}\n", v1, ...)`. C emit:
	// `printf("{\"k1\":%lld,...}\n", v1, ...);`. The whole-object form
	// (not per-key) keeps the emit a single I/O call so the bench
	// harness reads one atomic line per program run, matching the
	// `bench/template/bg/*.mochi` fixture contract.
	OpJsonI64Object
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
	case OpCmpEqStr:
		return "cmp.eq.str"
	case OpCmpNeStr:
		return "cmp.ne.str"
	case OpI64ToStr:
		return "i64.to.str"
	case OpF64ToStr:
		return "f64.to.str"
	case OpBoolToStr:
		return "bool.to.str"
	case OpCmpEqBool:
		return "cmp.eq.bool"
	case OpCmpNeBool:
		return "cmp.ne.bool"
	case OpAndBool:
		return "and.bool"
	case OpOrBool:
		return "or.bool"
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
	case OpListConcatI64:
		return "list.concat.i64"
	case OpF64ArrayConcat:
		return "f64arr.concat"
	case OpNow:
		return "now"
	case OpNewListAny:
		return "newlistany"
	case OpListAnyLen:
		return "listany.len"
	case OpListAnyPushAny:
		return "listany.push"
	case OpListAnyGetAny:
		return "listany.get"
	case OpJsonI64Object:
		return "json.i64.object"
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
	case OpAndI64:
		return "and.i64"
	case OpOrI64:
		return "or.i64"
	case OpXorI64:
		return "xor.i64"
	case OpShlI64:
		return "shl.i64"
	case OpShrI64:
		return "shr.i64"
	case OpNotI64:
		return "not.i64"
	case OpCmpEqF64:
		return "cmp.eq.f64"
	case OpCmpNeF64:
		return "cmp.ne.f64"
	case OpCmpLtF64:
		return "cmp.lt.f64"
	case OpCmpLeF64:
		return "cmp.le.f64"
	case OpCmpGtF64:
		return "cmp.gt.f64"
	case OpCmpGeF64:
		return "cmp.ge.f64"
	case OpNotBool:
		return "not.bool"
	case OpI64ToF64:
		return "i64.to.f64"
	case OpF64ToI64:
		return "f64.to.i64"
	case OpSqrtF64:
		return "sqrt.f64"
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
	// SealHandles enables MEP-41 §6.7 sealing at this call boundary.
	// When true, every argument that carries a Mochi handle (list,
	// map, set, struct) is wrapped in `ffi.Seal[T]` and the return
	// value is wrapped in `ffi.Unseal[T]`. The runtime helpers are
	// type-system markers; the safety property is invariant rather
	// than dynamic at the Go target. See MEP-43 Phase 10.
	SealHandles bool
	// IsValue is true when this binding names an exported package-level
	// var or const, not a function. The emitter renders these as
	// `alias.Name` (no parens, no args); ArgTypes is empty and Result
	// carries the value's Go-source type. This is the FFI counterpart
	// of `OpConst` for symbols whose value is decided at link time.
	IsValue bool
}

// JsonObject names the key list for one OpJsonI64Object call site.
// Keys is the declared-order list of JSON object keys; the matching
// OpJsonI64Object Value carries the values via Args (one i64 SSA ID
// per key). The frontend constructs one JsonObject per `json({...})`
// expression-statement and indexes it from OpJsonI64Object.Const.
type JsonObject struct {
	Keys []string
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
	GoBindings  []GoBinding
	JsonObjects []JsonObject
	// Strings is the side-table of constant string payloads referenced
	// by OpConst Values of Type TypeStr. The Value's Const field is the
	// index into this slice. Side-table form mirrors GoBindings and
	// JsonObjects so the Const field stays int64-shaped.
	Strings    []string
	SourceFile  string

	// RefModes records the MEP-41 §6.9 reference-mode tag for the
	// Values whose Mochi-surface binding carried one. Nil for any
	// function compiled from default-mode source. Keys are Value IDs;
	// values are RefMode. See compiler3/ir/refmode.go for the
	// SetRefMode / RefModeOf accessors and the rule class E checker in
	// compiler3/verify.
	RefModes map[uint32]RefMode
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
