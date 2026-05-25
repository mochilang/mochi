package aotir

import "strings"

// Program is a complete unit of lowered Mochi. Phase 1 ships a
// minimum shape sufficient for "one function calling one string
// print"; later phases extend the type set, the statement set,
// and the expression set as their gates require.
//
// Determinism: callers must keep Functions in sorted-by-Name
// order before emit (Phase 17 reproducibility gate). The
// verifier does not enforce this in Phase 1 (only one function
// exists), but Phase 11 onward relies on it.
type Program struct {
	// Functions is the closure-converted, monomorphic set of
	// functions. The entry point is Functions[Main].
	Functions []*Function

	// Main is the index into Functions of the entry point. The
	// entry function takes no arguments and returns TypeUnit.
	Main int

	// Records lists user-declared record types in source order.
	// Phase 3.0 adds this; emit walks it to write `struct mochi_R`
	// declarations and the per-record equality helper.
	Records []*RecordDecl

	// Unions lists user-declared sum types in source order.
	// Phase 4.0 adds this; emit walks it to write the tagged-union
	// C struct and per-variant constructor inlines.
	Unions []*UnionDecl
}

// RecordDecl declares one record type. Field order is source
// order; the emit pass preserves it into the C struct layout
// (Phase 17 reproducibility relies on a stable layout per source
// shape).
type RecordDecl struct {
	Name   string
	Fields []RecordField
}

// RecordField is one (Name, Type) pair inside a RecordDecl.
// RecordName carries the record's identity when Type==TypeRecord
// (nested records). Phase 3.0 keeps RecordName empty and the
// lowerer rejects nested records; future sub-phases will lift
// that restriction.
type RecordField struct {
	Name       string
	Type       Type
	RecordName string
}

// Param is one formal parameter of a Function. Phase 2.2 introduces
// user-defined multi-arg functions; before then the only callable
// was main() which took none. Phase 3.0 adds RecordName, valid when
// Type==TypeRecord. Phase 3.1 adds ElemType, valid when
// Type==TypeList; the element is always a scalar primitive
// (TypeInt / TypeFloat / TypeBool / TypeString) in 3.1. Phase 3.2
// adds KeyType + ValueType, valid when Type==TypeMap; both are
// scalar primitives drawn from the 3.2 per-(K,V) instantiation
// set (K ∈ {TypeInt, TypeString}, V ∈ {TypeInt, TypeFloat,
// TypeBool, TypeString}). Phase 3.4 adds ElemRecordName, valid when
// Type==TypeList and ElemType==TypeRecord, carrying the element
// record's identity so the emit pass can pick the right per-record
// list helper instantiation. Phase 3.4e adds ListValueElemType,
// valid when Type==TypeMap && ValueType==TypeList, carrying the
// inner scalar element type of the list value. Phase 3.4f adds
// MapElemKeyType and MapElemValueType, valid when Type==TypeList &&
// ElemType==TypeMap, carrying the map's K and V so helpers can be
// resolved.
type Param struct {
	Name           string
	Type           Type
	RecordName     string
	UnionName      string // valid when Type==TypeUnion (Phase 4)
	ElemType       Type
	ElemRecordName string
	// InnerElemType carries the inner element type when
	// Type==TypeList && ElemType==TypeList (one-level nested
	// list<list<T>>; Phase 3.4b restricts the inner to a scalar
	// primitive). Empty (TypeInvalid) otherwise.
	InnerElemType Type
	// MapElemKeyType and MapElemValueType carry the map's K and V
	// when Type==TypeList && ElemType==TypeMap (Phase 3.4f
	// list<map<K,V>>). Both are TypeInvalid otherwise.
	MapElemKeyType   Type
	MapElemValueType Type
	KeyType          Type
	ValueType        Type
	// ListValueElemType carries the inner scalar element type when
	// Type==TypeMap && ValueType==TypeList (Phase 3.4e map<K,list<V>>).
	// Empty (TypeInvalid) otherwise.
	ListValueElemType Type
	// FunSig carries the function type's signature when Type==TypeFun
	// (Phase 5.0). Nil otherwise.
	FunSig *FunSig
}

// Function is one monomorphic, closure-converted callable.
type Function struct {
	// Name is the mangled, emit-stable identifier. The mangling
	// scheme reserves an unambiguous mapping from Mochi name +
	// type arguments to a C identifier; the verifier checks
	// uniqueness across the Program.
	Name string

	// Params lists the formal parameters in source order.
	// The entry function (Main) has zero params.
	Params []Param

	// IsLifted marks a function that was lifted from an anonymous
	// closure literal (Phase 5.0+). Lifted functions receive
	// `void *__mochi_env` as their first parameter so they conform
	// to the mochi_closure_* function-pointer ABI.
	IsLifted bool

	// EnvTypeName is the C typedef name of the environment struct
	// this lifted function expects (e.g. "__anon_2_env_t"). Empty
	// for non-capturing lifted functions.
	EnvTypeName string

	// Captures lists the variables this lifted function captures from
	// the enclosing scope (Phase 5.1). Empty for non-capturing closures.
	// The emitter uses this to emit the env struct typedef before the
	// function definition.
	Captures []FunCapture

	// ReturnType is the function's monomorphic return type.
	ReturnType Type

	// ReturnRecordName carries the record identity when
	// ReturnType==TypeRecord. Empty otherwise.
	ReturnRecordName string

	// ReturnUnionName carries the union identity when
	// ReturnType==TypeUnion (Phase 4). Empty otherwise.
	ReturnUnionName string

	// ReturnElemType carries the element type when
	// ReturnType==TypeList. Phase 3.1 restricts it to the four
	// scalar primitives; Phase 3.4 widens it to TypeRecord with
	// ReturnElemRecordName naming the element record.
	ReturnElemType       Type
	ReturnElemRecordName string

	// ReturnInnerElemType carries the inner element type when
	// ReturnType==TypeList && ReturnElemType==TypeList (Phase 3.4b
	// list<list<T>>). The inner is a scalar primitive in 3.4b.
	ReturnInnerElemType Type
	// ReturnMapElemKeyType and ReturnMapElemValueType carry the map's
	// K and V when ReturnType==TypeList && ReturnElemType==TypeMap
	// (Phase 3.4f list<map<K,V>>). Both are TypeInvalid otherwise.
	ReturnMapElemKeyType   Type
	ReturnMapElemValueType Type

	// ReturnKeyType and ReturnValueType carry the K/V identities
	// when ReturnType==TypeMap. Phase 3.2 restricts the pair to
	// one of the eight per-(K,V) runtime instantiations.
	ReturnKeyType   Type
	ReturnValueType Type
	// ReturnListValueElemType carries the inner scalar element type when
	// ReturnType==TypeMap && ReturnValueType==TypeList (Phase 3.4e
	// map<K,list<V>>). Empty (TypeInvalid) otherwise.
	ReturnListValueElemType Type

	// ReturnFunSig carries the function signature when ReturnType==TypeFun
	// (Phase 5.0). Nil otherwise.
	ReturnFunSig *FunSig

	// Body is a single Block. Phase 1 does not introduce control
	// flow; Phase 2 introduces multi-block functions with a
	// terminator on every block.
	Body *Block
}

// Block is a straight-line sequence of statements. Phase 1
// blocks have no terminator; Phase 2 adds one.
type Block struct {
	// Statements run top-to-bottom.
	Statements []Stmt
}

// Stmt is one block statement. Phase 1 ships only CallStmt
// (a side-effecting call returning TypeUnit). Phase 2 adds
// LetStmt, AssignStmt, ReturnStmt, control-flow terminators.
type Stmt interface {
	isStmt()
}

// CallStmt is a procedure call evaluated for its side effect.
// The callee is a runtime builtin or a previously declared
// function; the verifier resolves Func against the active
// symbol set. When the callee returns a non-unit value, the
// statement form discards it (Mochi `foo()` at top level for a
// non-void foo).
type CallStmt struct {
	// Func is the mangled callee name.
	Func string

	// Args carries the call arguments in source order. Each
	// expression's Type must match the corresponding parameter
	// type of the resolved callee.
	Args []Expr
}

func (*CallStmt) isStmt() {}

// CallExpr is a value-producing call to a user-defined function
// (Phase 2.2). Builtins like the print family always return unit
// and so do not appear here. The lowerer resolves Func against the
// Program's function table at lower time and stamps Result with the
// callee's ReturnType; the verifier re-checks both invariants.
type CallExpr struct {
	Func                    string
	Args                    []Expr
	Result                  Type
	ResultRecordName        string // valid when Result==TypeRecord
	ResultUnionName         string // valid when Result==TypeUnion (Phase 4)
	ResultElemType          Type   // valid when Result==TypeList
	ResultElemRecordName    string // valid when Result==TypeList && ResultElemType==TypeRecord
	ResultInnerElemType     Type   // valid when Result==TypeList && ResultElemType==TypeList (Phase 3.4b)
	ResultMapElemKeyType    Type   // valid when Result==TypeList && ResultElemType==TypeMap (Phase 3.4f)
	ResultMapElemValueType  Type   // valid when Result==TypeList && ResultElemType==TypeMap (Phase 3.4f)
	ResultKeyType           Type    // valid when Result==TypeMap
	ResultValueType         Type    // valid when Result==TypeMap
	ResultListValueElemType Type    // valid when Result==TypeMap && ResultValueType==TypeList (Phase 3.4e)
	ResultFunSig            *FunSig // valid when Result==TypeFun (Phase 5.0/5.1)
}

func (c *CallExpr) Type() Type { return c.Result }

// Expr is a value-producing aotir node. Phase 1 ships only
// StringLit; Phase 2.0 adds the scalar literals plus binary
// and unary expressions over them.
type Expr interface {
	// Type reports the monomorphic type of this expression's
	// produced value. The verifier uses it to type-check call
	// arguments and (Phase 2 onward) return statements.
	Type() Type
}

// StringLit is a literal string value. The bytes are stored
// raw; the emit pass is responsible for C-string escaping.
type StringLit struct {
	Value string
}

func (*StringLit) Type() Type { return TypeString }

// IntLit is a 64-bit signed integer literal. Phase 2.0 emits
// it as an `INT64_C(N)` C constant so the value carries its
// type explicitly into integer-typed expression contexts.
type IntLit struct {
	Value int64
}

func (*IntLit) Type() Type { return TypeInt }

// FloatLit is an IEEE 754 binary64 literal. The emit pass
// renders it via Go's strconv.FormatFloat 'g' -1 64 so the
// generated C source round-trips exactly through the host's
// strtod. Phase 2.4 hardens this for NaN/Inf bit-equal
// reproduction.
type FloatLit struct {
	Value float64
}

func (*FloatLit) Type() Type { return TypeFloat }

// BoolLit is a true/false literal. Phase 2.0 emits it as a C
// `0` / `1` int constant; the runtime print path uses the
// `int` ABI for mochi_print_bool.
type BoolLit struct {
	Value bool
}

func (*BoolLit) Type() Type { return TypeBool }

// BinOp is the operator of a BinaryExpr. The set covers every
// operator the parser surfaces for scalar primitives. The
// verifier rejects type combinations the lowerer should have
// already monomorphised away (e.g. mixed int + float operands
// without an explicit cast lowering step).
type BinOp int

const (
	BinInvalid BinOp = iota
	// Integer arithmetic. Each operand is TypeInt; the result
	// is TypeInt.
	BinAddI64
	BinSubI64
	BinMulI64
	BinDivI64
	BinModI64
	// Float arithmetic. Each operand is TypeFloat; the result
	// is TypeFloat.
	BinAddF64
	BinSubF64
	BinMulF64
	BinDivF64
	// Integer comparison. Each operand is TypeInt; the result
	// is TypeBool.
	BinEqI64
	BinNeI64
	BinLtI64
	BinLeI64
	BinGtI64
	BinGeI64
	// Float comparison. Each operand is TypeFloat; the result
	// is TypeBool.
	BinEqF64
	BinNeF64
	BinLtF64
	BinLeF64
	BinGtF64
	BinGeF64
	// Bool comparison. Each operand is TypeBool; the result is
	// TypeBool.
	BinEqBool
	BinNeBool
	// String comparison. Each operand is TypeString; the result
	// is TypeBool. Lowered to strcmp(a,b)==0 / !=0 by the emit
	// pass. Added in Phase 3.0 to support record-equality fixtures
	// that include string fields.
	BinEqStr
	BinNeStr
	// Record comparison. Each operand is TypeRecord with the
	// same record name; the result is TypeBool. The emit pass
	// dispatches to a generated per-record `mochi_eq_<Name>`
	// helper that ANDs each field's comparison together.
	BinEqRec
	BinNeRec
	// List equality. Each operand is TypeList; the result is TypeBool.
	// The emit pass dispatches to a TU-local mochi_eq_list_<elem> helper.
	BinEqList
	BinNeList
	// Map equality. Each operand is TypeMap; the result is TypeBool.
	// The emit pass dispatches to a TU-local mochi_eq_map_<K>_<V> helper.
	BinEqMap
	BinNeMap
	// Short-circuit boolean. Each operand is TypeBool; the
	// result is TypeBool. The emitter must lower these so the
	// right-hand side is only evaluated when the left does not
	// already determine the answer.
	BinAndBool
	BinOrBool
	// String concatenation. Each operand is TypeString; the result
	// is TypeString. The emit pass calls mochi_str_cat(a, b).
	BinStrCat
)

// BinaryExpr applies a typed binary operator to two operands.
// The lowerer is responsible for inserting any monomorphisation
// (e.g. picking BinAddI64 vs BinAddF64 based on operand types)
// so the emit pass can pick the C operator from Op alone.
type BinaryExpr struct {
	Op    BinOp
	Left  Expr
	Right Expr
	// Result carries the operator's result type. Stored
	// explicitly so Type() never has to switch on Op, which
	// keeps the verifier and emitter independent of the BinOp
	// enum's value ordering.
	Result Type
}

func (b *BinaryExpr) Type() Type { return b.Result }

// UnOp is the operator of a UnaryExpr.
type UnOp int

const (
	UnInvalid UnOp = iota
	UnNegI64  // -x where x is TypeInt
	UnNegF64  // -x where x is TypeFloat
	UnNotBool // !x where x is TypeBool
)

// UnaryExpr applies a typed unary operator to one operand.
type UnaryExpr struct {
	Op      UnOp
	Operand Expr
	Result  Type
}

func (u *UnaryExpr) Type() Type { return u.Result }

// VarRef reads a previously-declared variable. Phase 2.1 emits the
// variable's mangled C identifier; later phases that introduce
// closure captures may rewrite Name into an env-relative access.
// Phase 3.0 adds RecordName, valid when VarType==TypeRecord. Phase
// 3.1 adds ElemType, valid when VarType==TypeList. Phase 3.4e adds
// ListValueElemType, valid when VarType==TypeMap && ValueType==TypeList.
// Phase 3.4f adds MapElemKeyType and MapElemValueType, valid when
// VarType==TypeList && ElemType==TypeMap.
type VarRef struct {
	Name              string
	VarType           Type
	RecordName        string
	ElemType          Type
	ElemRecordName    string // valid when VarType==TypeList && ElemType==TypeRecord
	InnerElemType     Type   // valid when VarType==TypeList && ElemType==TypeList (Phase 3.4b)
	MapElemKeyType    Type   // valid when VarType==TypeList && ElemType==TypeMap (Phase 3.4f)
	MapElemValueType  Type   // valid when VarType==TypeList && ElemType==TypeMap (Phase 3.4f)
	KeyType           Type    // valid when VarType==TypeMap
	ValueType         Type    // valid when VarType==TypeMap
	ListValueElemType Type    // valid when VarType==TypeMap && ValueType==TypeList (Phase 3.4e)
	FunSig            *FunSig // valid when VarType==TypeFun (Phase 5.0)
}

func (v *VarRef) Type() Type { return v.VarType }

// RecordLit constructs a record value with every field filled in.
// The lowerer enforces that every field of the named record is
// present, no duplicates, no unknowns, and that each Value's type
// matches the declared field type.
type RecordLit struct {
	TypeName string         // record name (matches RecordDecl.Name)
	Fields   []RecordLitArg // in record-decl source order, not Mochi-literal order
}

// RecordLitArg is one (FieldName, Value) pair in a RecordLit. The
// lowerer reorders the user's source-literal arguments into the
// record's declared order so the emit pass can render the C99
// designated init in struct-field order without an extra sort.
type RecordLitArg struct {
	Name  string
	Value Expr
}

func (*RecordLit) Type() Type { return TypeRecord }

// FieldAccess reads one field from a record receiver. The lowerer
// resolves FieldName against the record's declaration, stamps
// Result with the field's type and (when the field is itself a
// record) ResultRecordName with the nested record's name. Phase
// 3.0 rejects nested records in the lowerer so ResultRecordName
// is always empty for 3.0 fixtures; field of TypeRecord is wired
// for the future.
type FieldAccess struct {
	Receiver         Expr   // must produce TypeRecord
	RecordName       string // receiver's record name, captured by the lowerer
	FieldName        string
	Result           Type
	ResultRecordName string
}

func (f *FieldAccess) Type() Type { return f.Result }

// LetStmt declares a fresh, immutable binding and initialises it.
// Mochi `let x = expr` lowers here; the verifier rejects rebinding
// or assignment to a LetStmt name (mutability lives on VarStmt).
// Phase 3.4e adds ListValueElemType, valid when VarType==TypeMap &&
// ValueType==TypeList, carrying the inner scalar list element type.
// Phase 3.4f adds MapElemKeyType and MapElemValueType, valid when
// VarType==TypeList && ElemType==TypeMap.
type LetStmt struct {
	Name              string
	VarType           Type
	RecordName        string // valid when VarType==TypeRecord
	UnionName         string // valid when VarType==TypeUnion (Phase 4)
	ElemType          Type   // valid when VarType==TypeList
	ElemRecordName    string // valid when VarType==TypeList && ElemType==TypeRecord
	InnerElemType     Type   // valid when VarType==TypeList && ElemType==TypeList (Phase 3.4b)
	MapElemKeyType    Type   // valid when VarType==TypeList && ElemType==TypeMap (Phase 3.4f)
	MapElemValueType  Type   // valid when VarType==TypeList && ElemType==TypeMap (Phase 3.4f)
	KeyType           Type   // valid when VarType==TypeMap
	ValueType         Type   // valid when VarType==TypeMap
	ListValueElemType Type    // valid when VarType==TypeMap && ValueType==TypeList (Phase 3.4e)
	FunSig            *FunSig // valid when VarType==TypeFun (Phase 5.0)
	Init              Expr
	Mutable           bool // true for VarStmt-lowered bindings
}

func (*LetStmt) isStmt() {}

// AssignStmt updates a previously-declared mutable binding. The
// verifier ensures Name is in scope, was introduced by a VarStmt
// (Mutable=true), and the Value type matches the binding type.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (*AssignStmt) isStmt() {}

// ListSetStmt sets xs[i] = val in-place. The runtime helper
// bounds-checks `i` and mutates through the heap pointer.
type ListSetStmt struct {
	Name             string
	Index            Expr // must be TypeInt
	Value            Expr // must match ElemType
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
}

func (*ListSetStmt) isStmt() {}

// MapPutStmt inserts or updates m[k] = v in-place. The runtime
// helper receives a pointer to the local struct so it can
// resize the table when a new key is inserted.
type MapPutStmt struct {
	Name      string
	Key       Expr
	Value     Expr
	KeyType   Type
	ValueType Type
}

func (*MapPutStmt) isStmt() {}

// IfStmt is a two-armed conditional. else-if chains lower to a
// single Else block whose head is another IfStmt; the verifier
// does not flatten them so the emit pass preserves the source
// shape, which matters for debugger line tables (Phase 16).
type IfStmt struct {
	Cond Expr   // must be TypeBool
	Then *Block // executed when Cond is true
	Else *Block // optional; nil means no else arm
}

func (*IfStmt) isStmt() {}

// WhileStmt is a pre-test loop. The body executes while Cond
// evaluates true. BreakStmt and ContinueStmt inside Body refer
// to the nearest enclosing loop; the verifier enforces that they
// appear only in loop scope.
type WhileStmt struct {
	Cond Expr   // must be TypeBool
	Body *Block
}

func (*WhileStmt) isStmt() {}

// ForRangeStmt iterates Var over the half-open integer interval
// [Start, End). Phase 2.2 only covers the int-range form of Mochi's
// `for x in start..end`; list iteration lands with Phase 3.
//
// The induction variable is treated as immutable inside the body
// (assignment to Var is rejected), matching Mochi reference
// semantics. BreakStmt / ContinueStmt inside Body refer to this
// loop; the verifier increments its loop-depth counter accordingly.
type ForRangeStmt struct {
	Var   string
	Start Expr // must be TypeInt
	End   Expr // must be TypeInt
	Body  *Block
}

func (*ForRangeStmt) isStmt() {}

// BreakStmt exits the nearest enclosing WhileStmt (Phase 2.2 will
// extend to ForStmt). The verifier rejects BreakStmt outside a
// loop scope.
type BreakStmt struct{}

func (*BreakStmt) isStmt() {}

// ContinueStmt restarts the nearest enclosing loop at the
// condition test. Same scope rules as BreakStmt.
type ContinueStmt struct{}

func (*ContinueStmt) isStmt() {}

// ReturnStmt exits the enclosing function. A nil Value is a bare
// return; the verifier requires it iff the enclosing function
// returns TypeUnit. A non-nil Value must produce the function's
// declared ReturnType. Phase 2.2 widens this to value-returning
// user functions.
type ReturnStmt struct {
	Value Expr // nil for void return
}

func (*ReturnStmt) isStmt() {}

// ListLit constructs a list value with a fresh backing buffer.
// The lowerer requires every element to share ElemType (the four
// scalar primitives, Phase 3.1; Phase 3.4 widens to TypeRecord with
// ElemRecordName naming the element record) and stamps ElemType /
// ElemRecordName onto the node; the emitter renders this as a
// `mochi_list_<T>_lit` call for scalar elements or
// `mochi_list_<R>_lit` for record elements. Phase 3.4f adds
// MapElemKeyType and MapElemValueType for list<map<K,V>>.
type ListLit struct {
	ElemType         Type
	ElemRecordName   string // valid when ElemType==TypeRecord
	InnerElemType    Type   // valid when ElemType==TypeList (Phase 3.4b list<list<T>>)
	MapElemKeyType   Type   // valid when ElemType==TypeMap (Phase 3.4f list<map<K,V>>)
	MapElemValueType Type   // valid when ElemType==TypeMap (Phase 3.4f list<map<K,V>>)
	Elems            []Expr
}

func (*ListLit) Type() Type { return TypeList }

// IndexExpr reads `Receiver[Index]` for a list-typed receiver. The
// verifier checks Receiver.Type()==TypeList, Index.Type()==TypeInt,
// and stamps Result with the receiver's ElemType (carried as
// ElemType here too for emit-time helper-suffix selection). Bounds
// are checked at runtime inside the per-T `_index` helper. Phase 3.4
// adds ElemRecordName for list<R> receivers; the helper returns a
// `struct mochi_<R>` by value. Phase 3.4f adds MapElemKeyType and
// MapElemValueType for list<map<K,V>> receivers.
type IndexExpr struct {
	Receiver       Expr
	Index          Expr
	ElemType       Type
	ElemRecordName string // valid when ElemType==TypeRecord
	// InnerElemType is set when this IndexExpr produces a
	// list value (i.e., the receiver was list<list<T>>); it
	// carries the inner T so downstream IR can resolve helper
	// suffixes for further operations on the produced list.
	InnerElemType    Type
	MapElemKeyType   Type // valid when ElemType==TypeMap (Phase 3.4f)
	MapElemValueType Type // valid when ElemType==TypeMap (Phase 3.4f)
}

func (i *IndexExpr) Type() Type { return i.ElemType }

// LenExpr is the `len(xs)` builtin call when xs is a list. The
// verifier checks Receiver.Type()==TypeList and stamps the result
// as TypeInt. ElemType is carried so the emitter can pick the
// `_len` helper suffix; Phase 3.4 adds ElemRecordName for list<R>
// receivers so the suffix can resolve to the per-record helper.
// Phase 3.4f adds MapElemKeyType and MapElemValueType for list<map<K,V>>.
type LenExpr struct {
	Receiver         Expr
	ElemType         Type
	ElemRecordName   string // valid when ElemType==TypeRecord
	InnerElemType    Type   // valid when ElemType==TypeList (Phase 3.4b)
	MapElemKeyType   Type   // valid when ElemType==TypeMap (Phase 3.4f)
	MapElemValueType Type   // valid when ElemType==TypeMap (Phase 3.4f)
}

func (*LenExpr) Type() Type { return TypeInt }

// StrLenExpr is the `len(s)` builtin call when s is a string.
// The verifier checks Receiver.Type()==TypeString; the emitter
// renders this as (int64_t)strlen(s). Phase 6.0.
type StrLenExpr struct {
	Receiver Expr
}

func (*StrLenExpr) Type() Type { return TypeInt }

// NumCastExpr is the `int(x)` builtin that truncates a float to int.
// The emitter renders this as `(int64_t)(operand)`. Phase 2.5.
type NumCastExpr struct {
	Operand Expr // TypeFloat
}

func (*NumCastExpr) Type() Type { return TypeInt }

// ListMinExpr is the `min(xs)` builtin that returns the minimum element
// of a list. The emitter calls `mochi_list_<T>_min(xs)`. Phase 2.5.
type ListMinExpr struct {
	Receiver         Expr
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
}

func (e *ListMinExpr) Type() Type { return e.ElemType }

// ListMaxExpr is the `max(xs)` builtin that returns the maximum element
// of a list. The emitter calls `mochi_list_<T>_max(xs)`. Phase 2.5.
type ListMaxExpr struct {
	Receiver         Expr
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
}

func (e *ListMaxExpr) Type() Type { return e.ElemType }

// ListContainsExpr is the `val in list<T>` membership test. The emitter
// calls `mochi_list_<T>_contains(xs, val)` which returns 1 if val is in
// xs and 0 otherwise. Phase 2.6.
type ListContainsExpr struct {
	List     Expr
	Value    Expr
	ElemType Type // element type of the list (int, float, bool, string)
}

func (*ListContainsExpr) Type() Type { return TypeBool }

// ListSumExpr is the `sum(xs)` builtin that returns the sum of list
// elements. The emitter calls `mochi_list_<T>_sum(xs)`. Phase 2.6.
// ElemType determines int vs float return type.
type ListSumExpr struct {
	Receiver Expr
	ElemType Type // TypeInt or TypeFloat
}

func (e *ListSumExpr) Type() Type { return e.ElemType }

// MathCallExpr is an inline math builtin (abs, floor, ceil) that maps
// 1:1 to a C math.h function. The emitter renders it as
// `<Func>(operand)` with an appropriate cast. Phase 2.6.
// Func is one of: "abs_i64", "abs_f64", "floor", "ceil".
type MathCallExpr struct {
	Func   string // "abs_i64", "abs_f64", "floor", "ceil"
	Arg    Expr
	Result Type // TypeInt or TypeFloat
}

func (e *MathCallExpr) Type() Type { return e.Result }

// StrIndexExpr is the `s[i]` operation on a string. The emitter calls
// mochi_str_index(s, i), which returns a freshly allocated one-codepoint
// string (or "" on out-of-bounds). Phase 6.1.
type StrIndexExpr struct {
	Receiver Expr
	Index    Expr
}

func (*StrIndexExpr) Type() Type { return TypeString }

// StrContainsExpr is the `s.contains(sub)` method. The emitter calls
// mochi_str_contains(s, sub), which wraps strstr. Phase 6.1.
type StrContainsExpr struct {
	Receiver Expr
	Sub      Expr
}

func (*StrContainsExpr) Type() Type { return TypeBool }

// StrSubstringExpr is the `substring(s, start, end)` builtin. The
// emitter calls mochi_str_substring(s, start, end), which slices by
// rune index (matching vm3). Phase 6.1.
type StrSubstringExpr struct {
	Receiver Expr
	Start    Expr
	End      Expr
}

func (*StrSubstringExpr) Type() Type { return TypeString }

// StrReverseExpr is the `reverse(s)` builtin on strings. The emitter
// calls mochi_str_reverse(s). Phase 6.1.
type StrReverseExpr struct {
	Receiver Expr
}

func (*StrReverseExpr) Type() Type { return TypeString }

// StrConvertExpr is the `str(x)` builtin that converts an int, float,
// bool, or string to its string representation. The verifier accepts any
// scalar operand type. The emitter dispatches to mochi_str_from_i64,
// mochi_str_from_f64, mochi_str_from_bool, or identity for string.
// Phase 6.2.
type StrConvertExpr struct {
	Operand Expr // TypeInt | TypeFloat | TypeBool | TypeString
}

func (*StrConvertExpr) Type() Type { return TypeString }

// StrUpperExpr is the `upper(s)` builtin. The emitter calls
// mochi_str_upper(s). ASCII-only in Phase 6.3.
type StrUpperExpr struct {
	Receiver Expr
}

func (*StrUpperExpr) Type() Type { return TypeString }

// StrLowerExpr is the `lower(s)` builtin. The emitter calls
// mochi_str_lower(s). ASCII-only in Phase 6.3.
type StrLowerExpr struct {
	Receiver Expr
}

func (*StrLowerExpr) Type() Type { return TypeString }

// StrSplitExpr is the `split(s, sep)` builtin. Returns list<string>.
// The emitter calls mochi_str_split(s, sep). Phase 6.3.
type StrSplitExpr struct {
	Str Expr // TypeString
	Sep Expr // TypeString
}

func (*StrSplitExpr) Type() Type { return TypeList }

// StrJoinExpr is the `join(xs, sep)` builtin. Returns a string.
// The emitter calls mochi_str_join(xs, sep). Phase 6.3.
type StrJoinExpr struct {
	List Expr // TypeList (ElemType==TypeString)
	Sep  Expr // TypeString
}

func (*StrJoinExpr) Type() Type { return TypeString }

// StrMethodRef is a transient IR node produced during lowering when
// the lower pass processes a field access like `s.contains` on a
// string-typed receiver. It is never emitted; lowerPostfix replaces it
// with the appropriate Str*Expr when it sees the following CallOp.
type StrMethodRef struct {
	Receiver   Expr
	MethodName string
}

func (*StrMethodRef) Type() Type { return TypeInvalid }

// AppendExpr is the `append(xs, v)` builtin call. The verifier
// checks Receiver.Type()==TypeList, Value.Type()==ElemType, and
// stamps the result as TypeList with the same ElemType. The
// emitter renders this as a `mochi_list_<T>_append` call; the
// helper allocates a new buffer and returns a fresh list value,
// so the input is never mutated (functional append semantics).
// Phase 3.4 adds ElemRecordName for list<R> receivers. Phase 3.4f
// adds MapElemKeyType and MapElemValueType for list<map<K,V>>.
type AppendExpr struct {
	Receiver         Expr
	Value            Expr
	ElemType         Type
	ElemRecordName   string // valid when ElemType==TypeRecord
	InnerElemType    Type   // valid when ElemType==TypeList (Phase 3.4b)
	MapElemKeyType   Type   // valid when ElemType==TypeMap (Phase 3.4f)
	MapElemValueType Type   // valid when ElemType==TypeMap (Phase 3.4f)
}

func (a *AppendExpr) Type() Type { return TypeList }

// ListSortAscExpr sorts a list in ascending order and returns a new
// list. Phase 8.1 lowers `order by x` in a query expression.
// The emitter renders this as `mochi_list_<T>_sort_asc(xs)`.
type ListSortAscExpr struct {
	Receiver         Expr
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
}

func (e *ListSortAscExpr) Type() Type { return TypeList }

// ListSliceExpr slices a list from Start to End (exclusive, clamped).
// Phase 8.1 lowers `skip N` / `take N` in a query expression.
// The emitter renders this as `mochi_list_<T>_slice(xs, start, end)`.
type ListSliceExpr struct {
	Receiver         Expr
	Start            Expr // int64 expression
	End              Expr // int64 expression
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
}

func (e *ListSliceExpr) Type() Type { return TypeList }

// ForEachStmt iterates Var over the elements of a list-typed List
// expression. Phase 3.1's Mochi surface `for x in xs { ... }` lowers
// here. The induction variable is registered as immutable inside
// Body's scope with type ElemType; BreakStmt / ContinueStmt inside
// Body refer to this loop. The emitter compiles to a C `for` loop
// over indices [0, List.len) reading `List.data[i]` once per
// iteration. Phase 3.4 adds ElemRecordName for list<R> iteration.
// Phase 3.4f adds MapElemKeyType and MapElemValueType for
// list<map<K,V>> iteration.
type ForEachStmt struct {
	Var              string
	List             Expr
	ElemType         Type
	ElemRecordName   string // valid when ElemType==TypeRecord
	InnerElemType    Type   // valid when ElemType==TypeList (Phase 3.4b)
	MapElemKeyType   Type   // valid when ElemType==TypeMap (Phase 3.4f)
	MapElemValueType Type   // valid when ElemType==TypeMap (Phase 3.4f)
	Body             *Block
}

func (*ForEachStmt) isStmt() {}

// MapLit constructs a map value from parallel Keys + Values slices.
// The lowerer requires len(Keys)==len(Values) and all keys to share
// KeyType, all values to share ValueType (both drawn from the
// Phase 3.2 instantiation set). The emitter renders this as a
// `mochi_map_<K>_<V>_lit` call with two C99 compound-literal
// arrays carrying the key and value sequences.
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapLit struct {
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
	Keys              []Expr
	Values            []Expr
}

func (*MapLit) Type() Type { return TypeMap }

// MapGetExpr reads `Receiver[Key]` for a map-typed receiver. The
// verifier checks Receiver.Type()==TypeMap, Key.Type()==KeyType,
// and stamps the result type as ValueType. The runtime helper
// panics with mochi_panic_index() when Key is absent; programs that
// must probe should use MapHasExpr first.
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapGetExpr struct {
	Receiver          Expr
	Key               Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
}

func (m *MapGetExpr) Type() Type { return m.ValueType }

// MapHasExpr is the `has(m, k)` builtin call. Result is TypeBool;
// the runtime helper returns 1 if k is in m and 0 otherwise.
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapHasExpr struct {
	Receiver          Expr
	Key               Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
}

func (*MapHasExpr) Type() Type { return TypeBool }

// MapLenExpr is the `len(m)` builtin call when m is a map. Result
// is TypeInt; the helper returns the live-entry count.
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapLenExpr struct {
	Receiver          Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
}

func (*MapLenExpr) Type() Type { return TypeInt }

// MapKeysExpr is the `keys(m)` builtin call. Result is list<K>
// sorted ascending by key (matches the vm's sort-on-iteration
// behavior so AOT-C output stays byte-equal to the oracle).
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapKeysExpr struct {
	Receiver          Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
}

func (k *MapKeysExpr) Type() Type { return TypeList }

// MapValuesExpr is the `values(m)` builtin call. Result is list<V>
// in the same key-sorted order as MapKeysExpr.
// Phase 3.4e adds ListValueElemType, valid when ValueType==TypeList.
type MapValuesExpr struct {
	Receiver          Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
}

func (v *MapValuesExpr) Type() Type { return TypeList }

// ---- Phase 4: sum types and Maranget pattern matching ----

// UnionDecl declares one sum type (tagged union). Each variant maps to
// a uint8_t tag value and an anonymous struct inside the C union body.
// Phase 4.0 restricts variant fields to scalar primitives and records;
// later sub-phases widen to nested lists and maps.
type UnionDecl struct {
	Name     string
	Variants []VariantDecl
}

// FunSig describes a function type's parameter and return types.
// Phase 5.0 restricts to scalar primitives (int, float, bool, string)
// and unit returns. Complex types (record, union, list, map) are deferred.
type FunSig struct {
	ParamTypes []Type // each must be a scalar primitive in Phase 5.0
	ReturnType Type   // scalar primitive or TypeUnit
}

// FunTypeName returns the C typedef name for this function signature.
// Phase 5.1 changed the prefix from mochi_fnptr_ to mochi_closure_ to
// reflect the fat-pointer struct (fn + env) that every closure value uses.
// Format: mochi_closure_<p0>_<p1>_..._to_<ret>; no params: mochi_closure_to_<ret>.
func (sig *FunSig) FunTypeName() string {
	if len(sig.ParamTypes) == 0 {
		return "mochi_closure_to_" + funTypeAbbrev(sig.ReturnType)
	}
	paramParts := make([]string, len(sig.ParamTypes))
	for i, pt := range sig.ParamTypes {
		paramParts[i] = funTypeAbbrev(pt)
	}
	return "mochi_closure_" + strings.Join(paramParts, "_") + "_to_" + funTypeAbbrev(sig.ReturnType)
}

// funTypeAbbrev returns the abbreviated C type suffix used in function
// pointer typedef names. Only scalar primitives and unit are supported
// in Phase 5.0.
func funTypeAbbrev(t Type) string {
	switch t {
	case TypeInt:
		return "i64"
	case TypeFloat:
		return "f64"
	case TypeBool:
		return "bool"
	case TypeString:
		return "str"
	case TypeUnit:
		return "void"
	default:
		return "unknown"
	}
}

// VariantDecl is one named variant inside a UnionDecl.
type VariantDecl struct {
	// Name is the Mochi variant name; the emitter mangles it into
	// the C union member and the constructor function name.
	Name string
	// Tag is the uint8_t discriminant value assigned by the lowerer
	// in declaration order (first variant gets 0, second gets 1, ...).
	Tag    uint8
	Fields []VariantField
}

// VariantField is one named field of a variant.
type VariantField struct {
	Name string
	// FieldType is the monomorphic aotir type of the field value.
	FieldType Type
	// RecordName carries the record identity when FieldType==TypeRecord.
	RecordName string
	// UnionName carries the union identity when FieldType==TypeUnion.
	UnionName string
}

// VariantLit constructs a union-typed value. The lowerer stamps Tag
// and UnionName from the resolved declaration so the emitter can pick
// the correct constructor and variant member without re-resolving.
type VariantLit struct {
	UnionName   string
	VariantName string
	Tag         uint8
	Fields      []VariantLitArg
}

// VariantLitArg is one (FieldName, Value) pair in a VariantLit.
type VariantLitArg struct {
	Name  string
	Value Expr
}

func (*VariantLit) Type() Type { return TypeUnion }

// UnionVarRef reads a union-typed variable. UnionName carries the
// union's identity so downstream IR can resolve helper names and
// emit the right C type.
type UnionVarRef struct {
	Name      string
	UnionName string
}

func (*UnionVarRef) Type() Type { return TypeUnion }

// VariantFieldAccess reads one field from a union-typed receiver that
// is known (at lower time) to hold a specific variant. The emitter
// renders this as `val.u.<Variant>.<Field>`.
type VariantFieldAccess struct {
	Receiver    Expr // TypeUnion
	UnionName   string
	VariantName string
	FieldName   string
	Result      Type
	RecordName  string // valid when Result==TypeRecord
}

func (v *VariantFieldAccess) Type() Type { return v.Result }

// MatchArm is one case arm inside a MatchStmt. VariantName is empty
// for the wildcard arm (_). Bindings are the field-name → C-variable
// mappings generated by the lowerer for the pattern variables.
type MatchArm struct {
	VariantName string
	Tag         uint8
	Bindings    []MatchBinding
	Body        *Block
}

// MatchBinding maps a pattern variable to the union field it aliases.
type MatchBinding struct {
	VarName    string // the Mochi pattern variable name
	FieldName  string // the variant field being bound
	FieldType  Type
	RecordName string // valid when FieldType==TypeRecord
}

// MatchStmt lowers a Mochi `match` expression to a tagged switch.
// When ResultVar is non-empty, each arm's Body ends with an assignment
// to that mutable C variable; the emit pass also declares the variable
// above the switch with ResultType.
type MatchStmt struct {
	Target     Expr
	UnionName  string
	Arms       []MatchArm
	Default    *MatchArm // wildcard (_) arm; nil if absent
	ResultVar  string    // non-empty when match is used as an expression
	ResultType Type      // valid when ResultVar is non-empty
	// ResultUnionName is the union name when ResultType==TypeUnion.
	ResultUnionName string
	// ResultRecordName is the record name when ResultType==TypeRecord.
	ResultRecordName string
}

func (*MatchStmt) isStmt() {}

// ---- Phase 5.0: non-capturing closures ----
// ---- Phase 5.1: capturing closures ----

// FunCapture describes one variable captured from the enclosing scope.
// The lowerer populates this when it detects a free variable reference
// inside a closure body. The emitter uses it to fill in the env struct
// typedef and the malloc+fill sequence before the closure value.
type FunCapture struct {
	// FieldName is the C struct member name (same as the Mochi variable
	// name with no mangling, since captured names are already valid C
	// identifiers after the parser).
	FieldName string
	// VarType is the aotir type of the captured variable.
	VarType Type
	// SrcName is the Mochi variable name in the enclosing scope, used to
	// emit the initializer `__env->FieldName = SrcName;`.
	SrcName string
}

// ClosureEnvStmt allocates and fills a closure environment struct before
// the FunLit that captures it. The lowerer emits this immediately before
// the LetStmt that binds the closure value.
//
// The emitter renders:
//
//	<EnvTypeName> *<EnvVarName> = malloc(sizeof(<EnvTypeName>));
//	<EnvVarName>-><field0> = <src0>;
//	...
type ClosureEnvStmt struct {
	EnvTypeName string       // e.g. "__anon_2_env_t"
	EnvVarName  string       // e.g. "__anon_2_env"
	Captures    []FunCapture // captured variables in order
}

func (*ClosureEnvStmt) isStmt() {}

// FunLit represents a closure literal. During lowering, the closure body
// is lifted to a top-level aotir.Function. FunLit holds the lifted
// function's name, its type signature, and (for capturing closures) the
// environment variable to thread through.
type FunLit struct {
	FuncName    string       // name of the lifted function (e.g. __anon_1)
	Sig         *FunSig      // type signature of the anonymous function
	Captures    []FunCapture // non-empty for capturing closures (Phase 5.1)
	EnvTypeName string       // C typedef name for the env struct; empty if non-capturing
	EnvVarName  string       // C variable holding the env pointer; empty if non-capturing
}

func (f *FunLit) Type() Type { return TypeFun }

// FunCallExpr calls a function-typed value (a variable or literal of
// TypeFun). Callee is a TypeFun expression (VarRef, FunLit, etc.).
// Args are the arguments. Result is the return type of the call.
type FunCallExpr struct {
	Callee Expr   // TypeFun expression
	Args   []Expr // call arguments
	Result Type   // return type of the call (from Sig.ReturnType)
}

func (f *FunCallExpr) Type() Type { return f.Result }

// ReadFileExpr reads the entire content of a file and returns it as a
// string. The emitter calls mochi_read_file(path). Phase 6.5.
type ReadFileExpr struct{ Path Expr }

func (*ReadFileExpr) Type() Type { return TypeString }

// WriteFileStmt writes content to a file (creating or truncating it).
// The emitter calls mochi_write_file(path, content). Phase 6.5.
type WriteFileStmt struct {
	Path    Expr // TypeString
	Content Expr // TypeString
}

func (*WriteFileStmt) isStmt() {}

// AppendFileStmt appends content to a file.
// The emitter calls mochi_append_file(path, content). Phase 6.5.
type AppendFileStmt struct {
	Path    Expr // TypeString
	Content Expr // TypeString
}

func (*AppendFileStmt) isStmt() {}

// LinesExpr reads a file and returns each line as list<string>, stripping
// the trailing newline delimiter. The emitter calls mochi_lines(path).
// Phase 6.5.
type LinesExpr struct{ Path Expr }

func (*LinesExpr) Type() Type { return TypeList }

// LoadCSVExpr reads a CSV file and returns a list<list<string>> where
// each outer element is a row and each inner element is a cell value.
// The emitter calls the TU-local static helper __mochi_load_csv(path)
// which is emitted when any LoadCSVExpr is present in the program.
// Phase 8.4.
type LoadCSVExpr struct{ Path Expr }

func (*LoadCSVExpr) Type() Type { return TypeList }

// SaveCSVStmt writes a list<list<string>> to a CSV file, one row per
// line with cells separated by commas (RFC 4180 quoting applied when
// a cell contains a comma, double-quote, or newline). The emitter
// calls the TU-local static helper __mochi_save_csv(path, data).
// Phase 8.4.
type SaveCSVStmt struct {
	Path Expr // TypeString
	Data Expr // TypeList, ElemType==TypeList, InnerElemType==TypeString
}

func (*SaveCSVStmt) isStmt() {}

// QueryScopeStmt wraps the desugared query pipeline in an arena scope.
// Phase 8.3.
//
// The lowerer emits this node instead of emitting the LetStmt + ForEachStmt
// directly into the current block. The emitter:
//  1. Stack-allocates a mochi_arena_t and calls mochi_arena_init.
//  2. Declares the result list with zero capacity.
//  3. Emits Body (the ForEachStmt + optional sort/slice steps); any
//     AssignStmt whose value is AppendExpr targeting ResultVar uses
//     mochi_list_<T>_append_arena instead of the heap version.
//  4. Copies the result list to the heap via mochi_list_<T>_copy_heap.
//  5. Calls mochi_arena_free.
//
// ElemType, ElemRecordName, InnerElemType, MapElemKeyType, MapElemValueType
// mirror the corresponding fields on ListLit / AppendExpr and are needed so
// the emitter can build the correct mochi_list_* suffix.
type QueryScopeStmt struct {
	ResultVar        string // the __queryN temp variable (declared OUTSIDE this scope)
	ArenaVar         string // C variable name for the mochi_arena_t (__qaN)
	ElemType         Type
	ElemRecordName   string
	InnerElemType    Type
	MapElemKeyType   Type
	MapElemValueType Type
	Body             *Block // ForEachStmt(s) + optional sort/slice (no LetStmt for ResultVar)
}

func (*QueryScopeStmt) isStmt() {}
