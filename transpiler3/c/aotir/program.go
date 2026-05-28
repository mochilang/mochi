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

	// ExternFuncs lists C extern function declarations in source order.
	// Phase 10.0 adds this; emit walks it to write `extern <ctype> <name>(...);`
	// declarations in the generated C prologue.
	ExternFuncs []*ExternFuncDecl

	// Agents lists user-declared agent types in source order.
	// Phase 9.3 adds this; emit walks it to write a C struct typedef
	// and per-intent static functions for synchronous dispatch.
	Agents []*AgentDecl

	// GoFuncs lists Go FFI function declarations in source order.
	// Phase 10.2 adds this; emit walks it to write inline C wrapper
	// functions that forward calls to the Go companion via mochi_go_rpc_*.
	GoFuncs []*GoFuncDecl

	// PythonFuncs lists Python FFI function declarations in source order.
	// Phase 10.3 adds this; emit walks it to write inline C wrapper
	// functions that forward calls to the Python companion via mochi_py_rpc_*.
	PythonFuncs []*PythonFuncDecl

	// JSFuncs lists JavaScript FFI function declarations in source order.
	// Phase 10.4 adds this; emit walks it to write inline C wrapper
	// functions that forward calls to the JS companion via mochi_js_rpc_*.
	JSFuncs []*JSFuncDecl

	// JavaFuncs lists Java FFI function declarations in source order.
	// Phase 12.0 adds this; the JVM lowerer uses it to emit direct static or
	// instance calls to the Java class method.
	JavaFuncs []*JavaFuncDecl

	// Datalog holds facts and rules accumulated from `fact`/`rule` declarations.
	// Phase 8.0 adds this so the BEAM lowerer can implement compile-time evaluation.
	Datalog *DatalogProgram
}

// GoFuncDecl describes a Go FFI function. Phase 10.2.
// The lowerer populates this from `extern go fun` declarations;
// the emitter writes a static inline C wrapper per entry.
type GoFuncDecl struct {
	Name       string
	Params     []Param
	ReturnType Type
}

// PythonFuncDecl describes a Python FFI function. Phase 10.3.
// The lowerer populates this from `extern python fun` declarations;
// the emitter writes a static inline C wrapper per entry.
type PythonFuncDecl struct {
	Name       string
	Params     []Param
	ReturnType Type
}

// JSFuncDecl describes a JavaScript FFI function. Phase 10.4.
// The lowerer populates this from `extern js fun` declarations;
// the emitter writes a static inline C wrapper per entry.
type JSFuncDecl struct {
	Name       string
	Params     []Param
	ReturnType Type
}

// JavaFuncDecl describes a Java FFI function. Phase 12.0.
// The JVM lowerer populates this from `extern java fun` declarations.
type JavaFuncDecl struct {
	ClassName  string // e.g. "java.util.UUID"
	MethodName string // e.g. "randomUUID"
	MochiName  string // the Mochi alias, e.g. "uuid_new"
	Params     []Param
	ReturnType Type
	IsStatic   bool // true for static methods (Phase 12.0 only handles static)
}

// ExternFuncDecl describes a C extern function declaration. Phase 10.0.
// The lowerer populates this from `extern fun` declarations in the source;
// the emitter renders each entry as `extern <ReturnType> <Name>(<params>);`.
type ExternFuncDecl struct {
	Name       string
	Params     []Param
	ReturnType Type
	// ReturnRecord carries the record identity when ReturnType==TypeRecord.
	ReturnRecord string
	// OrigName is the original Mochi-source name (may contain dots for
	// dotted calls like "lists.reverse"). Phase 12.1: the BEAM lowerer uses
	// this to emit the correct module:function/arity call instead of a local
	// apply. Empty for plain (non-dotted) extern declarations.
	OrigName string
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
	ChanElemType      Type    // valid when VarType==TypeChan (Phase 9.1)
	StreamElemType    Type    // valid when VarType==TypeStream (Phase 9.2)
	SubElemType       Type    // valid when VarType==TypeSub (Phase 9.2)
	FutureElemType    Type    // valid when VarType==TypeFuture (Phase 11.0)
	AgentName         string  // valid when VarType==TypeAgent (Phase 9.3)
	IsSpawnedRef      bool    // valid when VarType==TypeAgent: true when binding came from `spawn` (Phase 9.1)
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
	ChanElemType      Type    // valid when VarType==TypeChan (Phase 9.1)
	StreamElemType    Type    // valid when VarType==TypeStream (Phase 9.2)
	SubElemType       Type    // valid when VarType==TypeSub (Phase 9.2)
	FutureElemType    Type    // valid when VarType==TypeFuture (Phase 11.0)
	AgentName         string  // valid when VarType==TypeAgent (Phase 9.3)
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
	Name              string
	Key               Expr
	Value             Expr
	KeyType           Type
	ValueType         Type
	ListValueElemType Type // valid when ValueType==TypeList (Phase 3.4e)
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

// TryCatchStmt lowers Mochi's `try { ... } catch e { ... }` to a
// setjmp/longjmp frame via the Phase 7.0 runtime.
// BufName is the unique C variable name for the jmp_buf (e.g. "__mochi_buf_0").
type TryCatchStmt struct {
	BufName   string // unique jmp_buf variable name
	TryBody   *Block
	CatchVar  string // variable bound to mochi_except_code in catch scope
	CatchBody *Block
}

func (*TryCatchStmt) isStmt() {}

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

// JsonDecodeExpr is the `json_decode(s)` builtin — Phase 14.2.
// On BEAM it lowers to mochi_json:decode/1 which uses OTP 27 json:decode/1.
// The result is typed as map<string, string>: top-level JSON object fields
// with non-string values are coerced to their string representations.
type JsonDecodeExpr struct{ Input Expr }

func (*JsonDecodeExpr) Type() Type { return TypeMap }

// ListMapExpr is the `map(xs, fn)` builtin — Phase 6.1.
// On BEAM it lowers to lists:map/2; on C it lowers to a manual loop.
type ListMapExpr struct {
	List     Expr
	Fn       Expr
	ElemType Type // element type of the result list
}

func (e *ListMapExpr) Type() Type { return TypeList }

// ListFilterExpr is the `filter(xs, fn)` builtin — Phase 6.1.
// On BEAM it lowers to lists:filter/2; on C it lowers to a manual loop.
type ListFilterExpr struct {
	List     Expr
	Fn       Expr
	ElemType Type // element type preserved from the input list
}

func (e *ListFilterExpr) Type() Type { return TypeList }

// ListFoldlExpr is the `reduce(xs, fn, init)` builtin — Phase 6.1.
// On BEAM it lowers to lists:foldl/3; on C it lowers to a manual loop.
type ListFoldlExpr struct {
	List    Expr
	Fn      Expr
	Init    Expr
	AccType Type // type of the accumulator / return value
}

func (e *ListFoldlExpr) Type() Type { return e.AccType }

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

// ---- Phase 3.3: set type (OTP sets module v2) ----

// SetLiteralExpr constructs a set value from a list of elements.
// The BEAM lowerer renders this as sets:from_list([Elem1, Elem2, ...]).
type SetLiteralExpr struct {
	Elems    []Expr
	ElemType Type
}

func (*SetLiteralExpr) Type() Type { return TypeSet }

// SetAddExpr is the `add(s, x)` builtin call. Result is TypeSet;
// the BEAM lowerer renders this as sets:add_element(X, S).
type SetAddExpr struct {
	Receiver Expr
	Elem     Expr
	ElemType Type
}

func (*SetAddExpr) Type() Type { return TypeSet }

// SetHasExpr is the `has(s, x)` or `x in s` builtin call for sets.
// Result is TypeBool; the BEAM lowerer renders this as sets:is_element(X, S).
type SetHasExpr struct {
	Receiver Expr
	Elem     Expr
	ElemType Type
}

func (*SetHasExpr) Type() Type { return TypeBool }

// SetLenExpr is the `len(s)` builtin call for sets.
// Result is TypeInt; the BEAM lowerer renders this as sets:size(S).
type SetLenExpr struct {
	Receiver Expr
	ElemType Type
}

func (*SetLenExpr) Type() Type { return TypeInt }

// SetToListExpr converts a set to a list for iteration.
// The BEAM lowerer renders this as sets:to_list(S).
// Result is TypeList with the same ElemType as the set.
type SetToListExpr struct {
	Receiver Expr
	ElemType Type
}

func (*SetToListExpr) Type() Type { return TypeList }

// ---- Phase 3.4 (omap): ordered-map type backed by OTP orddict ----

// OMapLiteralExpr constructs an ordered-map value from a list of key-value pairs.
// The BEAM lowerer renders this as orddict:from_list([{K1,V1},{K2,V2},...]).
type OMapLiteralExpr struct {
	Keys      []Expr
	Values    []Expr
	KeyType   Type
	ValueType Type
}

func (*OMapLiteralExpr) Type() Type { return TypeOMap }

// OMapGetExpr reads `Receiver[Key]` for an omap-typed receiver.
// The BEAM lowerer renders this as orddict:fetch(Key, Receiver).
type OMapGetExpr struct {
	Receiver  Expr
	Key       Expr
	KeyType   Type
	ValueType Type
}

func (e *OMapGetExpr) Type() Type { return e.ValueType }

// OMapSetExpr stores a key-value pair into an omap: `Receiver[Key] = Value`.
// The BEAM lowerer renders this as orddict:store(Key, Value, Receiver).
// The result (new omap) is bound back to the variable via OMapPutStmt.
type OMapSetExpr struct {
	Receiver  Expr
	Key       Expr
	Value     Expr
	KeyType   Type
	ValueType Type
}

func (*OMapSetExpr) Type() Type { return TypeOMap }

// OMapHasExpr is the `has(m, k)` builtin call for an omap receiver.
// The BEAM lowerer renders this as orddict:is_key(Key, Receiver).
type OMapHasExpr struct {
	Receiver  Expr
	Key       Expr
	KeyType   Type
	ValueType Type
}

func (*OMapHasExpr) Type() Type { return TypeBool }

// OMapLenExpr is the `len(m)` builtin call when m is an omap.
// orddict is a list, so the BEAM lowerer renders this as erlang:length(M).
type OMapLenExpr struct {
	Receiver  Expr
	KeyType   Type
	ValueType Type
}

func (*OMapLenExpr) Type() Type { return TypeInt }

// OMapPutStmt is the statement form of `m[k] = v` for omap receivers.
// The BEAM lowerer rebinds M to orddict:store(K, V, M).
type OMapPutStmt struct {
	Name      string // variable name of the omap
	Key       Expr
	Value     Expr
	KeyType   Type
	ValueType Type
}

func (*OMapPutStmt) isStmt() {}

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
// Guard is non-nil for arms with a `when <expr>` guard clause (Phase 5.1).
type MatchArm struct {
	VariantName string
	Tag         uint8
	Bindings    []MatchBinding
	Guard       Expr // nil when no guard (always-match); bool-typed expression when present
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

// HttpGetExpr performs an HTTP GET and returns the response body as a string.
// On BEAM it calls mochi_fetch:get/1 (httpc-backed). Phase 14.0.
type HttpGetExpr struct{ URL Expr }

func (*HttpGetExpr) Type() Type { return TypeString }

// AsyncExpr wraps a body expression in a spawned process via
// mochi_async:async/1. The result is a future reference (TypeFuture).
// Phase 11.0.
type AsyncExpr struct {
	Body     Expr // the expression to evaluate asynchronously
	ElemType Type // the element type T of the resulting future<T>
}

func (*AsyncExpr) Type() Type { return TypeFuture }

// AwaitExpr blocks until a future resolves and returns the result.
// On BEAM it calls mochi_async:await/1. Phase 11.1.
type AwaitExpr struct {
	Future   Expr // TypeFuture expression
	ElemType Type // the element type T; this is the Type() of AwaitExpr
}

func (e *AwaitExpr) Type() Type { return e.ElemType }

// JavaCallExpr is a call to a Java FFI method. Phase 12.0.
// The JVM lowerer emits this as a direct Java static or instance call.
type JavaCallExpr struct {
	Decl   *JavaFuncDecl
	Args   []Expr
	Result Type
}

func (e *JavaCallExpr) Type() Type { return e.Result }

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

// LLMGenerateExpr calls mochi_llm_generate(provider, model, prompt) and
// returns the text response as a string. Phase 14.0.
//
// Provider is the provider name literal ("openai", "anthropic", etc.).
// Model and Prompt are string-typed sub-expressions.
// In cassette mode (MOCHI_LLM_CASSETTE_DIR env var), the runtime
// replays a pre-recorded response; in live mode it requires Phase 14.1+.
type LLMGenerateExpr struct {
	Provider string // e.g. "openai", "anthropic", "google", "llama"
	Model    Expr   // TypeString; "" means provider default
	Prompt   Expr   // TypeString
}

func (*LLMGenerateExpr) Type() Type { return TypeString }

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

// PanicStmt lowers `panic(code, msg)` to `mochi_raise(code, msg);`.
// It never returns; Phase 7.3.
type PanicStmt struct {
	Code Expr // TypeInt
	Msg  Expr // TypeString
}

func (*PanicStmt) isStmt() {}

// RawCStmt carries a pre-rendered C statement block. Used by phases that
// generate complex C structures (Datalog eval, Phase 15) without threading
// a full IR sub-language through verify and emit.
// The emitter writes Code verbatim at the correct indentation level.
type RawCStmt struct {
	Code string // one or more C statements; caller is responsible for correctness
}

func (*RawCStmt) isStmt() {}

// RawCExpr carries a pre-rendered C expression. Used by phases that
// generate complex C structures without a full IR sub-language.
type RawCExpr struct {
	Code    string // a C expression; caller is responsible for correctness
	RawType Type   // type of the expression for the verifier and emit pass
}

func (e *RawCExpr) Type() Type { return e.RawType }

// ChanMakeExpr creates a bounded ring channel with capacity Cap.
// Type() returns TypeChan; ElemType carries the element type (Phase 9.1).
type ChanMakeExpr struct {
	Cap      Expr // must be TypeInt
	ElemType Type
}

func (e *ChanMakeExpr) Type() Type { return TypeChan }

// ChanSendStmt sends Val into Chan. Blocks (yields) when the channel is full.
// ElemType carries the element type for emit-time typed-wrapper selection (Phase 9.1).
type ChanSendStmt struct {
	Chan     Expr // must be TypeChan
	Val      Expr // must match ElemType
	ElemType Type
}

func (*ChanSendStmt) isStmt() {}

// ChanRecvExpr receives one value from Chan. Blocks (yields) when the channel is empty.
// Type() returns ElemType (Phase 9.1).
type ChanRecvExpr struct {
	Chan     Expr // must be TypeChan
	ElemType Type
}

func (e *ChanRecvExpr) Type() Type { return e.ElemType }

// --- Phase 9.2: stream<T> IR nodes ---

// StreamMakeExpr creates a bounded MPMC broadcast stream with capacity Cap.
// Type() returns TypeStream (Phase 9.2).
type StreamMakeExpr struct {
	Cap      Expr // must be TypeInt
	ElemType Type
}

func (e *StreamMakeExpr) Type() Type { return TypeStream }

// StreamEmitStmt sends Val to all subscribers of Stream. Blocks (yields) when
// the slowest subscriber has not yet drained its slot. Phase 9.2.
type StreamEmitStmt struct {
	Stream   Expr // must be TypeStream
	Val      Expr
	ElemType Type
}

func (*StreamEmitStmt) isStmt() {}

// SubMakeExpr creates a subscriber handle for Stream starting at the current
// write position. Type() returns TypeSub (Phase 9.2).
type SubMakeExpr struct {
	Stream   Expr // must be TypeStream
	ElemType Type
}

func (e *SubMakeExpr) Type() Type { return TypeSub }

// SubMakeLimitExpr creates a subscriber handle with backpressure: incoming
// messages are dropped when the buffer holds Limit items. Phase 10.2.
type SubMakeLimitExpr struct {
	Stream   Expr // must be TypeStream
	Limit    Expr // must be TypeInt; drop threshold
	ElemType Type
}

func (e *SubMakeLimitExpr) Type() Type { return TypeSub }

// SubRecvExpr receives the next value from subscriber Sub, blocking (yielding)
// when no new data is available. Type() returns ElemType (Phase 9.2).
type SubRecvExpr struct {
	Sub      Expr // must be TypeSub
	ElemType Type
}

func (e *SubRecvExpr) Type() Type { return e.ElemType }

// --- Phase 9.3: agent (synchronous dispatch, struct + functions) ---

// AgentIntentParam is one formal parameter of an intent method.
type AgentIntentParam struct {
	Name string
	Type Type
}

// AgentIntentDecl describes one intent method inside an AgentDecl.
// The Body is the lowered statement block of the intent; the emit pass
// renders it as a static C function `mochi_agent_NAME__INTENT`.
type AgentIntentDecl struct {
	Name       string
	Params     []AgentIntentParam
	ReturnType Type   // TypeUnit for void intents; scalar primitives for value-returning intents
	Body       *Block
}

// AgentDecl declares one agent type. Fields are the mutable state
// (scalar-typed only in Phase 9.3). Intents are the synchronous
// method bodies. OnClose holds the optional terminate body (Phase 9.3).
type AgentDecl struct {
	Name     string
	Fields   []RecordField    // same layout as RecordDecl; scalar types only in Phase 9.3
	Intents  []AgentIntentDecl
	OnClose  *Block           // Phase 9.3: body of the on_close { ... } block; nil if absent
}

// AgentLit constructs an agent value with every field filled in.
// Emitted identically to RecordLit but using the agent struct type.
type AgentLit struct {
	AgentName string
	Fields    []RecordLitArg // in agent-decl source order
}

func (a *AgentLit) Type() Type { return TypeAgent }

// AgentMethodRef is a transient IR node produced during lowering when
// the lower pass processes a field access like `c.increment` on an
// agent-typed receiver. It is never emitted; lowerPostfix replaces it
// with AgentIntentCallExpr/AgentIntentCallStmt when the following CallOp arrives.
type AgentMethodRef struct {
	AgentName  string
	IntentName string
	Receiver   Expr   // TypeAgent variable reference
	ReturnType Type   // return type of the intent
	SpawnedRef bool   // Phase 9.1: true when receiver came from `spawn`
}

func (*AgentMethodRef) Type() Type { return TypeInvalid } // transient

// AgentSpawnExpr creates a supervised gen_server process for the named agent
// type and returns an opaque agent ref (Erlang PID). Phase 9.1.
// The Erlang runtime spawns a message-loop process seeded with InitState.
type AgentSpawnExpr struct {
	AgentName string // e.g. "Counter"
	InitArgs  []Expr // field-value arguments for the initial state map
}

func (*AgentSpawnExpr) Type() Type { return TypeAgent }

// AgentIntentCallExpr is a synchronous intent call that returns a value.
// Emitted as `mochi_agent_NAME__INTENT(&receiver, args...)`.
// When SpawnedRef is true, the receiver is a PID from AgentSpawnExpr and
// the call must use a message-passing protocol instead of functional threading.
type AgentIntentCallExpr struct {
	AgentName  string
	IntentName string
	Receiver   Expr // TypeAgent
	Args       []Expr
	Result     Type // return type of the intent
	SpawnedRef bool // Phase 9.1: true when receiver is a PID from spawn
}

func (e *AgentIntentCallExpr) Type() Type { return e.Result }

// AgentIntentCallStmt is a synchronous intent call that discards the
// return value (or returns TypeUnit). Emitted as
// `mochi_agent_NAME__INTENT(&receiver, args...);`.
// When SpawnedRef is true, the receiver is a PID from AgentSpawnExpr.
type AgentIntentCallStmt struct {
	AgentName  string
	IntentName string
	Receiver   Expr // TypeAgent
	Args       []Expr
	SpawnedRef bool // Phase 9.1: true when receiver is a PID from spawn
}

func (*AgentIntentCallStmt) isStmt() {}

// ---- Phase 8 (BEAM): Datalog IR nodes ----

// DatalogFact is one ground tuple from `fact Name(arg1, ...)`.
// Args are raw string values (unquoted). Phase 8.0.
type DatalogFact struct {
	Name string
	Args []string
}

// DatalogRuleBody is one literal in a Datalog rule body.
type DatalogRuleBody struct {
	Name  string   // relation name (empty when IsNeq)
	Args  []string // var names or "\"literal\""-quoted constants
	IsNot bool     // negation-as-failure (not Pred(...))
	IsNeq bool     // X != Y inequality
	NeqA  string   // left variable when IsNeq
	NeqB  string   // right variable when IsNeq
}

// DatalogRule is one Datalog rule from `rule HeadName(headArgs) :- body`.
type DatalogRule struct {
	HeadName string
	HeadArgs []string // variable names or "\"const\"" constants
	Body     []DatalogRuleBody
}

// DatalogProgram holds all facts and rules at the point of a query.
type DatalogProgram struct {
	Facts []DatalogFact
	Rules []DatalogRule
}

// DatalogQueryExpr evaluates `query Name(args)` and returns list<string>
// (flat: free-variable values from all matching tuples concatenated).
// The C backend uses CResultVar (the variable reference) plus the RawCStmt
// already added to the block; the BEAM backend runs a compile-time Go
// semi-naive evaluator over Prog.
type DatalogQueryExpr struct {
	QueryName string
	QueryArgs []string        // "" = free variable; "\"foo\"" = bound constant
	Prog      *DatalogProgram
	// CResultVar is the C variable name (e.g. "__dl1_result") used by the
	// C backend; the corresponding setup RawCStmt is already in the block.
	CResultVar string
}

func (*DatalogQueryExpr) Type() Type { return TypeList }
