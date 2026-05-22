package aotir

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
}

// Param is one formal parameter of a Function. Phase 2.2 introduces
// user-defined multi-arg functions; before then the only callable
// was main() which took none.
type Param struct {
	Name string
	Type Type
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

	// ReturnType is the function's monomorphic return type.
	ReturnType Type

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
	Func   string
	Args   []Expr
	Result Type
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
	// Short-circuit boolean. Each operand is TypeBool; the
	// result is TypeBool. The emitter must lower these so the
	// right-hand side is only evaluated when the left does not
	// already determine the answer.
	BinAndBool
	BinOrBool
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
type VarRef struct {
	Name    string
	VarType Type
}

func (v *VarRef) Type() Type { return v.VarType }

// LetStmt declares a fresh, immutable binding and initialises it.
// Mochi `let x = expr` lowers here; the verifier rejects rebinding
// or assignment to a LetStmt name (mutability lives on VarStmt).
type LetStmt struct {
	Name     string
	VarType  Type
	Init     Expr
	Mutable  bool // true for VarStmt-lowered bindings
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
