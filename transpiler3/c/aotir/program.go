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

// Function is one monomorphic, closure-converted callable.
type Function struct {
	// Name is the mangled, emit-stable identifier. The mangling
	// scheme reserves an unambiguous mapping from Mochi name +
	// type arguments to a C identifier; the verifier checks
	// uniqueness across the Program.
	Name string

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
// symbol set.
type CallStmt struct {
	// Func is the mangled callee name.
	Func string

	// Args carries the call arguments in source order. Each
	// expression's Type must match the corresponding parameter
	// type of the resolved callee.
	Args []Expr
}

func (*CallStmt) isStmt() {}

// Expr is a value-producing aotir node. Phase 1 ships only
// StringLit; Phase 2 introduces IntLit, BoolLit, FloatLit and
// the arithmetic / comparison operations.
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

// Type implements Expr.
func (*StringLit) Type() Type { return TypeString }
