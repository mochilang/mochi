// Phase 2 widens tstree with let / const declarations, assignment,
// if / while / for / break / continue, and binary / unary
// expressions. All nodes obey the same printer contract as Phase 1:
// two-space indentation, no trailing whitespace, semicolon-terminated
// statements.
package tstree

import (
	"strings"
)

// LetDecl is `const NAME: TYPE = INIT;` (when Mutable=false) or
// `let NAME: TYPE = INIT;` (when Mutable=true). The emitter always
// renders the explicit type annotation so `tsc --strict` never has
// to infer through a runtime-helper boundary.
//
// When Init is nil, the LetDecl renders as
// `let NAME!: TYPE;` (definite assignment assertion). Phase 5 uses
// this form for the match-as-expression result temp: the C lowerer
// pre-emits a mutable LetStmt with nil Init, and the match arms each
// assign to it. The `!` tells tsc the binding is provably assigned
// before use, which the exhaustiveness trap (mochiUnreachable in
// the default arm) makes true at the type level. A nil-Init LetDecl
// with Mutable=false is a build error — there is no syntax for
// `const NAME!: TYPE;` (const requires an initialiser).
type LetDecl struct {
	Name    string
	Type    string
	Init    Expr // nil means "definite-assignment assertion" form
	Mutable bool
}

func (s *LetDecl) stmtNode() {}
func (s *LetDecl) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	keyword := "const"
	if s.Mutable {
		keyword = "let"
	}
	if s.Init == nil {
		return pad + keyword + " " + s.Name + "!: " + s.Type + ";"
	}
	return pad + keyword + " " + s.Name + ": " + s.Type + " = " + s.Init.TsString(0) + ";"
}

// AssignStmt is `NAME = VALUE;`. Phase 2 only emits this for
// `var`-introduced mutable bindings (Mochi enforces that `let` is
// immutable). The TS side enforces the same via `const`.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) stmtNode() {}
func (s *AssignStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + s.Name + " = " + s.Value.TsString(0) + ";"
}

// IfStmt is `if (COND) { THEN } else { ELSE }`. The Else block may
// be nil (no else arm). Else-if chains land here too: an `else if`
// is a non-empty Else block whose single statement is another IfStmt
// rendered without an outer brace pair.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt // nil = no else; non-nil but possibly length-1 containing another IfStmt
}

func (s *IfStmt) stmtNode() {}
func (s *IfStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	inner := strings.Repeat("  ", indent+1)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("if (")
	b.WriteString(s.Cond.TsString(0))
	b.WriteString(") {")
	if len(s.Then) == 0 {
		b.WriteByte('}')
	} else {
		b.WriteByte('\n')
		for _, t := range s.Then {
			b.WriteString(t.TsString(indent + 1))
			b.WriteByte('\n')
		}
		b.WriteString(pad)
		b.WriteByte('}')
	}
	if s.Else != nil {
		// If the else arm is exactly one IfStmt, render as `else if`
		// (no extra braces, no extra indent on the inner if).
		if len(s.Else) == 1 {
			if inner2, ok := s.Else[0].(*IfStmt); ok {
				_ = inner
				b.WriteString(" else ")
				// Render the inner IfStmt at indent 0 then prepend
				// the chained context: drop its leading indent and
				// glue it onto the same line.
				nested := inner2.TsString(indent)
				b.WriteString(strings.TrimPrefix(nested, pad))
				return b.String()
			}
		}
		b.WriteString(" else {")
		if len(s.Else) == 0 {
			b.WriteByte('}')
		} else {
			b.WriteByte('\n')
			for _, t := range s.Else {
				b.WriteString(t.TsString(indent + 1))
				b.WriteByte('\n')
			}
			b.WriteString(pad)
			b.WriteByte('}')
		}
	}
	return b.String()
}

// WhileStmt is `while (COND) { BODY }`.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (s *WhileStmt) stmtNode() {}
func (s *WhileStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("while (")
	b.WriteString(s.Cond.TsString(0))
	b.WriteString(") {")
	if len(s.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, t := range s.Body {
		b.WriteString(t.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// ForRangeStmt is `for (let VAR = START; VAR < END; VAR++) { BODY }`.
// The induction variable is always declared with `let` (not `const`)
// because the `++` rewrite is a re-binding; Mochi treats VAR as
// immutable inside Body and the IR rejects any explicit AssignStmt
// targeting VAR.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (s *ForRangeStmt) stmtNode() {}
func (s *ForRangeStmt) TsString(indent int) string {
	pad := strings.Repeat("  ", indent)
	var b strings.Builder
	b.WriteString(pad)
	b.WriteString("for (let ")
	b.WriteString(s.Var)
	b.WriteString(": number = ")
	b.WriteString(s.Start.TsString(0))
	b.WriteString("; ")
	b.WriteString(s.Var)
	b.WriteString(" < ")
	b.WriteString(s.End.TsString(0))
	b.WriteString("; ")
	b.WriteString(s.Var)
	b.WriteString("++) {")
	if len(s.Body) == 0 {
		b.WriteByte('}')
		return b.String()
	}
	b.WriteByte('\n')
	for _, t := range s.Body {
		b.WriteString(t.TsString(indent + 1))
		b.WriteByte('\n')
	}
	b.WriteString(pad)
	b.WriteByte('}')
	return b.String()
}

// BreakStmt is `break;`.
type BreakStmt struct{}

func (s *BreakStmt) stmtNode() {}
func (s *BreakStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + "break;"
}

// ContinueStmt is `continue;`.
type ContinueStmt struct{}

func (s *ContinueStmt) stmtNode() {}
func (s *ContinueStmt) TsString(indent int) string {
	return strings.Repeat("  ", indent) + "continue;"
}

// BinaryExpr is `LEFT OP RIGHT`. Op carries the raw TS operator
// (e.g. "+", "===", "&&"); the lowerer is responsible for choosing
// the correct TS operator from the aotir BinOp. The printer wraps
// the whole expression in parentheses so precedence is unambiguous
// for nested operators (`(a + b) * c`). Top-level parentheses only
// appear when this expression is itself nested inside another
// expression; the LetDecl / ReturnStmt / call-arg renderers do not
// add their own.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (e *BinaryExpr) exprNode() {}
func (e *BinaryExpr) TsString(_ int) string {
	return "(" + e.Left.TsString(0) + " " + e.Op + " " + e.Right.TsString(0) + ")"
}

// UnaryExpr is `OP OPERAND` (prefix). Op carries the raw TS operator
// (e.g. "-", "!"). The printer wraps the operand in parentheses so
// `--x` does not get confused with the decrement operator when the
// operand is itself a negation.
type UnaryExpr struct {
	Op      string
	Operand Expr
}

func (e *UnaryExpr) exprNode() {}
func (e *UnaryExpr) TsString(_ int) string {
	return e.Op + "(" + e.Operand.TsString(0) + ")"
}

// MemberCallExpr is `RECEIVER.METHOD(ARGS)`. Used for string
// method calls (`s.includes(t)`) and similar. Distinct from
// CallExpr (which takes a free-standing callee Expr) because the
// printer must not parenthesise the receiver into the call line.
type MemberCallExpr struct {
	Receiver Expr
	Method   string
	Args     []Expr
}

func (e *MemberCallExpr) exprNode() {}
func (e *MemberCallExpr) TsString(_ int) string {
	var b strings.Builder
	b.WriteString(e.Receiver.TsString(0))
	b.WriteByte('.')
	b.WriteString(e.Method)
	b.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(a.TsString(0))
	}
	b.WriteByte(')')
	return b.String()
}
