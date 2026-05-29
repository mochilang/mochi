package pysrc

import (
	"fmt"
	"strconv"
	"strings"
)

// Module is the top-level Python source file.
type Module struct {
	// FutureAnnotations emits `from __future__ import annotations` first.
	FutureAnnotations bool
	// Imports is the sorted list of import statements (ruff `I` rule sorts these).
	Imports []ImportStmt
	// Stmts is the body of the module after imports.
	Stmts []Stmt
}

// PySource returns the rendered .py source text terminated with a trailing newline.
func (m *Module) PySource() string {
	var sb strings.Builder
	if m.FutureAnnotations {
		sb.WriteString("from __future__ import annotations\n")
	}
	if m.FutureAnnotations && len(m.Imports) > 0 {
		sb.WriteByte('\n')
	}
	for _, imp := range m.Imports {
		sb.WriteString(imp.PyString(0))
		sb.WriteByte('\n')
	}
	// PEP 8: two blank lines between top-level imports and the first
	// top-level definition, and between successive top-level definitions.
	for i, s := range m.Stmts {
		if i == 0 {
			sb.WriteString("\n\n")
		} else {
			sb.WriteString("\n\n")
		}
		sb.WriteString(s.PyString(0))
		sb.WriteByte('\n')
	}
	return sb.String()
}

// ImportStmt is `from <module> import <names>` or `import <module>`.
type ImportStmt struct {
	// From is empty for plain `import x`; set for `from x import y, z`.
	From string
	// Names lists imported names in source order.
	Names []string
}

// PyString renders the import statement.
func (i ImportStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if i.From == "" {
		return pad + "import " + strings.Join(i.Names, ", ")
	}
	return pad + "from " + i.From + " import " + strings.Join(i.Names, ", ")
}

// Stmt is one statement.
type Stmt interface {
	isStmt()
	PyString(indent int) string
}

// FunctionDef is `def name(...) -> ret:`.
type FunctionDef struct {
	Name       string
	Params     []Param
	ReturnType TypeRef
	Body       []Stmt
	// Decorators lists @decorator lines above the def.
	Decorators []string
	// Async emits `async def` instead of `def` (Phase 9+).
	Async bool
}

func (*FunctionDef) isStmt() {}

// Param is one formal parameter `name: Type` or `name: Type = default`.
type Param struct {
	Name    string
	Type    TypeRef
	Default Expr
}

// PyString renders the function definition.
func (f *FunctionDef) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	for _, dec := range f.Decorators {
		sb.WriteString(pad)
		sb.WriteByte('@')
		sb.WriteString(dec)
		sb.WriteByte('\n')
	}
	sb.WriteString(pad)
	if f.Async {
		sb.WriteString("async ")
	}
	sb.WriteString("def ")
	sb.WriteString(f.Name)
	sb.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name)
		if p.Type.Name != "" {
			sb.WriteString(": ")
			sb.WriteString(p.Type.PyString())
		}
		if p.Default != nil {
			sb.WriteString(" = ")
			sb.WriteString(p.Default.PyString())
		}
	}
	sb.WriteByte(')')
	if f.ReturnType.Name != "" {
		sb.WriteString(" -> ")
		sb.WriteString(f.ReturnType.PyString())
	}
	sb.WriteString(":\n")
	if len(f.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	} else {
		for i, s := range f.Body {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(s.PyString(indent + 1))
		}
	}
	return sb.String()
}

// IfStmt is `if cond:` and optional `else:` block.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (*IfStmt) isStmt() {}

// PyString renders the if statement.
func (s *IfStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("if ")
	sb.WriteString(s.Cond.PyString())
	sb.WriteString(":\n")
	for i, st := range s.Then {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Else) > 0 {
		sb.WriteByte('\n')
		sb.WriteString(pad)
		sb.WriteString("else:\n")
		for i, st := range s.Else {
			if i > 0 {
				sb.WriteByte('\n')
			}
			sb.WriteString(st.PyString(indent + 1))
		}
	}
	return sb.String()
}

// ExprStmt is an expression evaluated for its side effect.
type ExprStmt struct {
	X Expr
}

func (*ExprStmt) isStmt() {}

// PyString renders the expression statement.
func (s *ExprStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + s.X.PyString()
}

// AssignStmt is `name: Type = value` (PEP 526 annotated) or `name = value`.
type AssignStmt struct {
	Target string
	Type   TypeRef
	Value  Expr
}

func (*AssignStmt) isStmt() {}

// PyString renders the assignment statement.
func (s *AssignStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if s.Type.Name != "" {
		return fmt.Sprintf("%s%s: %s = %s", pad, s.Target, s.Type.PyString(), s.Value.PyString())
	}
	return fmt.Sprintf("%s%s = %s", pad, s.Target, s.Value.PyString())
}

// ReturnStmt is `return value` or bare `return`.
type ReturnStmt struct {
	Value Expr
}

func (*ReturnStmt) isStmt() {}

// PyString renders the return statement.
func (s *ReturnStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if s.Value == nil {
		return pad + "return"
	}
	return pad + "return " + s.Value.PyString()
}

// PassStmt is the no-op `pass`.
type PassStmt struct{}

func (*PassStmt) isStmt() {}

// PyString renders the pass statement.
func (s *PassStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "pass"
}

// Expr is one expression.
type Expr interface {
	isExpr()
	PyString() string
}

// Call is `f(args, kw=v)`.
type Call struct {
	Func Expr
	Args []Expr
}

func (*Call) isExpr() {}

// PyString renders the call expression.
func (c *Call) PyString() string {
	var sb strings.Builder
	sb.WriteString(c.Func.PyString())
	sb.WriteByte('(')
	for i, a := range c.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.PyString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// BinaryEq is `left == right`. Kept as an alias for clarity at the
// __name__ == "__main__" guard site; equivalent to a BinaryExpr with Op="==".
type BinaryEq struct {
	Left  Expr
	Right Expr
}

func (*BinaryEq) isExpr() {}

// PyString renders the equality comparison.
func (b *BinaryEq) PyString() string {
	return b.Left.PyString() + " == " + b.Right.PyString()
}

// BinaryExpr is `left op right` for arithmetic, comparison, and boolean
// operators. Phase 2 only emits the operator forms; the operator string
// must already be the Python token (`+`, `-`, `*`, `/`, `//`, `%`,
// `==`, `!=`, `<`, `<=`, `>`, `>=`, `and`, `or`).
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (*BinaryExpr) isExpr() {}

// PyString renders the binary expression, parenthesising children so
// nested arithmetic and boolean expressions print unambiguously.
// Phase 2 keeps a conservative bracket policy (always parenthesise),
// matching what `ruff format` produces for these structures.
func (b *BinaryExpr) PyString() string {
	switch b.Op {
	case "and", "or":
		return "(" + b.Left.PyString() + " " + b.Op + " " + b.Right.PyString() + ")"
	}
	return "(" + b.Left.PyString() + " " + b.Op + " " + b.Right.PyString() + ")"
}

// UnaryExpr is `op operand`. Phase 2 ships `-` (negation) and `not`.
type UnaryExpr struct {
	Op      string
	Operand Expr
}

func (*UnaryExpr) isExpr() {}

// PyString renders the unary expression.
func (u *UnaryExpr) PyString() string {
	if u.Op == "not" {
		return "(not " + u.Operand.PyString() + ")"
	}
	return "(" + u.Op + u.Operand.PyString() + ")"
}

// IndexExpr is `receiver[index]`.
type IndexExpr struct {
	Receiver Expr
	Index    Expr
}

func (*IndexExpr) isExpr() {}

// PyString renders the index expression.
func (i *IndexExpr) PyString() string {
	return i.Receiver.PyString() + "[" + i.Index.PyString() + "]"
}

// WhileStmt is `while cond:` followed by a body.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) isStmt() {}

// PyString renders the while statement.
func (s *WhileStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("while ")
	sb.WriteString(s.Cond.PyString())
	sb.WriteString(":\n")
	for i, st := range s.Body {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	}
	return sb.String()
}

// ForRangeStmt is `for var in range(start, end):`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (*ForRangeStmt) isStmt() {}

// PyString renders the for-range statement.
func (s *ForRangeStmt) PyString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("for ")
	sb.WriteString(s.Var)
	sb.WriteString(" in range(")
	sb.WriteString(s.Start.PyString())
	sb.WriteString(", ")
	sb.WriteString(s.End.PyString())
	sb.WriteString("):\n")
	for i, st := range s.Body {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(st.PyString(indent + 1))
	}
	if len(s.Body) == 0 {
		sb.WriteString(pad)
		sb.WriteString("    pass")
	}
	return sb.String()
}

// BreakStmt is `break`.
type BreakStmt struct{}

func (*BreakStmt) isStmt() {}

// PyString renders `break`.
func (s *BreakStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "break"
}

// ContinueStmt is `continue`.
type ContinueStmt struct{}

func (*ContinueStmt) isStmt() {}

// PyString renders `continue`.
func (s *ContinueStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + "continue"
}

// ReassignStmt is plain `target = value` without a PEP 526 annotation,
// used when reassigning an already-declared mutable variable.
type ReassignStmt struct {
	Target string
	Value  Expr
}

func (*ReassignStmt) isStmt() {}

// PyString renders the bare assignment.
func (s *ReassignStmt) PyString(indent int) string {
	return strings.Repeat("    ", indent) + s.Target + " = " + s.Value.PyString()
}

// Name is a bare identifier.
type Name struct {
	Id string
}

func (*Name) isExpr() {}

// PyString returns the identifier.
func (n *Name) PyString() string { return n.Id }

// Attribute is `value.attr`.
type Attribute struct {
	Value Expr
	Attr  string
}

func (*Attribute) isExpr() {}

// PyString renders the attribute access.
func (a *Attribute) PyString() string {
	return a.Value.PyString() + "." + a.Attr
}

// StrLit is a Python string literal. The renderer uses strconv.Quote
// which produces a Go-style double-quoted string; this happens to be
// valid Python because both languages use the same escape table for
// the characters Phase 1 exercises (\n, \t, \\, \", \xNN). When Phase
// 5+ introduces interpolation, this widens to a JoinedStr node.
type StrLit struct {
	Value string
}

func (*StrLit) isExpr() {}

// PyString returns a double-quoted Python string literal.
func (s *StrLit) PyString() string {
	return strconv.Quote(s.Value)
}

// IntLit is a Python integer literal. Mochi int (int64) is Python int (arbitrary precision).
type IntLit struct {
	Value int64
}

func (*IntLit) isExpr() {}

// PyString returns the decimal int literal.
func (i *IntLit) PyString() string {
	return strconv.FormatInt(i.Value, 10)
}

// FloatLit is a Python float literal. Mochi float (binary64) is Python float (binary64).
type FloatLit struct {
	Value float64
}

func (*FloatLit) isExpr() {}

// PyString returns the float literal. NaN/Inf canonicalisation is deferred to Phase 2.1.
func (f *FloatLit) PyString() string {
	return strconv.FormatFloat(f.Value, 'g', -1, 64)
}

// BoolLit is `True` or `False`.
type BoolLit struct {
	Value bool
}

func (*BoolLit) isExpr() {}

// PyString returns "True" or "False".
func (b *BoolLit) PyString() string {
	if b.Value {
		return "True"
	}
	return "False"
}

// TypeRef is a Python annotation reference (e.g. "int", "str", "None", "list[int]").
type TypeRef struct {
	Name string
}

// PyString returns the type name verbatim.
func (t TypeRef) PyString() string { return t.Name }

// Predefined type refs.
var (
	TypeNone  = TypeRef{Name: "None"}
	TypeInt   = TypeRef{Name: "int"}
	TypeFloat = TypeRef{Name: "float"}
	TypeBool  = TypeRef{Name: "bool"}
	TypeStr   = TypeRef{Name: "str"}
)
