package rtree

import (
	"fmt"
	"strconv"
	"strings"
)

func indent(n int) string {
	return strings.Repeat("    ", n)
}

// ---- File ----

// File is the top-level unit; rendered as a complete .rs file.
type File struct {
	Name  string  // filename base (no .rs extension)
	Uses  []*Use  // `use` declarations, emitted in slice order
	Items []Item  // top-level items in slice order
}

// RustSource returns the rust source text for this file.
func (f *File) RustSource() string {
	var sb strings.Builder
	for _, u := range f.Uses {
		sb.WriteString(u.RustString(0))
		sb.WriteString("\n")
	}
	if len(f.Uses) > 0 {
		sb.WriteString("\n")
	}
	for i, it := range f.Items {
		if i > 0 {
			sb.WriteString("\n")
		}
		sb.WriteString(it.RustString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

// ---- Use ----

// Use is a `use path;` declaration.
type Use struct {
	Path string
}

func (u *Use) RustString(ind int) string {
	return fmt.Sprintf("%suse %s;", indent(ind), u.Path)
}

// ---- Item interface ----

// Item is any top-level item (fn, struct, enum, impl, mod, use).
type Item interface {
	rustItem()
	RustString(ind int) string
}

// ---- FnDecl ----

// FnDecl is a function declaration.
type FnDecl struct {
	Attributes []string  // e.g. ["#[tokio::main]"]
	Visibility string    // "", "pub", "pub(crate)"
	IsAsync    bool
	Name       string
	Params     []FnParam
	ReturnType string    // empty means Unit (no annotation)
	Body       []Stmt
}

// FnParam is one parameter of a FnDecl.
type FnParam struct {
	Name     string
	TypeName string
}

func (*FnDecl) rustItem() {}

func (f *FnDecl) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	for _, a := range f.Attributes {
		fmt.Fprintf(&sb, "%s%s\n", pad, a)
	}
	sb.WriteString(pad)
	if f.Visibility != "" {
		sb.WriteString(f.Visibility)
		sb.WriteByte(' ')
	}
	if f.IsAsync {
		sb.WriteString("async ")
	}
	sb.WriteString("fn ")
	sb.WriteString(f.Name)
	sb.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		fmt.Fprintf(&sb, "%s: %s", p.Name, p.TypeName)
	}
	sb.WriteByte(')')
	if f.ReturnType != "" {
		fmt.Fprintf(&sb, " -> %s", f.ReturnType)
	}
	sb.WriteString(" {\n")
	for _, s := range f.Body {
		sb.WriteString(s.RustString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- StructDecl ----

// StructDecl is a Rust struct declaration.
type StructDecl struct {
	Derives    []string  // e.g. ["Debug", "Clone"]
	Visibility string
	Name       string
	Fields     []StructField
}

// StructField is one field of a StructDecl.
type StructField struct {
	Visibility string
	Name       string
	TypeName   string
}

func (*StructDecl) rustItem() {}

func (s *StructDecl) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	if len(s.Derives) > 0 {
		fmt.Fprintf(&sb, "%s#[derive(%s)]\n", pad, strings.Join(s.Derives, ", "))
	}
	sb.WriteString(pad)
	if s.Visibility != "" {
		sb.WriteString(s.Visibility)
		sb.WriteByte(' ')
	}
	fmt.Fprintf(&sb, "struct %s {\n", s.Name)
	for _, f := range s.Fields {
		sb.WriteString(indent(ind + 1))
		if f.Visibility != "" {
			sb.WriteString(f.Visibility)
			sb.WriteByte(' ')
		}
		fmt.Fprintf(&sb, "%s: %s,\n", f.Name, f.TypeName)
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- EnumDecl ----

// EnumDecl is a Rust enum declaration (used for sum types in Phase 5).
type EnumDecl struct {
	Derives    []string
	Visibility string
	Name       string
	Variants   []EnumVariant
}

// EnumVariant is one variant of an EnumDecl.
type EnumVariant struct {
	Name   string
	Fields []StructField // empty for unit variants
}

func (*EnumDecl) rustItem() {}

func (e *EnumDecl) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	if len(e.Derives) > 0 {
		fmt.Fprintf(&sb, "%s#[derive(%s)]\n", pad, strings.Join(e.Derives, ", "))
	}
	sb.WriteString(pad)
	if e.Visibility != "" {
		sb.WriteString(e.Visibility)
		sb.WriteByte(' ')
	}
	fmt.Fprintf(&sb, "enum %s {\n", e.Name)
	for _, v := range e.Variants {
		sb.WriteString(indent(ind + 1))
		sb.WriteString(v.Name)
		if len(v.Fields) > 0 {
			sb.WriteString(" { ")
			for i, f := range v.Fields {
				if i > 0 {
					sb.WriteString(", ")
				}
				fmt.Fprintf(&sb, "%s: %s", f.Name, f.TypeName)
			}
			sb.WriteString(" }")
		}
		sb.WriteString(",\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- Stmt interface ----

// Stmt is a function-body statement.
type Stmt interface {
	rustStmt()
	RustString(ind int) string
}

// ---- LetStmt ----

// LetStmt is `let [mut] name[: ty] = expr;`.
type LetStmt struct {
	Mutable  bool
	Name     string
	TypeName string // empty = inferred
	Value    Expr
}

func (*LetStmt) rustStmt() {}

func (l *LetStmt) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad + "let ")
	if l.Mutable {
		sb.WriteString("mut ")
	}
	sb.WriteString(l.Name)
	if l.TypeName != "" {
		fmt.Fprintf(&sb, ": %s", l.TypeName)
	}
	if l.Value != nil {
		fmt.Fprintf(&sb, " = %s", l.Value.RustExpr())
	}
	sb.WriteByte(';')
	return sb.String()
}

// ---- AssignStmt ----

// AssignStmt is `target = value;`.
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (*AssignStmt) rustStmt() {}

func (a *AssignStmt) RustString(ind int) string {
	return fmt.Sprintf("%s%s = %s;", indent(ind), a.Target.RustExpr(), a.Value.RustExpr())
}

// ---- ExprStmt ----

// ExprStmt is an expression evaluated for its side effect.
type ExprStmt struct {
	Expr Expr
}

func (*ExprStmt) rustStmt() {}

func (e *ExprStmt) RustString(ind int) string {
	return fmt.Sprintf("%s%s;", indent(ind), e.Expr.RustExpr())
}

// ---- ReturnStmt ----

// ReturnStmt is `return [expr];`.
type ReturnStmt struct {
	Value Expr // nil = bare return
}

func (*ReturnStmt) rustStmt() {}

func (r *ReturnStmt) RustString(ind int) string {
	if r.Value == nil {
		return indent(ind) + "return;"
	}
	return fmt.Sprintf("%sreturn %s;", indent(ind), r.Value.RustExpr())
}

// ---- IfStmt ----

// IfStmt is `if cond { then } [else { else_ }]`.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (*IfStmt) rustStmt() {}

func (i *IfStmt) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sif %s {\n", pad, i.Cond.RustExpr())
	for _, s := range i.Then {
		sb.WriteString(s.RustString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	if len(i.Else) > 0 {
		sb.WriteString(" else {\n")
		for _, s := range i.Else {
			sb.WriteString(s.RustString(ind + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad + "}")
	}
	return sb.String()
}

// ---- WhileStmt ----

// WhileStmt is `while cond { body }`.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (*WhileStmt) rustStmt() {}

func (w *WhileStmt) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%swhile %s {\n", pad, w.Cond.RustExpr())
	for _, s := range w.Body {
		sb.WriteString(s.RustString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- ForRangeStmt ----

// ForRangeStmt is `for var in start..end { body }`.
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (*ForRangeStmt) rustStmt() {}

func (f *ForRangeStmt) RustString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sfor %s in %s..%s {\n", pad, f.Var, f.Start.RustExpr(), f.End.RustExpr())
	for _, s := range f.Body {
		sb.WriteString(s.RustString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- BreakStmt / ContinueStmt ----

// BreakStmt is `break;`.
type BreakStmt struct{}

func (*BreakStmt) rustStmt() {}

func (*BreakStmt) RustString(ind int) string { return indent(ind) + "break;" }

// ContinueStmt is `continue;`.
type ContinueStmt struct{}

func (*ContinueStmt) rustStmt() {}

func (*ContinueStmt) RustString(ind int) string { return indent(ind) + "continue;" }

// ---- Expr interface ----

// Expr is a value-producing expression.
type Expr interface {
	rustExpr()
	RustExpr() string
}

// ---- Literals ----

// StringLit is a string literal (rendered with Rust's escape rules).
type StringLit struct {
	Value string
}

func (*StringLit) rustExpr() {}

func (s *StringLit) RustExpr() string {
	return strconv.Quote(s.Value)
}

// IntLit is a 64-bit signed integer literal.
type IntLit struct {
	Value int64
}

func (*IntLit) rustExpr() {}

func (i *IntLit) RustExpr() string {
	return fmt.Sprintf("%d_i64", i.Value)
}

// FloatLit is an IEEE 754 binary64 literal.
type FloatLit struct {
	Value float64
}

func (*FloatLit) rustExpr() {}

func (f *FloatLit) RustExpr() string {
	s := strconv.FormatFloat(f.Value, 'g', -1, 64)
	if !strings.ContainsAny(s, ".eE") && !strings.ContainsAny(s, "iIn") {
		s += "_f64"
	} else {
		s += "_f64"
	}
	return s
}

// BoolLit is a true/false literal.
type BoolLit struct {
	Value bool
}

func (*BoolLit) rustExpr() {}

func (b *BoolLit) RustExpr() string {
	if b.Value {
		return "true"
	}
	return "false"
}

// ---- Ident ----

// Ident is a bare identifier reference.
type Ident struct {
	Name string
}

func (*Ident) rustExpr() {}

func (i *Ident) RustExpr() string {
	return i.Name
}

// ---- CallExpr ----

// CallExpr is `func(args...)`.
type CallExpr struct {
	Func string
	Args []Expr
}

func (*CallExpr) rustExpr() {}

func (c *CallExpr) RustExpr() string {
	var sb strings.Builder
	sb.WriteString(c.Func)
	sb.WriteByte('(')
	for i, a := range c.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.RustExpr())
	}
	sb.WriteByte(')')
	return sb.String()
}

// ---- MacroCall ----

// MacroCall is `macro!(args...)` (e.g. println!).
type MacroCall struct {
	Name string
	Args []Expr
}

func (*MacroCall) rustExpr() {}

func (m *MacroCall) RustExpr() string {
	var sb strings.Builder
	sb.WriteString(m.Name)
	sb.WriteString("!(")
	for i, a := range m.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.RustExpr())
	}
	sb.WriteByte(')')
	return sb.String()
}

// ---- BinaryExpr ----

// BinOp is the operator of a BinaryExpr.
type BinOp string

const (
	OpAdd BinOp = "+"
	OpSub BinOp = "-"
	OpMul BinOp = "*"
	OpDiv BinOp = "/"
	OpMod BinOp = "%"
	OpEq  BinOp = "=="
	OpNe  BinOp = "!="
	OpLt  BinOp = "<"
	OpLe  BinOp = "<="
	OpGt  BinOp = ">"
	OpGe  BinOp = ">="
	OpAnd BinOp = "&&"
	OpOr  BinOp = "||"
)

// BinaryExpr is `left op right`.
type BinaryExpr struct {
	Op    BinOp
	Left  Expr
	Right Expr
}

func (*BinaryExpr) rustExpr() {}

func (b *BinaryExpr) RustExpr() string {
	return fmt.Sprintf("(%s %s %s)", b.Left.RustExpr(), string(b.Op), b.Right.RustExpr())
}

// ---- UnaryExpr ----

// UnaryExpr is `op operand`.
type UnaryExpr struct {
	Op      string // "-", "!"
	Operand Expr
}

func (*UnaryExpr) rustExpr() {}

func (u *UnaryExpr) RustExpr() string {
	return fmt.Sprintf("(%s%s)", u.Op, u.Operand.RustExpr())
}

// ---- MethodCall ----

// MethodCall is `receiver.method(args...)`.
type MethodCall struct {
	Receiver Expr
	Method   string
	Args     []Expr
}

func (*MethodCall) rustExpr() {}

func (m *MethodCall) RustExpr() string {
	var sb strings.Builder
	fmt.Fprintf(&sb, "%s.%s(", m.Receiver.RustExpr(), m.Method)
	for i, a := range m.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.RustExpr())
	}
	sb.WriteByte(')')
	return sb.String()
}

// ---- FieldAccess ----

// FieldAccess is `receiver.field`.
type FieldAccess struct {
	Receiver Expr
	Field    string
}

func (*FieldAccess) rustExpr() {}

func (f *FieldAccess) RustExpr() string {
	return fmt.Sprintf("%s.%s", f.Receiver.RustExpr(), f.Field)
}
