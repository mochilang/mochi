package rtree

import (
	"fmt"
	"strings"
)

// indent returns a string of n*2 spaces (community Ruby style).
func indent(n int) string {
	return strings.Repeat("  ", n)
}

// ---- Top-level ----

// SourceFile is the top-level unit; rendered as a complete .rb file.
type SourceFile struct {
	Name                string   // filename base (no .rb extension)
	FrozenStringLiteral bool     // true => prepend "# frozen_string_literal: true"
	Requires            []string // e.g. ["mochi/runtime"]
	Decls               []Decl
}

// RubySource returns the full Ruby source text for this file.
func (sf *SourceFile) RubySource() string {
	var sb strings.Builder
	if sf.FrozenStringLiteral {
		sb.WriteString("# frozen_string_literal: true\n")
	}
	if len(sf.Requires) > 0 {
		sb.WriteByte('\n')
		for _, r := range sf.Requires {
			fmt.Fprintf(&sb, "require %q\n", r)
		}
	}
	for _, d := range sf.Decls {
		sb.WriteByte('\n')
		sb.WriteString(d.RubyString(0))
		sb.WriteByte('\n')
	}
	return sb.String()
}

// ---- Decl interface ----

// Decl is a top-level (or nested module-level) declaration.
type Decl interface {
	rubyDecl()
	RubyString(ind int) string
}

// ---- ModuleDecl ----

// ModuleDecl is `module Name ... end`.
type ModuleDecl struct {
	Name  string
	Decls []Decl
}

func (*ModuleDecl) rubyDecl() {}

func (m *ModuleDecl) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%smodule %s\n", pad, m.Name)
	for i, d := range m.Decls {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(d.RubyString(ind + 1))
		sb.WriteByte('\n')
	}
	fmt.Fprintf(&sb, "%send", pad)
	return sb.String()
}

// ---- ClassDecl ----

// ClassDecl is `class Name; ... end`. Used for agent declarations whose
// mutable state is held in @ivars and whose intents are instance methods.
type ClassDecl struct {
	Name  string
	Decls []Decl
}

func (*ClassDecl) rubyDecl() {}

func (c *ClassDecl) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sclass %s\n", pad, c.Name)
	for i, d := range c.Decls {
		if i > 0 {
			sb.WriteByte('\n')
		}
		sb.WriteString(d.RubyString(ind + 1))
		sb.WriteByte('\n')
	}
	fmt.Fprintf(&sb, "%send", pad)
	return sb.String()
}

// ---- DataDecl ----

// DataDecl is `Name = Data.define(:f1, :f2, ...)`.
type DataDecl struct {
	Name   string
	Fields []string
}

func (*DataDecl) rubyDecl() {}

func (d *DataDecl) RubyString(ind int) string {
	pad := indent(ind)
	var fs []string
	for _, f := range d.Fields {
		fs = append(fs, ":"+f)
	}
	return fmt.Sprintf("%s%s = Data.define(%s)", pad, d.Name, strings.Join(fs, ", "))
}

// ---- MethodDecl ----

// MethodDecl is a Ruby method definition: `def [self.]name(params); body; end`.
type MethodDecl struct {
	Receiver string // "" for instance method; "self" for module-level singleton method
	Name     string
	Params   []MethodParam
	Body     []Stmt
}

// MethodParam is one formal parameter.
type MethodParam struct {
	Name string
}

func (*MethodDecl) rubyDecl() {}

func (m *MethodDecl) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("def ")
	if m.Receiver != "" {
		sb.WriteString(m.Receiver)
		sb.WriteByte('.')
	}
	sb.WriteString(m.Name)
	if len(m.Params) > 0 {
		sb.WriteByte('(')
		for i, p := range m.Params {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(p.Name)
		}
		sb.WriteByte(')')
	}
	sb.WriteByte('\n')
	for _, s := range m.Body {
		sb.WriteString(s.RubyString(ind + 1))
		sb.WriteByte('\n')
	}
	fmt.Fprintf(&sb, "%send", pad)
	return sb.String()
}

// ---- RawDecl ----

// RawDecl is a raw multi-line declaration injected verbatim. Used sparingly
// for things like sealed-module sum declarations whose canonical shape would
// require many node kinds to model precisely.
type RawDecl struct {
	Text string
}

func (*RawDecl) rubyDecl() {}

func (r *RawDecl) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	lines := strings.Split(strings.TrimRight(r.Text, "\n"), "\n")
	for i, l := range lines {
		if i > 0 {
			sb.WriteByte('\n')
		}
		if l == "" {
			continue
		}
		sb.WriteString(pad)
		sb.WriteString(l)
	}
	return sb.String()
}

// ---- Stmt interface ----

// Stmt is a statement in a method body or block.
type Stmt interface {
	rubyStmt()
	RubyString(ind int) string
}

// ExprStmt wraps an expression evaluated for its side effect.
type ExprStmt struct {
	X Expr
}

func (*ExprStmt) rubyStmt() {}

func (s *ExprStmt) RubyString(ind int) string {
	return indent(ind) + s.X.RubyExprString()
}

// Assign is `lhs = rhs`.
type Assign struct {
	LHS string
	RHS Expr
}

func (*Assign) rubyStmt() {}

func (s *Assign) RubyString(ind int) string {
	return fmt.Sprintf("%s%s = %s", indent(ind), s.LHS, s.RHS.RubyExprString())
}

// Return is `return X` (or bare `return` if X == nil).
type Return struct {
	X Expr
}

func (*Return) rubyStmt() {}

func (s *Return) RubyString(ind int) string {
	if s.X == nil {
		return indent(ind) + "return"
	}
	return fmt.Sprintf("%sreturn %s", indent(ind), s.X.RubyExprString())
}

// IfStmt is `if Cond ... [elsif ...]* [else ...]? end`.
type IfStmt struct {
	Cond   Expr
	Then   []Stmt
	Elsifs []ElsifBranch
	Else   []Stmt
}

// ElsifBranch is one `elsif Cond ...` arm.
type ElsifBranch struct {
	Cond Expr
	Body []Stmt
}

func (*IfStmt) rubyStmt() {}

func (s *IfStmt) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sif %s\n", pad, s.Cond.RubyExprString())
	for _, st := range s.Then {
		sb.WriteString(st.RubyString(ind + 1))
		sb.WriteByte('\n')
	}
	for _, e := range s.Elsifs {
		fmt.Fprintf(&sb, "%selsif %s\n", pad, e.Cond.RubyExprString())
		for _, st := range e.Body {
			sb.WriteString(st.RubyString(ind + 1))
			sb.WriteByte('\n')
		}
	}
	if len(s.Else) > 0 {
		fmt.Fprintf(&sb, "%selse\n", pad)
		for _, st := range s.Else {
			sb.WriteString(st.RubyString(ind + 1))
			sb.WriteByte('\n')
		}
	}
	sb.WriteString(pad + "end")
	return sb.String()
}

// CaseInStmt is `case Scrutinee; in Pat then Body; end`. Used for sum matches.
type CaseInStmt struct {
	Scrutinee Expr
	Arms      []CaseInArm
	Else      []Stmt
}

// CaseInArm is one `in Pat then Body` arm.
type CaseInArm struct {
	Pattern string // e.g. "User(id:, name:)" - raw Ruby pattern text
	Body    []Stmt
}

func (*CaseInStmt) rubyStmt() {}

func (s *CaseInStmt) RubyString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%scase %s\n", pad, s.Scrutinee.RubyExprString())
	for _, a := range s.Arms {
		fmt.Fprintf(&sb, "%sin %s\n", pad, a.Pattern)
		for _, st := range a.Body {
			sb.WriteString(st.RubyString(ind + 1))
			sb.WriteByte('\n')
		}
	}
	if len(s.Else) > 0 {
		fmt.Fprintf(&sb, "%selse\n", pad)
		for _, st := range s.Else {
			sb.WriteString(st.RubyString(ind + 1))
			sb.WriteByte('\n')
		}
	}
	sb.WriteString(pad + "end")
	return sb.String()
}

// RawStmt is a verbatim statement string.
type RawStmt struct {
	Text string
}

func (*RawStmt) rubyStmt() {}

func (s *RawStmt) RubyString(ind int) string {
	pad := indent(ind)
	lines := strings.Split(strings.TrimRight(s.Text, "\n"), "\n")
	var out []string
	for _, l := range lines {
		if l == "" {
			out = append(out, "")
			continue
		}
		out = append(out, pad+l)
	}
	return strings.Join(out, "\n")
}

// ---- Expr interface ----

// Expr is a Ruby expression.
type Expr interface {
	rubyExpr()
	RubyExprString() string
}

// IntLit is an integer literal.
type IntLit struct {
	Value int64
}

func (*IntLit) rubyExpr() {}

func (e *IntLit) RubyExprString() string { return fmt.Sprintf("%d", e.Value) }

// FloatLit is a floating-point literal.
type FloatLit struct {
	Value float64
}

func (*FloatLit) rubyExpr() {}

func (e *FloatLit) RubyExprString() string {
	// Keep a trailing ".0" so Ruby sees the literal as a Float, never an
	// Integer (e.g. 5.0 must not render as "5"). strconv-style %g preserves
	// precision while staying compact, but doesn't always emit the decimal
	// point; we append ".0" when missing.
	s := fmt.Sprintf("%g", e.Value)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	return s
}

// StringLit is a Ruby string literal. The value is escaped for embedding in
// double-quoted Ruby source.
type StringLit struct {
	Value string
}

func (*StringLit) rubyExpr() {}

func (e *StringLit) RubyExprString() string {
	return strconvQuote(e.Value)
}

// BoolLit is `true` / `false`.
type BoolLit struct {
	Value bool
}

func (*BoolLit) rubyExpr() {}

func (e *BoolLit) RubyExprString() string {
	if e.Value {
		return "true"
	}
	return "false"
}

// NilLit is `nil`.
type NilLit struct{}

func (*NilLit) rubyExpr() {}

func (*NilLit) RubyExprString() string { return "nil" }

// Ident is a bare identifier reference (local variable, method name, constant).
type Ident struct {
	Name string
}

func (*Ident) rubyExpr() {}

func (e *Ident) RubyExprString() string { return e.Name }

// MethodCall is `Receiver.Method(Args...)`.
// If Receiver is nil, the receiver is omitted (implicit self).
// Method-with-block call sites are emitted via RawExpr (see lower.go).
type MethodCall struct {
	Receiver  Expr
	Method    string
	Args      []Expr
	UseParens bool // when false and Args==nil, omit parens (e.g. `puts "..."`)
}

func (*MethodCall) rubyExpr() {}

func (e *MethodCall) RubyExprString() string {
	var sb strings.Builder
	if e.Receiver != nil {
		sb.WriteString(e.Receiver.RubyExprString())
		sb.WriteByte('.')
	}
	sb.WriteString(e.Method)
	if len(e.Args) > 0 {
		if e.UseParens || e.Receiver != nil {
			sb.WriteByte('(')
		} else {
			sb.WriteByte(' ')
		}
		for i, a := range e.Args {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(a.RubyExprString())
		}
		if e.UseParens || e.Receiver != nil {
			sb.WriteByte(')')
		}
	} else if e.UseParens {
		sb.WriteString("()")
	}
	return sb.String()
}

// BinaryOp is `Lhs Op Rhs` (e.g. `a + b`, `x == y`).
type BinaryOp struct {
	Op  string
	Lhs Expr
	Rhs Expr
}

func (*BinaryOp) rubyExpr() {}

func (e *BinaryOp) RubyExprString() string {
	return fmt.Sprintf("(%s %s %s)", e.Lhs.RubyExprString(), e.Op, e.Rhs.RubyExprString())
}

// UnaryOp is `Op X` (e.g. `-x`, `!cond`).
type UnaryOp struct {
	Op string
	X  Expr
}

func (*UnaryOp) rubyExpr() {}

func (e *UnaryOp) RubyExprString() string {
	return fmt.Sprintf("(%s%s)", e.Op, e.X.RubyExprString())
}

// RawExpr is a verbatim expression string (escape hatch).
type RawExpr struct {
	Text string
}

func (*RawExpr) rubyExpr() {}

func (e *RawExpr) RubyExprString() string { return e.Text }

// strconvQuote returns a double-quoted Ruby string literal. Escapes \, ", \n,
// \r, \t, and non-printable ASCII via \xHH.
func strconvQuote(s string) string {
	var sb strings.Builder
	sb.WriteByte('"')
	for _, r := range s {
		switch r {
		case '\\':
			sb.WriteString(`\\`)
		case '"':
			sb.WriteString(`\"`)
		case '\n':
			sb.WriteString(`\n`)
		case '\r':
			sb.WriteString(`\r`)
		case '\t':
			sb.WriteString(`\t`)
		case '#':
			sb.WriteString(`\#`) // suppress interpolation
		default:
			if r < 0x20 {
				fmt.Fprintf(&sb, `\x%02X`, r)
			} else {
				sb.WriteRune(r)
			}
		}
	}
	sb.WriteByte('"')
	return sb.String()
}
