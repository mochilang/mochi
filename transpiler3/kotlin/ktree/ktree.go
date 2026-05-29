// Package ktree defines the Kotlin shadow AST used by the Mochi-to-Kotlin transpiler.
// Each node implements KtString(indent int) string for source generation.
package ktree

import (
	"fmt"
	"strings"
)

// indent returns a string of n*4 spaces.
func indent(n int) string {
	return strings.Repeat("    ", n)
}

// ---- Top-level ----

// SourceFile is the top-level unit; rendered as a complete .kt file.
type SourceFile struct {
	Name    string // filename base (no .kt extension)
	Package string // e.g. "mochi.user"
	Decls   []Decl
}

// KtSource returns the full Kotlin source text for this file.
func (sf *SourceFile) KtSource() string {
	var sb strings.Builder
	if sf.Package != "" {
		fmt.Fprintf(&sb, "package %s\n\n", sf.Package)
	}
	for _, d := range sf.Decls {
		sb.WriteString(d.KtString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

// ---- Decl interface ----

// Decl is a top-level declaration.
type Decl interface {
	ktDecl()
	KtString(indent int) string
}

// ---- FunDecl ----

// FunDecl is a Kotlin function declaration.
type FunDecl struct {
	Modifiers  []string // e.g. ["private"]
	Name       string
	Params     []FunParam
	ReturnType string // empty means Unit / no return annotation
	Body       []Stmt
}

// FunParam is one parameter of a FunDecl.
type FunParam struct {
	Name     string
	TypeName string
}

func (*FunDecl) ktDecl() {}

func (f *FunDecl) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range f.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("fun ")
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
		fmt.Fprintf(&sb, ": %s", f.ReturnType)
	}
	sb.WriteString(" {\n")
	for _, s := range f.Body {
		sb.WriteString(s.KtString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- DataClassDecl ----

// DataClassDecl is a Kotlin data class (for Mochi records).
type DataClassDecl struct {
	Name   string
	Fields []DataClassField
}

// DataClassField is one field of a DataClassDecl.
type DataClassField struct {
	Name     string
	TypeName string
}

func (*DataClassDecl) ktDecl() {}

func (d *DataClassDecl) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	fmt.Fprintf(&sb, "data class %s(", d.Name)
	for i, f := range d.Fields {
		if i > 0 {
			sb.WriteString(", ")
		}
		fmt.Fprintf(&sb, "val %s: %s", f.Name, f.TypeName)
	}
	sb.WriteString(")")
	return sb.String()
}

// ---- SealedClassDecl ----

// SealedClassDecl is a Kotlin sealed class (for Mochi sum types).
type SealedClassDecl struct {
	Name      string
	Subclasses []SealedSubclass
}

// SealedSubclass is one subclass of a SealedClassDecl.
type SealedSubclass struct {
	Name   string
	Fields []DataClassField // empty = object, non-empty = data class
}

func (*SealedClassDecl) ktDecl() {}

func (s *SealedClassDecl) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%ssealed class %s {\n", pad, s.Name)
	for _, sub := range s.Subclasses {
		if len(sub.Fields) == 0 {
			fmt.Fprintf(&sb, "%s    object %s : %s()\n", pad, sub.Name, s.Name)
		} else {
			parts := make([]string, len(sub.Fields))
			for i, f := range sub.Fields {
				parts[i] = fmt.Sprintf("val %s: %s", f.Name, f.TypeName)
			}
			fmt.Fprintf(&sb, "%s    data class %s(%s) : %s()\n", pad, sub.Name, strings.Join(parts, ", "), s.Name)
		}
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- RawKtDecl ----

// RawKtDecl is an escape hatch for raw Kotlin declarations.
type RawKtDecl struct {
	Code string
}

func (*RawKtDecl) ktDecl() {}

func (r *RawKtDecl) KtString(ind int) string {
	return indent(ind) + r.Code
}

// ---- Stmt interface ----

// Stmt is a Kotlin statement.
type Stmt interface {
	KtString(indent int) string
}

// ---- ExprStmt ----

// ExprStmt is an expression used as a statement.
type ExprStmt struct {
	X Expr
}

func (s *ExprStmt) KtString(ind int) string {
	return indent(ind) + s.X.KtExprString()
}

// ---- ReturnStmt ----

// ReturnStmt is a return statement.
type ReturnStmt struct {
	Value Expr // nil for bare return
}

func (s *ReturnStmt) KtString(ind int) string {
	if s.Value == nil {
		return indent(ind) + "return"
	}
	return indent(ind) + "return " + s.Value.KtExprString()
}

// ---- LocalVarStmt (val/var inside a function) ----

// LocalVarStmt is a val or var declaration inside a function body.
type LocalVarStmt struct {
	IsVar    bool
	Name     string
	TypeName string // empty = inferred
	Init     Expr
}

func (s *LocalVarStmt) KtString(ind int) string {
	pad := indent(ind)
	kw := "val"
	if s.IsVar {
		kw = "var"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%s%s %s", pad, kw, s.Name)
	if s.TypeName != "" {
		fmt.Fprintf(&sb, ": %s", s.TypeName)
	}
	if s.Init != nil {
		fmt.Fprintf(&sb, " = %s", s.Init.KtExprString())
	}
	return sb.String()
}

// ---- AssignStmt ----

// AssignStmt is an assignment: name = expr.
type AssignStmt struct {
	Target string
	Value  Expr
}

func (s *AssignStmt) KtString(ind int) string {
	return fmt.Sprintf("%s%s = %s", indent(ind), s.Target, s.Value.KtExprString())
}

// ---- IfStmt ----

// IfStmt is an if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt // nil means no else arm
}

func (s *IfStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sif (%s) {\n", pad, s.Cond.KtExprString())
	for _, st := range s.Then {
		sb.WriteString(st.KtString(ind + 1))
		sb.WriteString("\n")
	}
	if len(s.Else) > 0 {
		fmt.Fprintf(&sb, "%s} else {\n", pad)
		for _, st := range s.Else {
			sb.WriteString(st.KtString(ind + 1))
			sb.WriteString("\n")
		}
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- ForInStmt ----

// ForInStmt is a for-in loop.
type ForInStmt struct {
	Var        string
	Collection Expr
	Body       []Stmt
}

func (s *ForInStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sfor (%s in %s) {\n", pad, s.Var, s.Collection.KtExprString())
	for _, st := range s.Body {
		sb.WriteString(st.KtString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- ForRangeStmt ----

// ForRangeStmt is a for-in range loop: for (x in start until end) { ... }
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (s *ForRangeStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sfor (%s in %s until %s) {\n", pad, s.Var, s.Start.KtExprString(), s.End.KtExprString())
	for _, st := range s.Body {
		sb.WriteString(st.KtString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- WhileStmt ----

// WhileStmt is a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (s *WhileStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%swhile (%s) {\n", pad, s.Cond.KtExprString())
	for _, st := range s.Body {
		sb.WriteString(st.KtString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- BreakStmt ----

// BreakStmt is a break statement.
type BreakStmt struct{}

func (s *BreakStmt) KtString(ind int) string {
	return indent(ind) + "break"
}

// ---- ContinueStmt ----

// ContinueStmt is a continue statement.
type ContinueStmt struct{}

func (s *ContinueStmt) KtString(ind int) string {
	return indent(ind) + "continue"
}

// ---- WhenStmt ----

// WhenCase is one arm of a WhenStmt.
type WhenCase struct {
	// Pattern is the Kotlin when-entry condition string, or "else" for the default.
	Pattern string
	Body    []Stmt
	// ResultExpr, when non-nil, means this is a single-expression arm in expression form.
	ResultExpr Expr
}

// WhenStmt is a Kotlin when statement used for pattern matching.
type WhenStmt struct {
	Subject Expr
	Cases   []WhenCase
}

func (s *WhenStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%swhen (%s) {\n", pad, s.Subject.KtExprString())
	for _, c := range s.Cases {
		if c.Pattern == "else" {
			fmt.Fprintf(&sb, "%s    else -> {\n", pad)
		} else {
			fmt.Fprintf(&sb, "%s    is %s -> {\n", pad, c.Pattern)
		}
		for _, st := range c.Body {
			sb.WriteString(st.KtString(ind + 2))
			sb.WriteString("\n")
		}
		fmt.Fprintf(&sb, "%s    }\n", pad)
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- WhenAssignStmt ----

// WhenAssignStmt is `target = when(subject) { ... }` - assigns a when expression result.
type WhenAssignStmt struct {
	IsVar    bool   // true for var, false for val
	Target   string // variable name to assign to
	TypeName string // type annotation (if IsVar and no prior decl)
	Subject  Expr
	Cases    []WhenCase
}

func (s *WhenAssignStmt) KtString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	if s.TypeName != "" {
		kw := "val"
		if s.IsVar {
			kw = "var"
		}
		fmt.Fprintf(&sb, "%s%s %s: %s = when (%s) {\n", pad, kw, s.Target, s.TypeName, s.Subject.KtExprString())
	} else {
		fmt.Fprintf(&sb, "%s%s = when (%s) {\n", pad, s.Target, s.Subject.KtExprString())
	}
	for _, c := range s.Cases {
		if c.Pattern == "else" {
			sb.WriteString(pad + "    else -> ")
		} else {
			fmt.Fprintf(&sb, "%s    is %s -> ", pad, c.Pattern)
		}
		if c.ResultExpr != nil && len(c.Body) == 0 {
			// Simple single-expression arm.
			fmt.Fprintf(&sb, "%s\n", c.ResultExpr.KtExprString())
		} else if c.ResultExpr != nil && len(c.Body) > 0 {
			// Body stmts followed by result expression (yields value).
			sb.WriteString("{\n")
			for _, st := range c.Body {
				sb.WriteString(st.KtString(ind + 2))
				sb.WriteString("\n")
			}
			fmt.Fprintf(&sb, "%s        %s\n", pad, c.ResultExpr.KtExprString())
			fmt.Fprintf(&sb, "%s    }\n", pad)
		} else {
			sb.WriteString("{\n")
			for _, st := range c.Body {
				sb.WriteString(st.KtString(ind + 2))
				sb.WriteString("\n")
			}
			fmt.Fprintf(&sb, "%s    }\n", pad)
		}
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- IndexSetStmt ----

// IndexSetStmt is an indexed assignment: receiver[index] = value.
type IndexSetStmt struct {
	Receiver string
	Index    Expr
	Value    Expr
}

func (s *IndexSetStmt) KtString(ind int) string {
	return fmt.Sprintf("%s%s[%s] = %s", indent(ind), s.Receiver, s.Index.KtExprString(), s.Value.KtExprString())
}

// ---- RawKtStmt ----

// RawKtStmt is an escape hatch for raw Kotlin source statements.
type RawKtStmt struct {
	Code string
}

func (s *RawKtStmt) KtString(ind int) string {
	return indent(ind) + s.Code
}

// ---- Expr interface ----

// Expr is a Kotlin expression.
type Expr interface {
	KtExprString() string
}

// ---- CallExpr ----

// CallExpr is a function call: func(args...).
type CallExpr struct {
	Func string
	Args []Expr
}

func (e *CallExpr) KtExprString() string {
	args := make([]string, len(e.Args))
	for i, a := range e.Args {
		args[i] = a.KtExprString()
	}
	return fmt.Sprintf("%s(%s)", e.Func, strings.Join(args, ", "))
}

// ---- MethodCallExpr ----

// MethodCallExpr is a method call: receiver.method(args...).
type MethodCallExpr struct {
	Receiver Expr
	Method   string
	Args     []Expr
}

func (e *MethodCallExpr) KtExprString() string {
	args := make([]string, len(e.Args))
	for i, a := range e.Args {
		args[i] = a.KtExprString()
	}
	return fmt.Sprintf("%s.%s(%s)", e.Receiver.KtExprString(), e.Method, strings.Join(args, ", "))
}

// ---- BinaryExpr ----

// BinaryExpr is a binary expression: left op right.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (e *BinaryExpr) KtExprString() string {
	return fmt.Sprintf("(%s %s %s)", e.Left.KtExprString(), e.Op, e.Right.KtExprString())
}

// ---- UnaryExpr ----

// UnaryExpr is a unary expression: op expr.
type UnaryExpr struct {
	Op      string
	Operand Expr
}

func (e *UnaryExpr) KtExprString() string {
	return fmt.Sprintf("(%s%s)", e.Op, e.Operand.KtExprString())
}

// ---- IdentExpr ----

// IdentExpr is an identifier reference.
type IdentExpr struct {
	Name string
}

func (e *IdentExpr) KtExprString() string {
	return e.Name
}

// ---- LongLitExpr ----

// LongLitExpr is a Long literal.
type LongLitExpr struct {
	Value int64
}

func (e *LongLitExpr) KtExprString() string {
	return fmt.Sprintf("%dL", e.Value)
}

// ---- DoubleLitExpr ----

// DoubleLitExpr is a Double literal.
type DoubleLitExpr struct {
	Repr string // pre-formatted string (e.g. "3.14")
}

func (e *DoubleLitExpr) KtExprString() string {
	return e.Repr
}

// ---- StringLitExpr ----

// StringLitExpr is a string literal.
type StringLitExpr struct {
	Value string // raw unescaped Go string; KtExprString escapes it
}

func (e *StringLitExpr) KtExprString() string {
	return quoteKotlin(e.Value)
}

// quoteKotlin returns a Kotlin double-quoted string literal for s.
func quoteKotlin(s string) string {
	var sb strings.Builder
	sb.WriteByte('"')
	for _, r := range s {
		switch r {
		case '"':
			sb.WriteString(`\"`)
		case '\\':
			sb.WriteString(`\\`)
		case '\n':
			sb.WriteString(`\n`)
		case '\r':
			sb.WriteString(`\r`)
		case '\t':
			sb.WriteString(`\t`)
		default:
			sb.WriteRune(r)
		}
	}
	sb.WriteByte('"')
	return sb.String()
}

// ---- BoolLitExpr ----

// BoolLitExpr is a Boolean literal.
type BoolLitExpr struct {
	Value bool
}

func (e *BoolLitExpr) KtExprString() string {
	if e.Value {
		return "true"
	}
	return "false"
}

// ---- ListLitExpr ----

// ListLitExpr is a mutableListOf(...) expression.
type ListLitExpr struct {
	ElemType string // Kotlin type name, e.g. "Long"
	Elems    []Expr
}

func (e *ListLitExpr) KtExprString() string {
	parts := make([]string, len(e.Elems))
	for i, elem := range e.Elems {
		parts[i] = elem.KtExprString()
	}
	if len(e.Elems) == 0 {
		return fmt.Sprintf("mutableListOf<%s>()", e.ElemType)
	}
	return fmt.Sprintf("mutableListOf(%s)", strings.Join(parts, ", "))
}

// ---- MapLitExpr ----

// MapLitExpr is a linkedMapOf(...) expression.
type MapLitExpr struct {
	Keys   []Expr
	Values []Expr
}

func (e *MapLitExpr) KtExprString() string {
	if len(e.Keys) == 0 {
		return "linkedMapOf()"
	}
	parts := make([]string, len(e.Keys))
	for i := range e.Keys {
		parts[i] = e.Keys[i].KtExprString() + " to " + e.Values[i].KtExprString()
	}
	return "linkedMapOf(" + strings.Join(parts, ", ") + ")"
}

// ---- SetLitExpr ----

// SetLitExpr is a linkedSetOf(...) expression.
type SetLitExpr struct {
	ElemType string // Kotlin type name
	Elems    []Expr
}

func (e *SetLitExpr) KtExprString() string {
	if len(e.Elems) == 0 {
		return fmt.Sprintf("linkedSetOf<%s>()", e.ElemType)
	}
	parts := make([]string, len(e.Elems))
	for i, elem := range e.Elems {
		parts[i] = elem.KtExprString()
	}
	return "linkedSetOf(" + strings.Join(parts, ", ") + ")"
}

// ---- SubscriptExpr ----

// SubscriptExpr is a subscript expression: expr[index].
type SubscriptExpr struct {
	Receiver Expr
	Index    Expr
}

func (e *SubscriptExpr) KtExprString() string {
	return fmt.Sprintf("%s[%s]", e.Receiver.KtExprString(), e.Index.KtExprString())
}

// ---- MemberExpr ----

// MemberExpr is a member access expression: expr.member.
type MemberExpr struct {
	Receiver Expr
	Member   string
}

func (e *MemberExpr) KtExprString() string {
	return fmt.Sprintf("%s.%s", e.Receiver.KtExprString(), e.Member)
}

// ---- ClosureLitExpr ----

// ClosureLitExpr is a Kotlin lambda expression: { params -> body }.
type ClosureLitExpr struct {
	Params []string
	Body   []Stmt
}

func (e *ClosureLitExpr) KtExprString() string {
	var sb strings.Builder
	sb.WriteString("{ ")
	if len(e.Params) > 0 {
		sb.WriteString(strings.Join(e.Params, ", "))
		sb.WriteString(" ->")
	}
	sb.WriteString("\n")
	for _, s := range e.Body {
		sb.WriteString(s.KtString(1))
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// ---- RawKtExpr ----

// RawKtExpr is an escape hatch for raw Kotlin expressions.
type RawKtExpr struct {
	Code string
}

func (e *RawKtExpr) KtExprString() string {
	return e.Code
}
