package sxtree

import (
	"fmt"
	"strings"
)

// indent returns a string of n*4 spaces.
func indent(n int) string {
	return strings.Repeat("    ", n)
}

// ---- Top-level ----

// SourceFile is the top-level unit; rendered as a complete .swift file.
type SourceFile struct {
	Name    string // filename base (no .swift extension)
	Imports []string
	Decls   []Decl
}

// SwiftSource returns the full Swift source text for this file.
func (sf *SourceFile) SwiftSource() string {
	var sb strings.Builder
	for _, imp := range sf.Imports {
		fmt.Fprintf(&sb, "import %s\n", imp)
	}
	if len(sf.Imports) > 0 {
		sb.WriteString("\n")
	}
	for _, d := range sf.Decls {
		sb.WriteString(d.SwiftString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

// ---- Decl interface ----

// Decl is a top-level declaration.
type Decl interface {
	swiftDecl()
	SwiftString(indent int) string
}

// ---- StructDecl ----

// StructDecl is a Swift struct declaration, e.g. @main struct HelloMochi { ... }.
type StructDecl struct {
	Attributes []string // e.g. ["@main"]
	Modifiers  []string // e.g. ["public"]
	Name       string
	Protocols  []string // e.g. ["Equatable"]
	Members    []Decl
}

func (*StructDecl) swiftDecl() {}

func (s *StructDecl) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	for _, attr := range s.Attributes {
		fmt.Fprintf(&sb, "%s%s\n", pad, attr)
	}
	sb.WriteString(pad)
	for _, m := range s.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	if len(s.Protocols) > 0 {
		fmt.Fprintf(&sb, "struct %s: %s {\n", s.Name, strings.Join(s.Protocols, ", "))
	} else {
		fmt.Fprintf(&sb, "struct %s {\n", s.Name)
	}
	for _, mem := range s.Members {
		sb.WriteString(mem.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- FuncDecl ----

// FuncDecl is a function declaration.
type FuncDecl struct {
	Attributes []string // e.g. ["@discardableResult"]
	Modifiers  []string // e.g. ["public", "static"]
	Name       string
	Params     []FuncParam
	ReturnType string // empty means Void / no return annotation
	IsAsync    bool
	Body       []Stmt
}

// FuncParam is one parameter of a FuncDecl.
type FuncParam struct {
	Label    string // external label (use "_" for no label)
	Name     string // internal name
	TypeName string
}

func (*FuncDecl) swiftDecl() {}

func (f *FuncDecl) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	for _, attr := range f.Attributes {
		fmt.Fprintf(&sb, "%s%s\n", pad, attr)
	}
	sb.WriteString(pad)
	for _, m := range f.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("func ")
	sb.WriteString(f.Name)
	sb.WriteByte('(')
	for i, p := range f.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		if p.Label != "" && p.Label != p.Name {
			fmt.Fprintf(&sb, "%s %s: %s", p.Label, p.Name, p.TypeName)
		} else if p.Label == "_" {
			fmt.Fprintf(&sb, "_ %s: %s", p.Name, p.TypeName)
		} else {
			fmt.Fprintf(&sb, "%s: %s", p.Name, p.TypeName)
		}
	}
	sb.WriteByte(')')
	if f.IsAsync {
		sb.WriteString(" async")
	}
	if f.ReturnType != "" {
		fmt.Fprintf(&sb, " -> %s", f.ReturnType)
	}
	sb.WriteString(" {\n")
	for _, s := range f.Body {
		sb.WriteString(s.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- ExtensionDecl ----

// ExtensionDecl is a Swift extension declaration.
type ExtensionDecl struct {
	TypeName string
	Members  []Decl
}

func (*ExtensionDecl) swiftDecl() {}

func (e *ExtensionDecl) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sextension %s {\n", pad, e.TypeName)
	for _, m := range e.Members {
		sb.WriteString(m.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- VarDecl (top-level) ----

// VarDecl is a let/var declaration at struct/global scope.
type VarDecl struct {
	IsVar    bool   // true = var, false = let
	Name     string
	TypeName string // empty = inferred
	Init     Expr   // nil = no initializer
}

func (*VarDecl) swiftDecl() {}

func (v *VarDecl) SwiftString(ind int) string {
	pad := indent(ind)
	kw := "let"
	if v.IsVar {
		kw = "var"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%s%s %s", pad, kw, v.Name)
	if v.TypeName != "" {
		fmt.Fprintf(&sb, ": %s", v.TypeName)
	}
	if v.Init != nil {
		fmt.Fprintf(&sb, " = %s", v.Init.SwiftExprString())
	}
	return sb.String()
}

// ---- Stmt interface ----

// Stmt is a Swift statement.
type Stmt interface {
	SwiftString(indent int) string
}

// ---- ExprStmt ----

// ExprStmt is an expression used as a statement.
type ExprStmt struct {
	X Expr
}

func (s *ExprStmt) SwiftString(ind int) string {
	return indent(ind) + s.X.SwiftExprString()
}

// ---- ReturnStmt ----

// ReturnStmt is a return statement.
type ReturnStmt struct {
	Value Expr // nil for bare return
}

func (s *ReturnStmt) SwiftString(ind int) string {
	if s.Value == nil {
		return indent(ind) + "return"
	}
	return indent(ind) + "return " + s.Value.SwiftExprString()
}

// ---- LocalVarStmt (let/var inside a function) ----

// LocalVarStmt is a let or var declaration inside a function body.
type LocalVarStmt struct {
	IsVar    bool
	Name     string
	TypeName string // empty = inferred
	Init     Expr
}

func (s *LocalVarStmt) SwiftString(ind int) string {
	pad := indent(ind)
	kw := "let"
	if s.IsVar {
		kw = "var"
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%s%s %s", pad, kw, s.Name)
	if s.TypeName != "" {
		fmt.Fprintf(&sb, ": %s", s.TypeName)
	}
	if s.Init != nil {
		fmt.Fprintf(&sb, " = %s", s.Init.SwiftExprString())
	}
	return sb.String()
}

// ---- AssignStmt ----

// AssignStmt is an assignment: name = expr.
type AssignStmt struct {
	Target string
	Value  Expr
}

func (s *AssignStmt) SwiftString(ind int) string {
	return fmt.Sprintf("%s%s = %s", indent(ind), s.Target, s.Value.SwiftExprString())
}

// ---- IfStmt ----

// IfStmt is an if/else statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt // nil means no else arm
}

func (s *IfStmt) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sif %s {\n", pad, s.Cond.SwiftExprString())
	for _, st := range s.Then {
		sb.WriteString(st.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	if len(s.Else) > 0 {
		fmt.Fprintf(&sb, "%s} else {\n", pad)
		for _, st := range s.Else {
			sb.WriteString(st.SwiftString(ind + 1))
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

func (s *ForInStmt) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sfor %s in %s {\n", pad, s.Var, s.Collection.SwiftExprString())
	for _, st := range s.Body {
		sb.WriteString(st.SwiftString(ind + 1))
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

func (s *WhileStmt) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%swhile %s {\n", pad, s.Cond.SwiftExprString())
	for _, st := range s.Body {
		sb.WriteString(st.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- BreakStmt ----

// BreakStmt is a break statement.
type BreakStmt struct{}

func (s *BreakStmt) SwiftString(ind int) string {
	return indent(ind) + "break"
}

// ---- ContinueStmt ----

// ContinueStmt is a continue statement.
type ContinueStmt struct{}

func (s *ContinueStmt) SwiftString(ind int) string {
	return indent(ind) + "continue"
}

// ---- ForRangeStmt ----

// ForRangeStmt is a for-in range loop: for x in start..<end { ... }
type ForRangeStmt struct {
	Var   string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (s *ForRangeStmt) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sfor %s in %s..<%s {\n", pad, s.Var, s.Start.SwiftExprString(), s.End.SwiftExprString())
	for _, st := range s.Body {
		sb.WriteString(st.SwiftString(ind + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- RawSwiftStmt ----

// RawSwiftStmt is an escape hatch for raw Swift source statements.
type RawSwiftStmt struct {
	Code string
}

func (s *RawSwiftStmt) SwiftString(ind int) string {
	return indent(ind) + s.Code
}

// ---- Expr interface ----

// Expr is a Swift expression.
type Expr interface {
	SwiftExprString() string
}

// ---- CallExpr ----

// CallExpr is a function call: func(args...).
type CallExpr struct {
	Func string
	Args []Expr
}

func (e *CallExpr) SwiftExprString() string {
	args := make([]string, len(e.Args))
	for i, a := range e.Args {
		args[i] = a.SwiftExprString()
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

func (e *MethodCallExpr) SwiftExprString() string {
	args := make([]string, len(e.Args))
	for i, a := range e.Args {
		args[i] = a.SwiftExprString()
	}
	return fmt.Sprintf("%s.%s(%s)", e.Receiver.SwiftExprString(), e.Method, strings.Join(args, ", "))
}

// ---- BinaryExpr ----

// BinaryExpr is a binary expression: left op right.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (e *BinaryExpr) SwiftExprString() string {
	return fmt.Sprintf("(%s %s %s)", e.Left.SwiftExprString(), e.Op, e.Right.SwiftExprString())
}

// ---- UnaryExpr ----

// UnaryExpr is a unary expression: op expr.
type UnaryExpr struct {
	Op      string
	Operand Expr
}

func (e *UnaryExpr) SwiftExprString() string {
	return fmt.Sprintf("(%s%s)", e.Op, e.Operand.SwiftExprString())
}

// ---- IdentExpr ----

// IdentExpr is an identifier reference.
type IdentExpr struct {
	Name string
}

func (e *IdentExpr) SwiftExprString() string {
	return e.Name
}

// ---- IntLitExpr ----

// IntLitExpr is an Int64 literal.
type IntLitExpr struct {
	Value int64
}

func (e *IntLitExpr) SwiftExprString() string {
	return fmt.Sprintf("Int64(%d)", e.Value)
}

// ---- FloatLitExpr ----

// FloatLitExpr is a Double literal.
type FloatLitExpr struct {
	Repr string // pre-formatted string (e.g. "3.14")
}

func (e *FloatLitExpr) SwiftExprString() string {
	return e.Repr
}

// ---- StringLitExpr ----

// StringLitExpr is a string literal.
type StringLitExpr struct {
	Value string // raw unescaped Go string; SwiftExprString escapes it
}

func (e *StringLitExpr) SwiftExprString() string {
	return quoteSwift(e.Value)
}

// quoteSwift returns a Swift double-quoted string literal for s.
func quoteSwift(s string) string {
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

// BoolLitExpr is a Bool literal.
type BoolLitExpr struct {
	Value bool
}

func (e *BoolLitExpr) SwiftExprString() string {
	if e.Value {
		return "true"
	}
	return "false"
}

// ---- ArrayLitExpr ----

// ArrayLitExpr is an array literal: [e1, e2, ...].
type ArrayLitExpr struct {
	Elems []Expr
}

func (e *ArrayLitExpr) SwiftExprString() string {
	parts := make([]string, len(e.Elems))
	for i, elem := range e.Elems {
		parts[i] = elem.SwiftExprString()
	}
	return "[" + strings.Join(parts, ", ") + "]"
}

// ---- NilLitExpr ----

// NilLitExpr is the nil literal.
type NilLitExpr struct{}

func (e *NilLitExpr) SwiftExprString() string {
	return "nil"
}

// ---- ClosureExpr ----

// ClosureExpr is a closure literal: { params in body }.
type ClosureExpr struct {
	Params []string
	Body   []Stmt
}

func (e *ClosureExpr) SwiftExprString() string {
	var sb strings.Builder
	sb.WriteString("{ ")
	if len(e.Params) > 0 {
		sb.WriteString(strings.Join(e.Params, ", "))
		sb.WriteString(" in")
	}
	sb.WriteString("\n")
	for _, s := range e.Body {
		sb.WriteString(s.SwiftString(1))
		sb.WriteString("\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// ---- TernaryExpr ----

// TernaryExpr is a ternary expression: cond ? then : else.
type TernaryExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (e *TernaryExpr) SwiftExprString() string {
	return fmt.Sprintf("(%s ? %s : %s)", e.Cond.SwiftExprString(), e.Then.SwiftExprString(), e.Else.SwiftExprString())
}

// ---- TypeCastExpr ----

// TypeCastExpr is a safe type cast: expr as Type.
type TypeCastExpr struct {
	X        Expr
	TypeName string
}

func (e *TypeCastExpr) SwiftExprString() string {
	return fmt.Sprintf("(%s as %s)", e.X.SwiftExprString(), e.TypeName)
}

// ---- ForceCastExpr ----

// ForceCastExpr is a forced type cast: expr as! Type.
type ForceCastExpr struct {
	X        Expr
	TypeName string
}

func (e *ForceCastExpr) SwiftExprString() string {
	return fmt.Sprintf("(%s as! %s)", e.X.SwiftExprString(), e.TypeName)
}

// ---- SubscriptExpr ----

// SubscriptExpr is a subscript expression: expr[index].
type SubscriptExpr struct {
	Receiver Expr
	Index    Expr
}

func (e *SubscriptExpr) SwiftExprString() string {
	return fmt.Sprintf("%s[%s]", e.Receiver.SwiftExprString(), e.Index.SwiftExprString())
}

// ---- MemberExpr ----

// MemberExpr is a member access expression: expr.member.
type MemberExpr struct {
	Receiver Expr
	Member   string
}

func (e *MemberExpr) SwiftExprString() string {
	return fmt.Sprintf("%s.%s", e.Receiver.SwiftExprString(), e.Member)
}

// ---- TupleExpr ----

// TupleExpr is a tuple expression: (e1, e2).
type TupleExpr struct {
	Elems []Expr
}

func (e *TupleExpr) SwiftExprString() string {
	parts := make([]string, len(e.Elems))
	for i, elem := range e.Elems {
		parts[i] = elem.SwiftExprString()
	}
	return "(" + strings.Join(parts, ", ") + ")"
}

// ---- OptionalExpr ----

// OptionalExpr is an optional chain expression: expr?.
type OptionalExpr struct {
	X Expr
}

func (e *OptionalExpr) SwiftExprString() string {
	return e.X.SwiftExprString() + "?"
}

// ---- RawSwiftExpr ----

// RawSwiftExpr is an escape hatch for raw Swift expressions.
type RawSwiftExpr struct {
	Code string
}

func (e *RawSwiftExpr) SwiftExprString() string {
	return e.Code
}

// ---- EnumDecl ----

// EnumCaseDecl is one case inside an EnumDecl.
type EnumCaseDecl struct {
	Name   string
	Fields []EnumCaseField // empty for nullary cases
}

// EnumCaseField is one associated-value field of an enum case.
type EnumCaseField struct {
	Label string // external label (empty = use positional)
	Type  string
}

// EnumDecl is a Swift enum declaration with optional associated values.
type EnumDecl struct {
	Modifiers []string
	Name      string
	Cases     []EnumCaseDecl
}

func (*EnumDecl) swiftDecl() {}

func (e *EnumDecl) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range e.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	fmt.Fprintf(&sb, "enum %s {\n", e.Name)
	for _, c := range e.Cases {
		if len(c.Fields) == 0 {
			fmt.Fprintf(&sb, "%s    case %s\n", pad, c.Name)
		} else {
			parts := make([]string, len(c.Fields))
			for i, f := range c.Fields {
				if f.Label != "" {
					parts[i] = f.Label + ": " + f.Type
				} else {
					parts[i] = f.Type
				}
			}
			fmt.Fprintf(&sb, "%s    case %s(%s)\n", pad, c.Name, strings.Join(parts, ", "))
		}
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- SwitchStmt ----

// SwitchCase is one case arm inside a SwitchStmt.
type SwitchCase struct {
	// Pattern is the Swift case pattern string, e.g. ".Circle(let r)" or "default".
	Pattern string
	Body    []Stmt
}

// SwitchStmt is a Swift switch statement.
type SwitchStmt struct {
	Subject Expr
	Cases   []SwitchCase
}

func (s *SwitchStmt) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sswitch %s {\n", pad, s.Subject.SwiftExprString())
	for _, c := range s.Cases {
		if c.Pattern == "default" {
			fmt.Fprintf(&sb, "%sdefault:\n", pad)
		} else {
			fmt.Fprintf(&sb, "%scase %s:\n", pad, c.Pattern)
		}
		for _, st := range c.Body {
			sb.WriteString(st.SwiftString(ind + 1))
			sb.WriteString("\n")
		}
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ---- ClassDecl ----

// ClassProp is one stored property of a ClassDecl.
type ClassProp struct {
	Name     string
	TypeName string
}

// ClassDecl is a Swift final class declaration for Mochi agents.
// It is rendered as `public final class Name { ... }`.
type ClassDecl struct {
	Name    string
	Props   []ClassProp  // public var properties
	Inits   []FuncParam  // init parameters (name + type + default via Init field)
	Inits2  []string     // default values as Swift expressions, parallel to Inits
	Methods []FuncDecl   // public func methods
}

func (*ClassDecl) swiftDecl() {}

func (c *ClassDecl) SwiftString(ind int) string {
	pad := indent(ind)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%spublic final class %s {\n", pad, c.Name)

	// Properties.
	for _, p := range c.Props {
		fmt.Fprintf(&sb, "%s    public var %s: %s\n", pad, p.Name, p.TypeName)
	}

	// Initialiser.
	sb.WriteString("\n")
	sb.WriteString(pad + "    public init(")
	for i, p := range c.Inits {
		if i > 0 {
			sb.WriteString(", ")
		}
		if c.Inits2 != nil && i < len(c.Inits2) && c.Inits2[i] != "" {
			fmt.Fprintf(&sb, "%s: %s = %s", p.Name, p.TypeName, c.Inits2[i])
		} else {
			fmt.Fprintf(&sb, "%s: %s", p.Name, p.TypeName)
		}
	}
	sb.WriteString(") {\n")
	for _, p := range c.Inits {
		fmt.Fprintf(&sb, "%s        self.%s = %s\n", pad, p.Name, p.Name)
	}
	sb.WriteString(pad + "    }\n")

	// Methods.
	for _, m := range c.Methods {
		sb.WriteString("\n")
		sb.WriteString(m.SwiftString(ind + 1))
		sb.WriteString("\n")
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

func (s *IndexSetStmt) SwiftString(ind int) string {
	return fmt.Sprintf("%s%s[%s] = %s", indent(ind), s.Receiver, s.Index.SwiftExprString(), s.Value.SwiftExprString())
}
