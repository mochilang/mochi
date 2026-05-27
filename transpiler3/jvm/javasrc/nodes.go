package javasrc

import (
	"fmt"
	"strings"
)

// javaString() implementations produce Java source fragments.
// Callers are responsible for indentation; nodes always start at column 0.

// --- Declaration nodes ---

// CompilationUnit is the top-level Java source file.
type CompilationUnit struct {
	Package string     // e.g. "dev.mochi.user"
	Imports []string   // e.g. ["java.util.ArrayList", "java.util.List"]
	Types   []TypeDecl // class/record/interface declarations
}

// TypeDecl is the common interface for all type declarations.
type TypeDecl interface {
	isTypeDecl()
	javaString(indent int) string
}

func (cu *CompilationUnit) JavaString() string {
	var sb strings.Builder
	if cu.Package != "" {
		fmt.Fprintf(&sb, "package %s;\n\n", cu.Package)
	}
	for _, imp := range cu.Imports {
		fmt.Fprintf(&sb, "import %s;\n", imp)
	}
	if len(cu.Imports) > 0 {
		sb.WriteString("\n")
	}
	for _, td := range cu.Types {
		sb.WriteString(td.javaString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

// ClassDecl is a class declaration.
type ClassDecl struct {
	Modifiers  []string   // e.g. ["public", "final"]
	Name       string
	TypeParams []TypeParam
	Supertype  *TypeRef  // nil if no extends
	Interfaces []TypeRef // implements ...
	Members    []Member
}

func (*ClassDecl) isTypeDecl() {}

func (c *ClassDecl) javaString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range c.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("class ")
	sb.WriteString(c.Name)
	if len(c.TypeParams) > 0 {
		sb.WriteByte('<')
		for i, tp := range c.TypeParams {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(tp.javaString())
		}
		sb.WriteByte('>')
	}
	if c.Supertype != nil {
		sb.WriteString(" extends ")
		sb.WriteString(c.Supertype.JavaString())
	}
	if len(c.Interfaces) > 0 {
		sb.WriteString(" implements ")
		for i, iface := range c.Interfaces {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(iface.JavaString())
		}
	}
	sb.WriteString(" {\n")
	for _, mem := range c.Members {
		sb.WriteString(mem.memberString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// RecordDecl is a Java record declaration (JEP 395).
type RecordDecl struct {
	Modifiers  []string
	Name       string
	Components []RecordComponent
	Implements []TypeRef
	Members    []Member // additional methods
}

func (*RecordDecl) isTypeDecl() {}

// RecordComponent is a single record component (type + name).
type RecordComponent struct {
	Type TypeRef
	Name string
}

func (r *RecordDecl) javaString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range r.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("record ")
	sb.WriteString(r.Name)
	sb.WriteByte('(')
	for i, c := range r.Components {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(c.Type.JavaString())
		sb.WriteByte(' ')
		sb.WriteString(c.Name)
	}
	sb.WriteByte(')')
	if len(r.Implements) > 0 {
		sb.WriteString(" implements ")
		for i, iface := range r.Implements {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(iface.JavaString())
		}
	}
	if len(r.Members) == 0 {
		sb.WriteString(" {}")
	} else {
		sb.WriteString(" {\n")
		for _, mem := range r.Members {
			sb.WriteString(mem.memberString(indent + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad)
		sb.WriteString("}")
	}
	return sb.String()
}

// SealedInterfaceDecl is a sealed interface with a permits clause.
type SealedInterfaceDecl struct {
	Modifiers  []string
	Name       string
	TypeParams []TypeParam
	Permits    []string // permitted subtype names
	Members    []Member
}

func (*SealedInterfaceDecl) isTypeDecl() {}

func (s *SealedInterfaceDecl) javaString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range s.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("sealed interface ")
	sb.WriteString(s.Name)
	if len(s.TypeParams) > 0 {
		sb.WriteByte('<')
		for i, tp := range s.TypeParams {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(tp.javaString())
		}
		sb.WriteByte('>')
	}
	if len(s.Permits) > 0 {
		sb.WriteString(" permits ")
		sb.WriteString(strings.Join(s.Permits, ", "))
	}
	sb.WriteString(" {\n")
	for _, mem := range s.Members {
		sb.WriteString(mem.memberString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// Member is the common interface for class/record/interface members.
type Member interface {
	memberString(indent int) string
}

// MethodDecl is an instance or static method declaration.
type MethodDecl struct {
	Modifiers  []string
	TypeParams []TypeParam
	ReturnType TypeRef
	Name       string
	Params     []Param
	Body       *Block // nil for abstract/interface methods
}

func (m *MethodDecl) memberString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, mod := range m.Modifiers {
		sb.WriteString(mod)
		sb.WriteByte(' ')
	}
	if len(m.TypeParams) > 0 {
		sb.WriteByte('<')
		for i, tp := range m.TypeParams {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(tp.javaString())
		}
		sb.WriteString("> ")
	}
	sb.WriteString(m.ReturnType.JavaString())
	sb.WriteByte(' ')
	sb.WriteString(m.Name)
	sb.WriteByte('(')
	for i, p := range m.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Type.JavaString())
		sb.WriteByte(' ')
		sb.WriteString(p.Name)
	}
	sb.WriteByte(')')
	if m.Body == nil {
		sb.WriteByte(';')
	} else {
		sb.WriteByte(' ')
		sb.WriteString(m.Body.blockString(indent))
	}
	return sb.String()
}

// ConstructorDecl is a constructor.
type ConstructorDecl struct {
	Modifiers []string
	Name      string
	Params    []Param
	Body      Block
}

func (c *ConstructorDecl) memberString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, mod := range c.Modifiers {
		sb.WriteString(mod)
		sb.WriteByte(' ')
	}
	sb.WriteString(c.Name)
	sb.WriteByte('(')
	for i, p := range c.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Type.JavaString())
		sb.WriteByte(' ')
		sb.WriteString(p.Name)
	}
	sb.WriteString(") ")
	sb.WriteString(c.Body.blockString(indent))
	return sb.String()
}

// FieldDecl is a field declaration.
type FieldDecl struct {
	Modifiers []string
	Type      TypeRef
	Name      string
	Init      Expr // nil if no initialiser
}

func (f *FieldDecl) memberString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, mod := range f.Modifiers {
		sb.WriteString(mod)
		sb.WriteByte(' ')
	}
	sb.WriteString(f.Type.JavaString())
	sb.WriteByte(' ')
	sb.WriteString(f.Name)
	if f.Init != nil {
		sb.WriteString(" = ")
		sb.WriteString(f.Init.ExprString())
	}
	sb.WriteByte(';')
	return sb.String()
}

// EnumDecl is an enum (used for singleton sum-type variants).
type EnumDecl struct {
	Modifiers  []string
	Name       string
	Implements []TypeRef
	Constants  []string
	Members    []Member
}

func (*EnumDecl) isTypeDecl() {}

func (e *EnumDecl) javaString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range e.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("enum ")
	sb.WriteString(e.Name)
	if len(e.Implements) > 0 {
		sb.WriteString(" implements ")
		for i, iface := range e.Implements {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(iface.JavaString())
		}
	}
	sb.WriteString(" {\n")
	sb.WriteString(pad + "    ")
	sb.WriteString(strings.Join(e.Constants, ", "))
	sb.WriteString(";\n")
	for _, mem := range e.Members {
		sb.WriteString(mem.memberString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// --- Statement nodes ---

// Stmt is the common interface for all statements.
type Stmt interface {
	stmtString(indent int) string
}

// Block is a sequence of statements enclosed in braces.
type Block struct {
	Stmts []Stmt
}

func (b *Block) stmtString(indent int) string {
	return b.blockString(indent)
}

func (b *Block) blockString(indent int) string {
	if len(b.Stmts) == 0 {
		return "{}"
	}
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString("{\n")
	for _, s := range b.Stmts {
		sb.WriteString(s.stmtString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// ReturnStmt is a return statement.
type ReturnStmt struct {
	Value Expr // nil for void return
}

func (r *ReturnStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	if r.Value == nil {
		return pad + "return;"
	}
	return pad + "return " + r.Value.ExprString() + ";"
}

// IfStmt is an if/else statement.
type IfStmt struct {
	Cond Expr
	Then Block
	Else *Block // nil if no else
}

func (s *IfStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("if (")
	sb.WriteString(s.Cond.ExprString())
	sb.WriteString(") ")
	sb.WriteString(s.Then.blockString(indent))
	if s.Else != nil {
		sb.WriteString(" else ")
		sb.WriteString(s.Else.blockString(indent))
	}
	return sb.String()
}

// ForStmt is a classic for loop.
type ForStmt struct {
	Init   Stmt // VarDeclStmt or ExprStmt; nil for empty init
	Cond   Expr // nil for infinite loop
	Update Stmt // ExprStmt; nil for empty update
	Body   Block
}

func (s *ForStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	initStr := ""
	if s.Init != nil {
		raw := s.Init.stmtString(0)
		initStr = strings.TrimSuffix(strings.TrimSpace(raw), ";")
	}
	condStr := ""
	if s.Cond != nil {
		condStr = s.Cond.ExprString()
	}
	updateStr := ""
	if s.Update != nil {
		raw := s.Update.stmtString(0)
		updateStr = strings.TrimSuffix(strings.TrimSpace(raw), ";")
	}
	return pad + "for (" + initStr + "; " + condStr + "; " + updateStr + ") " + s.Body.blockString(indent)
}

// ForEachStmt is an enhanced for loop.
type ForEachStmt struct {
	ElemType TypeRef
	ElemName string
	Iter     Expr
	Body     Block
}

func (s *ForEachStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + "for (" + s.ElemType.JavaString() + " " + s.ElemName + " : " + s.Iter.ExprString() + ") " + s.Body.blockString(indent)
}

// WhileStmt is a while loop.
type WhileStmt struct {
	Cond Expr
	Body Block
}

func (s *WhileStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + "while (" + s.Cond.ExprString() + ") " + s.Body.blockString(indent)
}

// BreakStmt is a break statement.
type BreakStmt struct{}

func (*BreakStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + "break;"
}

// ContinueStmt is a continue statement.
type ContinueStmt struct{}

func (*ContinueStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + "continue;"
}

// ExprStmt wraps an expression as a statement.
type ExprStmt struct {
	X Expr
}

func (s *ExprStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + s.X.ExprString() + ";"
}

// VarDeclStmt is a local variable declaration.
type VarDeclStmt struct {
	Type *TypeRef // nil means use var (Java 10+)
	Name string
	Init Expr // nil if no initialiser
}

func (s *VarDeclStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	typeName := "var"
	if s.Type != nil {
		typeName = s.Type.JavaString()
	}
	if s.Init == nil {
		return pad + typeName + " " + s.Name + ";"
	}
	return pad + typeName + " " + s.Name + " = " + s.Init.ExprString() + ";"
}

// TryCatchStmt is a try/catch statement.
type TryCatchStmt struct {
	Body      Block
	CatchVar  string
	CatchTyp  TypeRef
	CatchBody Block
}

func (s *TryCatchStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("try ")
	sb.WriteString(s.Body.blockString(indent))
	sb.WriteString(" catch (")
	sb.WriteString(s.CatchTyp.JavaString())
	sb.WriteByte(' ')
	sb.WriteString(s.CatchVar)
	sb.WriteString(") ")
	sb.WriteString(s.CatchBody.blockString(indent))
	return sb.String()
}

// ThrowStmt is a throw statement.
type ThrowStmt struct {
	Value Expr
}

func (s *ThrowStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + "throw " + s.Value.ExprString() + ";"
}

// SwitchStmt is a switch statement (not expression).
type SwitchStmt struct {
	Tag   Expr
	Cases []SwitchCase
}

// SwitchCase is a single case arm in a switch statement.
type SwitchCase struct {
	Pattern string // Java pattern or value, e.g. "Integer i" or "42"
	Body    []Stmt
	Default bool
}

func (s *SwitchStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("switch (")
	sb.WriteString(s.Tag.ExprString())
	sb.WriteString(") {\n")
	for _, c := range s.Cases {
		if c.Default {
			sb.WriteString(pad + "    default -> ")
		} else {
			sb.WriteString(pad + "    case " + c.Pattern + " -> ")
		}
		if len(c.Body) == 1 {
			sb.WriteString(c.Body[0].stmtString(0))
			sb.WriteString("\n")
		} else {
			sb.WriteString("{\n")
			for _, stmt := range c.Body {
				sb.WriteString(stmt.stmtString(indent + 2))
				sb.WriteString("\n")
			}
			sb.WriteString(pad + "    }\n")
		}
	}
	sb.WriteString(pad)
	sb.WriteString("}")
	return sb.String()
}

// AssignStmt is an assignment statement (not declaration).
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + s.Target.ExprString() + " = " + s.Value.ExprString() + ";"
}

// --- Expression nodes ---

// Expr is the common interface for all expressions.
type Expr interface {
	ExprString() string
}

// SwitchExpr is a switch expression (JEP 361+).
type SwitchExpr struct {
	Tag   Expr
	Cases []SwitchExprCase
}

// SwitchExprCase is a single case arm in a switch expression.
type SwitchExprCase struct {
	Pattern string // e.g. "MyType.Variant(long x, long y)"
	Guard   string // optional when-guard, e.g. "x > 0"
	Body    Expr
	Default bool
}

func (e *SwitchExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("switch (")
	sb.WriteString(e.Tag.ExprString())
	sb.WriteString(") {\n")
	for _, c := range e.Cases {
		if c.Default {
			sb.WriteString("    default -> ")
		} else {
			sb.WriteString("    case ")
			sb.WriteString(c.Pattern)
			if c.Guard != "" {
				sb.WriteString(" when ")
				sb.WriteString(c.Guard)
			}
			sb.WriteString(" -> ")
		}
		sb.WriteString(c.Body.ExprString())
		sb.WriteString(";\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// CallExpr is an instance method call.
type CallExpr struct {
	Receiver Expr
	Method   string
	TypeArgs []TypeRef
	Args     []Expr
}

func (e *CallExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Receiver.ExprString())
	sb.WriteByte('.')
	sb.WriteString(e.Method)
	if len(e.TypeArgs) > 0 {
		sb.WriteByte('<')
		for i, ta := range e.TypeArgs {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(ta.JavaString())
		}
		sb.WriteByte('>')
	}
	sb.WriteByte('(')
	for i, arg := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(arg.ExprString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// StaticCallExpr is a static method call.
type StaticCallExpr struct {
	Class    string // e.g. "System.out" or "java.util.Arrays"
	Method   string
	TypeArgs []TypeRef
	Args     []Expr
}

func (e *StaticCallExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Class)
	sb.WriteByte('.')
	sb.WriteString(e.Method)
	if len(e.TypeArgs) > 0 {
		sb.WriteByte('<')
		for i, ta := range e.TypeArgs {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(ta.JavaString())
		}
		sb.WriteByte('>')
	}
	sb.WriteByte('(')
	for i, arg := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(arg.ExprString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// FieldAccessExpr is a field access expression.
type FieldAccessExpr struct {
	Receiver Expr
	Field    string
}

func (e *FieldAccessExpr) ExprString() string {
	return e.Receiver.ExprString() + "." + e.Field
}

// BinaryExpr is a binary expression.
type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (e *BinaryExpr) ExprString() string {
	return "(" + e.Left.ExprString() + " " + e.Op + " " + e.Right.ExprString() + ")"
}

// UnaryExpr is a unary expression (prefix or postfix).
type UnaryExpr struct {
	Op      string
	Operand Expr
	Postfix bool
}

func (e *UnaryExpr) ExprString() string {
	if e.Postfix {
		return e.Operand.ExprString() + e.Op
	}
	return e.Op + e.Operand.ExprString()
}

// LiteralExpr is a literal value.
type LiteralExpr struct {
	Value string // Java source literal, e.g. "42L", "3.14", "\"hello\"", "true", "null"
}

func (e *LiteralExpr) ExprString() string {
	return e.Value
}

// Param is a method/lambda parameter.
type Param struct {
	Type *TypeRef // nil = inferred (for lambda params)
	Name string
}

// LambdaExpr is a lambda expression.
type LambdaExpr struct {
	Params []Param
	Body   Expr   // expression body (used when Block is nil)
	Block  *Block // if non-nil, used as block body instead of Body
}

func (e *LambdaExpr) ExprString() string {
	var sb strings.Builder
	if len(e.Params) == 1 && e.Params[0].Type == nil {
		sb.WriteString(e.Params[0].Name)
	} else {
		sb.WriteByte('(')
		for i, p := range e.Params {
			if i > 0 {
				sb.WriteString(", ")
			}
			if p.Type != nil {
				sb.WriteString(p.Type.JavaString())
				sb.WriteByte(' ')
			}
			sb.WriteString(p.Name)
		}
		sb.WriteByte(')')
	}
	sb.WriteString(" -> ")
	if e.Block != nil {
		sb.WriteString(e.Block.blockString(0))
	} else {
		sb.WriteString(e.Body.ExprString())
	}
	return sb.String()
}

// CastExpr is a type cast.
type CastExpr struct {
	Type TypeRef
	X    Expr
}

func (e *CastExpr) ExprString() string {
	return "(" + e.Type.JavaString() + ") " + e.X.ExprString()
}

// NewExpr is a constructor call.
type NewExpr struct {
	Type TypeRef
	Args []Expr
}

func (e *NewExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Type.JavaString())
	sb.WriteByte('(')
	for i, arg := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(arg.ExprString())
	}
	sb.WriteByte(')')
	return sb.String()
}

// ArrayNewExpr is an array creation expression.
type ArrayNewExpr struct {
	ElemType TypeRef
	Size     Expr
}

func (e *ArrayNewExpr) ExprString() string {
	return "new " + e.ElemType.JavaString() + "[" + e.Size.ExprString() + "]"
}

// InstanceofExpr is an instanceof check, optionally with a binding pattern.
type InstanceofExpr struct {
	X       Expr
	Type    TypeRef
	Binding string // non-empty for pattern binding: "x instanceof Foo f"
}

func (e *InstanceofExpr) ExprString() string {
	s := e.X.ExprString() + " instanceof " + e.Type.JavaString()
	if e.Binding != "" {
		s += " " + e.Binding
	}
	return s
}

// ConditionalExpr is a ternary expression.
type ConditionalExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (e *ConditionalExpr) ExprString() string {
	return "(" + e.Cond.ExprString() + " ? " + e.Then.ExprString() + " : " + e.Else.ExprString() + ")"
}

// NameExpr is a simple name reference (variable, class name, etc).
type NameExpr struct {
	Name string
}

func (e *NameExpr) ExprString() string {
	return e.Name
}

// --- Type nodes ---

// TypeRef is a reference to a Java type.
type TypeRef struct {
	Name     string    // e.g. "long", "String", "java.util.List"
	TypeArgs []TypeRef // generic type arguments
	Array    bool      // true if this is an array type
	Wildcard bool      // true for "?"
}

func (t TypeRef) JavaString() string {
	if t.Wildcard {
		return "?"
	}
	s := t.Name
	if len(t.TypeArgs) > 0 {
		args := make([]string, len(t.TypeArgs))
		for i, ta := range t.TypeArgs {
			args[i] = ta.JavaString()
		}
		s += "<" + strings.Join(args, ", ") + ">"
	}
	if t.Array {
		s += "[]"
	}
	return s
}

// TypeParam is a generic type parameter with optional bounds.
type TypeParam struct {
	Name   string
	Bounds []TypeRef // "extends" bounds
}

func (tp TypeParam) javaString() string {
	if len(tp.Bounds) == 0 {
		return tp.Name
	}
	bounds := make([]string, len(tp.Bounds))
	for i, b := range tp.Bounds {
		bounds[i] = b.JavaString()
	}
	return tp.Name + " extends " + strings.Join(bounds, " & ")
}

// --- Helpers ---

// Primitive TypeRef shortcuts.
var (
	TypeLong    = TypeRef{Name: "long"}
	TypeDouble  = TypeRef{Name: "double"}
	TypeBoolean = TypeRef{Name: "boolean"}
	TypeVoid    = TypeRef{Name: "void"}
	TypeString  = TypeRef{Name: "String"}
	TypeObject  = TypeRef{Name: "Object"}
)

// Lit returns a LiteralExpr.
func Lit(v string) *LiteralExpr { return &LiteralExpr{Value: v} }

// LongLit returns a long literal expression.
func LongLit(v int64) *LiteralExpr { return &LiteralExpr{Value: fmt.Sprintf("%dL", v)} }

// DoubleLit returns a double literal expression.
func DoubleLit(v float64) *LiteralExpr { return &LiteralExpr{Value: fmt.Sprintf("%v", v)} }

// StringLit returns a Java string literal with proper escaping.
func StringLit(v string) *LiteralExpr {
	var sb strings.Builder
	sb.WriteByte('"')
	for _, r := range v {
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
	return &LiteralExpr{Value: sb.String()}
}

// BoolLit returns a boolean literal expression.
func BoolLit(v bool) *LiteralExpr {
	if v {
		return &LiteralExpr{Value: "true"}
	}
	return &LiteralExpr{Value: "false"}
}

// Null is the null literal.
var Null = &LiteralExpr{Value: "null"}
