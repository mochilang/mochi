package csharpsrc

import (
	"fmt"
	"strings"
)

// CSString(indent int) string implementations produce C# source fragments.
// Callers are responsible for indentation; nodes always start at column 0.

// --- Declaration nodes ---

// TypeDecl is the common interface for all type declarations.
type TypeDecl interface {
	isTypeDecl()
	CSString(indent int) string
}

// CompilationUnit is the top-level C# source file.
type CompilationUnit struct {
	Namespace string     // e.g. "Mochi.User"
	Usings    []string   // e.g. ["System", "System.Collections.Generic"]
	Types     []TypeDecl // class/record/interface declarations
}

// CSSource returns the full C# source text for this compilation unit.
func (cu *CompilationUnit) CSSource() string {
	var sb strings.Builder
	for _, u := range cu.Usings {
		fmt.Fprintf(&sb, "using %s;\n", u)
	}
	if len(cu.Usings) > 0 {
		sb.WriteString("\n")
	}
	if cu.Namespace != "" {
		fmt.Fprintf(&sb, "namespace %s;\n\n", cu.Namespace)
	}
	for _, td := range cu.Types {
		sb.WriteString(td.CSString(0))
		sb.WriteString("\n")
	}
	return sb.String()
}

// CSString implements TypeDecl (so CompilationUnit can be embedded); unused externally.
func (cu *CompilationUnit) CSString(indent int) string { return cu.CSSource() }
func (*CompilationUnit) isTypeDecl()                   {}

// NamespaceDecl is an explicit namespace block (used when file-scoped ns is not desired).
type NamespaceDecl struct {
	Name  string
	Types []TypeDecl
}

func (*NamespaceDecl) isTypeDecl() {}

func (n *NamespaceDecl) CSString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%snamespace %s\n%s{\n", pad, n.Name, pad)
	for _, td := range n.Types {
		sb.WriteString(td.CSString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// ClassDecl is a class declaration (static class for modules).
type ClassDecl struct {
	Modifiers []string // e.g. ["public", "static"]
	Name      string
	Members   []Member
}

func (*ClassDecl) isTypeDecl() {}

func (c *ClassDecl) CSString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range c.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("class ")
	sb.WriteString(c.Name)
	sb.WriteString("\n" + pad + "{\n")
	for _, mem := range c.Members {
		sb.WriteString(mem.memberString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// RecordDecl is a positional sealed record class (for product types).
type RecordDecl struct {
	Modifiers  []string // e.g. ["public", "sealed"]
	Name       string
	Components []RecordComponent
	Interfaces []TypeRef
	Members    []Member
}

func (*RecordDecl) isTypeDecl() {}

// RecordComponent is a single positional record parameter.
type RecordComponent struct {
	Type TypeRef
	Name string
}

func (r *RecordDecl) CSString(indent int) string {
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
		sb.WriteString(c.Type.CSString())
		sb.WriteByte(' ')
		sb.WriteString(c.Name)
	}
	sb.WriteByte(')')
	if len(r.Interfaces) > 0 {
		sb.WriteString(" : ")
		for i, iface := range r.Interfaces {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(iface.CSString())
		}
	}
	if len(r.Members) == 0 {
		sb.WriteString(";")
	} else {
		sb.WriteString("\n" + pad + "{\n")
		for _, mem := range r.Members {
			sb.WriteString(mem.memberString(indent + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad + "}")
	}
	return sb.String()
}

// RecordStructDecl is a readonly record struct (for value types).
type RecordStructDecl struct {
	Modifiers  []string
	Name       string
	Components []RecordComponent
	Members    []Member
}

func (*RecordStructDecl) isTypeDecl() {}

func (r *RecordStructDecl) CSString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range r.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("record struct ")
	sb.WriteString(r.Name)
	sb.WriteByte('(')
	for i, c := range r.Components {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(c.Type.CSString())
		sb.WriteByte(' ')
		sb.WriteString(c.Name)
	}
	sb.WriteByte(')')
	if len(r.Members) == 0 {
		sb.WriteString(";")
	} else {
		sb.WriteString("\n" + pad + "{\n")
		for _, mem := range r.Members {
			sb.WriteString(mem.memberString(indent + 1))
			sb.WriteString("\n")
		}
		sb.WriteString(pad + "}")
	}
	return sb.String()
}

// AbstractRecordDecl is an abstract record class used as a sum type base.
type AbstractRecordDecl struct {
	Modifiers  []string // e.g. ["public", "abstract"]
	Name       string
	Interfaces []TypeRef
	Members    []Member
	Subtypes   []TypeDecl // nested variant record declarations
}

func (*AbstractRecordDecl) isTypeDecl() {}

func (a *AbstractRecordDecl) CSString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range a.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString("record ")
	sb.WriteString(a.Name)
	if len(a.Interfaces) > 0 {
		sb.WriteString(" : ")
		for i, iface := range a.Interfaces {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(iface.CSString())
		}
	}
	sb.WriteString("\n" + pad + "{\n")
	for _, mem := range a.Members {
		sb.WriteString(mem.memberString(indent + 1))
		sb.WriteString("\n")
	}
	for _, sub := range a.Subtypes {
		sb.WriteString(sub.CSString(indent + 1))
		sb.WriteString("\n")
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// Member is the common interface for class/record members.
type Member interface {
	memberString(indent int) string
}

// InnerTypeDecl wraps a TypeDecl so it can appear as a nested type member.
type InnerTypeDecl struct {
	Decl TypeDecl
}

func (i *InnerTypeDecl) memberString(indent int) string {
	return i.Decl.CSString(indent)
}

// FieldDecl is a field declaration.
type FieldDecl struct {
	Modifiers []string // e.g. ["public", "static", "readonly"]
	Type      TypeRef
	Name      string
	Init      Expr // nil if no initialiser
}

func (f *FieldDecl) memberString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, m := range f.Modifiers {
		sb.WriteString(m)
		sb.WriteByte(' ')
	}
	sb.WriteString(f.Type.CSString())
	sb.WriteByte(' ')
	sb.WriteString(f.Name)
	if f.Init != nil {
		sb.WriteString(" = ")
		sb.WriteString(f.Init.ExprString())
	}
	sb.WriteByte(';')
	return sb.String()
}

// MethodDecl is a sync static method declaration.
type MethodDecl struct {
	Modifiers  []string // e.g. ["public", "static"]
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
	sb.WriteString(m.ReturnType.CSString())
	sb.WriteByte(' ')
	sb.WriteString(m.Name)
	sb.WriteByte('(')
	for i, p := range m.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Type.CSString())
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

// AsyncMethodDecl is an async Task<T> method declaration.
type AsyncMethodDecl struct {
	Modifiers  []string // e.g. ["public", "static"]
	ReturnType TypeRef  // inner return type (Task<T> wrapper is added automatically)
	Name       string
	Params     []Param
	Body       *Block
}

func (a *AsyncMethodDecl) memberString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	for _, mod := range a.Modifiers {
		sb.WriteString(mod)
		sb.WriteByte(' ')
	}
	sb.WriteString("async ")
	// Wrap return type in Task<> unless it is void -> Task
	rt := a.ReturnType.CSString()
	if rt == "void" {
		sb.WriteString("Task")
	} else {
		sb.WriteString("Task<")
		sb.WriteString(rt)
		sb.WriteByte('>')
	}
	sb.WriteByte(' ')
	sb.WriteString(a.Name)
	sb.WriteByte('(')
	for i, p := range a.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Type.CSString())
		sb.WriteByte(' ')
		sb.WriteString(p.Name)
	}
	sb.WriteByte(')')
	if a.Body == nil {
		sb.WriteByte(';')
	} else {
		sb.WriteByte(' ')
		sb.WriteString(a.Body.blockString(indent))
	}
	return sb.String()
}

// Param is a method or lambda parameter.
type Param struct {
	Type TypeRef
	Name string
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
		return "{ }"
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
		sb.WriteString("\n" + pad + "else ")
		sb.WriteString(s.Else.blockString(indent))
	}
	return sb.String()
}

// ForeachStmt is a foreach loop.
type ForeachStmt struct {
	ElemType TypeRef
	ElemName string
	Iter     Expr
	Body     Block
}

func (s *ForeachStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + "foreach (" + s.ElemType.CSString() + " " + s.ElemName + " in " + s.Iter.ExprString() + ") " + s.Body.blockString(indent)
}

// ForStmt is a classic for loop.
type ForStmt struct {
	Init   Stmt // LocalDeclStmt or ExprStmt; nil for empty init
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

// WhileStmt is a while loop.
type WhileStmt struct {
	Cond Expr
	Body Block
}

func (s *WhileStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	return pad + "while (" + s.Cond.ExprString() + ") " + s.Body.blockString(indent)
}

// EmptyStmt is a no-op statement (renders as nothing).
type EmptyStmt struct{}

func (*EmptyStmt) stmtString(int) string { return "" }

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

// LocalDeclStmt is a local variable declaration (var or typed).
type LocalDeclStmt struct {
	Type *TypeRef // nil means use var
	Name string
	Init Expr // nil if no initialiser
}

func (s *LocalDeclStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	typeName := "var"
	if s.Type != nil {
		typeName = s.Type.CSString()
	}
	if s.Init == nil {
		return pad + typeName + " " + s.Name + ";"
	}
	return pad + typeName + " " + s.Name + " = " + s.Init.ExprString() + ";"
}

// AssignStmt is an assignment statement: Target = Value;
type AssignStmt struct {
	Target Expr
	Value  Expr
}

func (s *AssignStmt) stmtString(indent int) string {
	return strings.Repeat("    ", indent) + s.Target.ExprString() + " = " + s.Value.ExprString() + ";"
}

// TryCatchStmt is a try/catch statement.
type TryCatchStmt struct {
	Body      Block
	CatchVar  string
	CatchType TypeRef
	CatchBody Block
}

func (s *TryCatchStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	var sb strings.Builder
	sb.WriteString(pad)
	sb.WriteString("try ")
	sb.WriteString(s.Body.blockString(indent))
	sb.WriteString("\n" + pad + "catch (")
	sb.WriteString(s.CatchType.CSString())
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

// SwitchCaseClause is one arm of a SwitchStmt.
// For a type-pattern case, Label is the C# pattern (e.g. "Circle __mc_Circle").
// For the default arm, IsDefault is true and Label is ignored.
type SwitchCaseClause struct {
	IsDefault bool
	NoBreak   bool   // suppress trailing break when body always throws
	Label     string // C# pattern label, e.g. "Circle __mc" or "" for default
	Body      []Stmt
}

// SwitchStmt is a C# switch statement with type-pattern cases.
type SwitchStmt struct {
	Tag   Expr
	Cases []SwitchCaseClause
}

func (s *SwitchStmt) stmtString(indent int) string {
	pad := strings.Repeat("    ", indent)
	inner := strings.Repeat("    ", indent+1)
	var sb strings.Builder
	fmt.Fprintf(&sb, "%sswitch (%s)\n%s{\n", pad, s.Tag.ExprString(), pad)
	for _, c := range s.Cases {
		if c.IsDefault {
			fmt.Fprintf(&sb, "%sdefault:\n", inner)
		} else {
			fmt.Fprintf(&sb, "%scase %s:\n", inner, c.Label)
		}
		fmt.Fprintf(&sb, "%s{\n", inner)
		for _, st := range c.Body {
			sb.WriteString(st.stmtString(indent + 2))
			sb.WriteString("\n")
		}
		if !c.NoBreak {
			fmt.Fprintf(&sb, "%sbreak;\n", strings.Repeat("    ", indent+2))
		}
		fmt.Fprintf(&sb, "%s}\n", inner)
	}
	sb.WriteString(pad + "}")
	return sb.String()
}

// --- Expression nodes ---

// Expr is the common interface for all expressions.
type Expr interface {
	ExprString() string
}

// SwitchArm is a single arm in a switch expression.
type SwitchArm struct {
	Pattern Pattern // nil means default
	Guard   Expr    // optional when guard
	Body    Expr
}

// SwitchExpr is a C# switch expression.
type SwitchExpr struct {
	Tag  Expr
	Arms []SwitchArm
}

func (e *SwitchExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Tag.ExprString())
	sb.WriteString(" switch\n{\n")
	for _, arm := range e.Arms {
		sb.WriteString("    ")
		if arm.Pattern == nil {
			sb.WriteString("_")
		} else {
			sb.WriteString(arm.Pattern.patternString())
		}
		if arm.Guard != nil {
			sb.WriteString(" when ")
			sb.WriteString(arm.Guard.ExprString())
		}
		sb.WriteString(" => ")
		sb.WriteString(arm.Body.ExprString())
		sb.WriteString(",\n")
	}
	sb.WriteString("}")
	return sb.String()
}

// CallExpr is an instance method call.
type CallExpr struct {
	Receiver Expr
	Method   string
	Args     []Expr
}

func (e *CallExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Receiver.ExprString())
	sb.WriteByte('.')
	sb.WriteString(e.Method)
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
	Class  string // e.g. "Console" or "System.Math"
	Method string
	Args   []Expr
}

func (e *StaticCallExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Class)
	sb.WriteByte('.')
	sb.WriteString(e.Method)
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

// LambdaExpr is a lambda expression (x => expr or (x, y) => expr).
type LambdaExpr struct {
	Params []Param
	Body   Expr   // expression body (used when Block is nil)
	Block  *Block // if non-nil, used as block body instead of Body
}

func (e *LambdaExpr) ExprString() string {
	var sb strings.Builder
	if len(e.Params) == 1 {
		sb.WriteString(e.Params[0].Name)
	} else {
		sb.WriteByte('(')
		for i, p := range e.Params {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(p.Name)
		}
		sb.WriteByte(')')
	}
	sb.WriteString(" => ")
	if e.Block != nil {
		sb.WriteString(e.Block.blockString(0))
	} else {
		sb.WriteString(e.Body.ExprString())
	}
	return sb.String()
}

// DelegateCallExpr invokes a delegate/Func value: callee(arg1, arg2, ...).
type DelegateCallExpr struct {
	Callee Expr
	Args   []Expr
}

func (e *DelegateCallExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString(e.Callee.ExprString())
	sb.WriteByte('(')
	for i, a := range e.Args {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.ExprString())
	}
	sb.WriteByte(')')
	return sb.String()
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

// AgentNewExpr creates a mutable object with an object-initializer: new T() { field = val, ... }.
type AgentNewExpr struct {
	Type  TypeRef
	Inits []DictEntry // Key is a NameExpr for the field; Value is the initializer
}

func (e *AgentNewExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Type.CSString())
	sb.WriteString("()")
	if len(e.Inits) > 0 {
		sb.WriteString(" { ")
		for i, init := range e.Inits {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(init.Key.ExprString())
			sb.WriteString(" = ")
			sb.WriteString(init.Value.ExprString())
		}
		sb.WriteString(" }")
	}
	return sb.String()
}

// UnaryExpr is a unary expression.
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
	Value string // C# source literal, e.g. "42L", "3.14", "\"hello\"", "true", "null"
}

func (e *LiteralExpr) ExprString() string { return e.Value }

// NewExpr is a constructor call.
type NewExpr struct {
	Type TypeRef
	Args []Expr
}

func (e *NewExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Type.CSString())
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

// FieldAccessExpr is a field or property access expression.
type FieldAccessExpr struct {
	Receiver Expr
	Field    string
}

func (e *FieldAccessExpr) ExprString() string {
	return e.Receiver.ExprString() + "." + e.Field
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

// CastExpr is an explicit type cast: (T)expr.
type CastExpr struct {
	Type TypeRef
	X    Expr
}

func (e *CastExpr) ExprString() string {
	return "((" + e.Type.CSString() + ")" + e.X.ExprString() + ")"
}

// IndexAccessExpr is a subscript access: receiver[index].
type IndexAccessExpr struct {
	Receiver Expr
	Index    Expr
}

func (e *IndexAccessExpr) ExprString() string {
	return e.Receiver.ExprString() + "[" + e.Index.ExprString() + "]"
}

// CollectionInitExpr is a collection object-creation expression:
//
//	new List<T>(ctorArgs) { elem1, elem2, ... }
//
// CtorArgs may be empty (no constructor args). Elems may be empty (empty initializer).
type CollectionInitExpr struct {
	Type     TypeRef
	CtorArgs []Expr
	Elems    []Expr
}

func (e *CollectionInitExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Type.CSString())
	sb.WriteByte('(')
	for i, a := range e.CtorArgs {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(a.ExprString())
	}
	sb.WriteByte(')')
	if len(e.Elems) > 0 {
		sb.WriteString(" { ")
		for i, el := range e.Elems {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(el.ExprString())
		}
		sb.WriteString(" }")
	}
	return sb.String()
}

// DictEntry is a single key-value pair in a DictInitExpr.
type DictEntry struct {
	Key   Expr
	Value Expr
}

// DictInitExpr is a dictionary object-creation expression:
//
//	new Dictionary<K,V> { { k1, v1 }, { k2, v2 } }
type DictInitExpr struct {
	Type    TypeRef
	Entries []DictEntry
}

func (e *DictInitExpr) ExprString() string {
	var sb strings.Builder
	sb.WriteString("new ")
	sb.WriteString(e.Type.CSString())
	sb.WriteString("()")
	if len(e.Entries) > 0 {
		sb.WriteString(" { ")
		for i, en := range e.Entries {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString("{ ")
			sb.WriteString(en.Key.ExprString())
			sb.WriteString(", ")
			sb.WriteString(en.Value.ExprString())
			sb.WriteString(" }")
		}
		sb.WriteString(" }")
	}
	return sb.String()
}

// NameExpr is a simple name reference.
type NameExpr struct {
	Name string
}

func (e *NameExpr) ExprString() string { return e.Name }

// AwaitExpr wraps an expression with await.
type AwaitExpr struct {
	X Expr
}

func (e *AwaitExpr) ExprString() string { return "await " + e.X.ExprString() }

// --- Pattern nodes ---

// Pattern is the common interface for switch/is patterns.
type Pattern interface {
	patternString() string
}

// TypePattern matches a type with a binding variable.
type TypePattern struct {
	Type TypeRef
	Name string // binding variable name; empty for discard
}

func (p *TypePattern) patternString() string {
	if p.Name == "" {
		return p.Type.CSString()
	}
	return p.Type.CSString() + " " + p.Name
}

// WildcardPattern matches anything (the _ discard pattern).
type WildcardPattern struct{}

func (*WildcardPattern) patternString() string { return "_" }

// --- Type nodes ---

// TypeRef is a reference to a C# type.
type TypeRef struct {
	Name     string    // e.g. "long", "string", "System.Collections.Generic.List"
	TypeArgs []TypeRef // generic type arguments
	Array    bool      // true if this is an array type
	Nullable bool      // true for T?
}

func (t TypeRef) CSString() string {
	s := t.Name
	if len(t.TypeArgs) > 0 {
		args := make([]string, len(t.TypeArgs))
		for i, ta := range t.TypeArgs {
			args[i] = ta.CSString()
		}
		s += "<" + strings.Join(args, ", ") + ">"
	}
	if t.Array {
		s += "[]"
	}
	if t.Nullable {
		s += "?"
	}
	return s
}

// --- Primitive TypeRef shortcuts ---

var (
	TypeLong    = TypeRef{Name: "long"}
	TypeDouble  = TypeRef{Name: "double"}
	TypeBool    = TypeRef{Name: "bool"}
	TypeVoid    = TypeRef{Name: "void"}
	TypeString  = TypeRef{Name: "string"}
	TypeObject  = TypeRef{Name: "object"}
	TypeInt     = TypeRef{Name: "int"}
	TypeTask    = TypeRef{Name: "Task"}
)

// ListTypeRef returns a List<T> TypeRef.
func ListTypeRef(elem TypeRef) TypeRef {
	return TypeRef{Name: "List", TypeArgs: []TypeRef{elem}}
}

// DictTypeRef returns a Dictionary<K,V> TypeRef.
func DictTypeRef(key, val TypeRef) TypeRef {
	return TypeRef{Name: "Dictionary", TypeArgs: []TypeRef{key, val}}
}

// HashSetTypeRef returns a HashSet<T> TypeRef.
func HashSetTypeRef(elem TypeRef) TypeRef {
	return TypeRef{Name: "HashSet", TypeArgs: []TypeRef{elem}}
}

// BlockingCollectionTypeRef returns a BlockingCollection<T> TypeRef.
func BlockingCollectionTypeRef(elem TypeRef) TypeRef {
	return TypeRef{Name: "BlockingCollection", TypeArgs: []TypeRef{elem}}
}

// MochiStreamTypeRef returns a MochiStream<T> TypeRef.
func MochiStreamTypeRef(elem TypeRef) TypeRef {
	return TypeRef{Name: "MochiStream", TypeArgs: []TypeRef{elem}}
}

// --- Literal helpers ---

// Lit returns a LiteralExpr.
func Lit(v string) *LiteralExpr { return &LiteralExpr{Value: v} }

// LongLit returns a long literal expression.
func LongLit(v int64) *LiteralExpr { return &LiteralExpr{Value: fmt.Sprintf("%dL", v)} }

// DoubleLit returns a double literal expression.
func DoubleLit(v float64) *LiteralExpr { return &LiteralExpr{Value: fmt.Sprintf("%vd", v)} }

// StringLit returns a C# verbatim string literal with proper escaping.
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
