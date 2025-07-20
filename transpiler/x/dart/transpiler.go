//go:build slow

package dartt

import (
	"fmt"
	"io"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- Simple Dart AST ---

// Program represents a sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) error }

// WhileStmt represents a simple while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (w *WhileStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "while ("); err != nil {
		return err
	}
	if err := w.Cond.emit(out); err != nil {
		return err
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range w.Body {
		if _, err := io.WriteString(out, "  "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		if _, ok := st.(*IfStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*WhileStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForRangeStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else if _, ok := st.(*ForInStmt); ok {
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(out, "}")
	return err
}

// ForRangeStmt represents a numeric for-loop like `for i in 0..10 {}`.
type ForRangeStmt struct {
	Name  string
	Start Expr
	End   Expr
	Body  []Stmt
}

func (f *ForRangeStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "for (var "+f.Name+" = "); err != nil {
		return err
	}
	if f.Start != nil {
		if err := f.Start.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+" < "); err != nil {
		return err
	}
	if f.End != nil {
		if err := f.End.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, "; "+f.Name+"++) {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "  "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(out, "}")
	return err
}

// ForInStmt represents iteration over an iterable expression.
type ForInStmt struct {
	Name     string
	Iterable Expr
	Body     []Stmt
}

func (f *ForInStmt) emit(out io.Writer) error {
	if _, err := io.WriteString(out, "for (var "+f.Name+" in "); err != nil {
		return err
	}
	if f.Iterable != nil {
		if err := f.Iterable.emit(out); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(out, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(out, "  "); err != nil {
			return err
		}
		if err := st.emit(out); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt:
			if _, err := io.WriteString(out, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(out, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(out, "}")
	return err
}

// IfStmt represents a conditional statement with an optional else branch.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (s *IfStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "if ("); err != nil {
		return err
	}
	if err := s.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range s.Then {
		if _, err := io.WriteString(w, "    "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ";\n"); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, "  }"); err != nil {
		return err
	}
	if len(s.Else) > 0 {
		if _, err := io.WriteString(w, " else {\n"); err != nil {
			return err
		}
		for _, st := range s.Else {
			if _, err := io.WriteString(w, "    "); err != nil {
				return err
			}
			if err := st.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, "  }"); err != nil {
			return err
		}
	}
	return nil
}

type VarStmt struct {
	Name  string
	Value Expr
}

func (s *VarStmt) emit(w io.Writer) error {
	typ := inferType(s.Value)
	if typ == "var" {
		if _, err := io.WriteString(w, "var "+s.Name); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, typ+" "+s.Name); err != nil {
			return err
		}
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " = "); err != nil {
			return err
		}
		return s.Value.emit(w)
	}
	return nil
}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, s.Name+" = "); err != nil {
		return err
	}
	return s.Value.emit(w)
}

type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) error {
	typ := inferType(s.Value)
	if typ == "var" {
		if _, err := io.WriteString(w, "var "+s.Name+" = "); err != nil {
			return err
		}
	} else {
		if _, err := io.WriteString(w, typ+" "+s.Name+" = "); err != nil {
			return err
		}
	}
	return s.Value.emit(w)
}

// ReturnStmt represents a `return` statement.
type ReturnStmt struct {
	Value Expr
}

func (s *ReturnStmt) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "return"); err != nil {
		return err
	}
	if s.Value != nil {
		if _, err := io.WriteString(w, " "); err != nil {
			return err
		}
		if err := s.Value.emit(w); err != nil {
			return err
		}
	}
	return nil
}

// BreakStmt represents a `break` statement.
type BreakStmt struct{}

func (s *BreakStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "break")
	return err
}

// ContinueStmt represents a `continue` statement.
type ContinueStmt struct{}

func (s *ContinueStmt) emit(w io.Writer) error {
	_, err := io.WriteString(w, "continue")
	return err
}

// FuncDecl represents a function definition.
type FuncDecl struct {
	Name   string
	Params []string
	Body   []Stmt
}

func (f *FuncDecl) emit(w io.Writer) error {
	retType := inferReturnType(f.Body)
	if _, err := io.WriteString(w, retType+" "+f.Name+"("); err != nil {
		return err
	}
	for i, p := range f.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") {\n"); err != nil {
		return err
	}
	for _, st := range f.Body {
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		switch st.(type) {
		case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl:
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		default:
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) error { return s.Expr.emit(w) }

type Expr interface{ emit(io.Writer) error }

type UnaryExpr struct {
	Op string
	X  Expr
}

func (u *UnaryExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, u.Op); err != nil {
		return err
	}
	return u.X.emit(w)
}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) error {
	if b.Op == "in" {
		if err := b.Right.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ".contains("); err != nil {
			return err
		}
		if err := b.Left.emit(w); err != nil {
			return err
		}
		_, err := io.WriteString(w, ")")
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if err := b.Left.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " "+b.Op+" "); err != nil {
		return err
	}
	if err := b.Right.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// CondExpr represents a conditional expression like `cond ? a : b`.
type CondExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (c *CondExpr) emit(w io.Writer) error {
	if err := c.Cond.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " ? "); err != nil {
		return err
	}
	if err := c.Then.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, " : "); err != nil {
		return err
	}
	return c.Else.emit(w)
}

type CallExpr struct {
	Func Expr
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) error {
	if err := c.Func.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := a.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

type Name struct{ Name string }

func (n *Name) emit(w io.Writer) error { _, err := io.WriteString(w, n.Name); return err }

// SelectorExpr represents receiver.field access.
type SelectorExpr struct {
	Receiver Expr
	Field    string
}

func (s *SelectorExpr) emit(w io.Writer) error {
	if err := s.Receiver.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "."+s.Field)
	return err
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%q", s.Value); return err }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) error { _, err := fmt.Fprintf(w, "%d", i.Value); return err }

// BoolLit represents a boolean literal.
type BoolLit struct{ Value bool }

func (b *BoolLit) emit(w io.Writer) error {
	if b.Value {
		_, err := io.WriteString(w, "true")
		return err
	}
	_, err := io.WriteString(w, "false")
	return err
}

// ListLit represents a list literal.
type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	for i, e := range l.Elems {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := e.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

// MapLit represents a simple map literal.
type MapLit struct{ Entries []MapEntry }

// MapEntry is a key/value pair inside a map literal.
type MapEntry struct {
	Key   Expr
	Value Expr
}

func (m *MapLit) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "{"); err != nil {
		return err
	}
	for i, e := range m.Entries {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if err := e.Key.emit(w); err != nil {
			return err
		}
		if _, err := io.WriteString(w, ": "); err != nil {
			return err
		}
		if err := e.Value.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "}")
	return err
}

// IndexExpr represents target[index].
type IndexExpr struct {
	Target Expr
	Index  Expr
}

func (i *IndexExpr) emit(w io.Writer) error {
	if err := i.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if i.Index != nil {
		if err := i.Index.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, "]")
	return err
}

// SubstringExpr represents substring(s, start, end).
type SubstringExpr struct {
	Str   Expr
	Start Expr
	End   Expr
}

func (s *SubstringExpr) emit(w io.Writer) error {
	if err := s.Str.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".substring("); err != nil {
		return err
	}
	if s.Start != nil {
		if err := s.Start.emit(w); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if s.End != nil {
		if err := s.End.emit(w); err != nil {
			return err
		}
	}
	_, err := io.WriteString(w, ")")
	return err
}

// ContainsExpr represents right.contains(left).
type ContainsExpr struct {
	Target Expr
	Elem   Expr
}

func (c *ContainsExpr) emit(w io.Writer) error {
	if err := c.Target.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".contains("); err != nil {
		return err
	}
	if err := c.Elem.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// AppendExpr represents append(list, value).
type AppendExpr struct {
	List  Expr
	Value Expr
}

func (a *AppendExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "["); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "..."); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ", "); err != nil {
		return err
	}
	if err := a.Value.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, "]")
	return err
}

// AvgExpr represents avg(list).
type AvgExpr struct{ List Expr }

func (a *AvgExpr) emit(w io.Writer) error {
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".isEmpty ? 0 : ("); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, ".reduce((a, b) => a + b) / "); err != nil {
		return err
	}
	if err := a.List.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length)")
	return err
}

// LambdaExpr represents an inline function expression.
type LambdaExpr struct {
	Params []string
	Body   Expr
}

func (l *LambdaExpr) emit(w io.Writer) error {
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "("); err != nil {
		return err
	}
	for i, p := range l.Params {
		if i > 0 {
			if _, err := io.WriteString(w, ", "); err != nil {
				return err
			}
		}
		if _, err := io.WriteString(w, p); err != nil {
			return err
		}
	}
	if _, err := io.WriteString(w, ") => "); err != nil {
		return err
	}
	if err := l.Body.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ")")
	return err
}

// LenExpr represents the `len` builtin.
type LenExpr struct{ X Expr }

func (l *LenExpr) emit(w io.Writer) error {
	if err := l.X.emit(w); err != nil {
		return err
	}
	_, err := io.WriteString(w, ".length")
	return err
}

// inferType attempts to guess the Dart type for the given expression.
func inferType(e Expr) string {
	switch ex := e.(type) {
	case *IntLit:
		return "int"
	case *BoolLit:
		return "bool"
	case *StringLit:
		return "String"
	case *Name:
		return "var"
	case *ListLit:
		if len(ex.Elems) == 0 {
			return "List<dynamic>"
		}
		typ := inferType(ex.Elems[0])
		for _, el := range ex.Elems[1:] {
			if t := inferType(el); t != typ {
				typ = "dynamic"
				break
			}
		}
		return "List<" + typ + ">"
	case *MapLit:
		if len(ex.Entries) == 0 {
			return "Map<dynamic, dynamic>"
		}
		kt := inferType(ex.Entries[0].Key)
		vt := inferType(ex.Entries[0].Value)
		for _, it := range ex.Entries[1:] {
			if t := inferType(it.Key); t != kt {
				kt = "dynamic"
			}
			if t := inferType(it.Value); t != vt {
				vt = "dynamic"
			}
		}
		return "Map<" + kt + ", " + vt + ">"
	case *BinaryExpr:
		switch ex.Op {
		case "+":
			lt := inferType(ex.Left)
			rt := inferType(ex.Right)
			if lt == "String" || rt == "String" {
				return "String"
			}
			return "int"
		case "-", "*", "/", "%":
			return "int"
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||":
			return "bool"
		default:
			return "var"
		}
	case *UnaryExpr:
		if ex.Op == "-" {
			return "int"
		}
		return inferType(ex.X)
	case *CondExpr:
		t1 := inferType(ex.Then)
		t2 := inferType(ex.Else)
		if t1 == t2 {
			return t1
		}
		return "dynamic"
	case *CallExpr:
		if n, ok := ex.Func.(*Name); ok {
			switch n.Name {
			case "len":
				return "int"
			case "avg":
				return "num"
			case "append":
				if len(ex.Args) > 0 {
					return inferType(ex.Args[0])
				}
			}
		}
		return "dynamic"
	case *SelectorExpr, *IndexExpr:
		return "dynamic"
	case *ContainsExpr:
		return "bool"
	case *LenExpr:
		return "int"
	case *SubstringExpr:
		return "String"
	case *AppendExpr:
		return inferType(ex.List)
	case *AvgExpr:
		return "num"
	default:
		if e == nil {
			return "var"
		}
		return "var"
	}
}

func inferReturnType(body []Stmt) string {
	if len(body) == 0 {
		return "void"
	}
	if ret, ok := body[len(body)-1].(*ReturnStmt); ok {
		if ret.Value == nil {
			return "void"
		}
		t := inferType(ret.Value)
		if t == "var" {
			return "dynamic"
		}
		return t
	}
	return "void"
}

func emitExpr(w io.Writer, e Expr) error { return e.emit(w) }

func isBlockStmt(s Stmt) bool {
	switch s.(type) {
	case *IfStmt, *WhileStmt, *ForRangeStmt, *ForInStmt, *FuncDecl:
		return true
	default:
		return false
	}
}

// Emit writes Dart source for p to w.
func Emit(w io.Writer, p *Program) error {
	if _, err := io.WriteString(w, "// Generated by Mochi transpiler\n"); err != nil {
		return err
	}
	for _, st := range p.Stmts {
		if fd, ok := st.(*FuncDecl); ok {
			if err := fd.emit(w); err != nil {
				return err
			}
			if _, err := io.WriteString(w, "\n\n"); err != nil {
				return err
			}
		}
	}
	if _, err := io.WriteString(w, "void main() {\n"); err != nil {
		return err
	}
	for _, st := range p.Stmts {
		if _, ok := st.(*FuncDecl); ok {
			continue
		}
		if _, err := io.WriteString(w, "  "); err != nil {
			return err
		}
		if err := st.emit(w); err != nil {
			return err
		}
		if isBlockStmt(st) {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		} else {
			if _, err := io.WriteString(w, ";\n"); err != nil {
				return err
			}
		}
	}
	_, err := io.WriteString(w, "}\n")
	return err
}

// Transpile converts a Mochi program into a simple Dart AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	p := &Program{}
	for _, st := range prog.Statements {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		p.Stmts = append(p.Stmts, s)
	}
	_ = env
	return p, nil
}

func convertIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := convertExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenStmts, err := convertStmtList(i.Then)
	if err != nil {
		return nil, err
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := convertIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		elseStmts, err = convertStmtList(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func convertWhileStmt(wst *parser.WhileStmt) (Stmt, error) {
	cond, err := convertExpr(wst.Cond)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(wst.Body)
	if err != nil {
		return nil, err
	}
	return &WhileStmt{Cond: cond, Body: body}, nil
}

func convertForStmt(fst *parser.ForStmt) (Stmt, error) {
	if fst.RangeEnd != nil {
		start, err := convertExpr(fst.Source)
		if err != nil {
			return nil, err
		}
		end, err := convertExpr(fst.RangeEnd)
		if err != nil {
			return nil, err
		}
		body, err := convertStmtList(fst.Body)
		if err != nil {
			return nil, err
		}
		return &ForRangeStmt{Name: fst.Name, Start: start, End: end, Body: body}, nil
	}
	iter, err := convertExpr(fst.Source)
	if err != nil {
		return nil, err
	}
	body, err := convertStmtList(fst.Body)
	if err != nil {
		return nil, err
	}
	return &ForInStmt{Name: fst.Name, Iterable: iter, Body: body}, nil
}

func convertIfExpr(ie *parser.IfExpr) (Expr, error) {
	cond, err := convertExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := convertExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if ie.ElseIf != nil {
		elseExpr, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseExpr, err = convertExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	} else {
		elseExpr = &IntLit{Value: 0}
	}
	return &CondExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

func convertStmtList(list []*parser.Statement) ([]Stmt, error) {
	var out []Stmt
	for _, st := range list {
		s, err := convertStmtInternal(st)
		if err != nil {
			return nil, err
		}
		out = append(out, s)
	}
	return out, nil
}

func convertStmtInternal(st *parser.Statement) (Stmt, error) {
	switch {
	case st.Expr != nil:
		e, err := convertExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case st.Let != nil:
		var e Expr
		var err error
		if st.Let.Value != nil {
			e, err = convertExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil && st.Let.Type.Simple != nil && *st.Let.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		} else {
			return nil, fmt.Errorf("let missing value not supported")
		}
		return &LetStmt{Name: st.Let.Name, Value: e}, nil
	case st.Var != nil:
		var e Expr
		if st.Var.Value != nil {
			var err error
			e, err = convertExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil && st.Var.Type.Simple != nil && *st.Var.Type.Simple == "int" {
			e = &IntLit{Value: 0}
		}
		return &VarStmt{Name: st.Var.Name, Value: e}, nil
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return nil, fmt.Errorf("complex assignment not supported")
		}
		e, err := convertExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &AssignStmt{Name: st.Assign.Name, Value: e}, nil
	case st.Return != nil:
		var e Expr
		if st.Return.Value != nil {
			var err error
			e, err = convertExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &ReturnStmt{Value: e}, nil
	case st.Break != nil:
		return &BreakStmt{}, nil
	case st.Continue != nil:
		return &ContinueStmt{}, nil
	case st.Fun != nil:
		body, err := convertStmtList(st.Fun.Body)
		if err != nil {
			return nil, err
		}
		var params []string
		for _, p := range st.Fun.Params {
			params = append(params, p.Name)
		}
		return &FuncDecl{Name: st.Fun.Name, Params: params, Body: body}, nil
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.If != nil:
		return convertIfStmt(st.If)
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil {
		return nil, fmt.Errorf("nil expr")
	}
	return convertBinary(e.Binary)
}

func convertBinary(b *parser.BinaryExpr) (Expr, error) {
	if b == nil {
		return nil, fmt.Errorf("nil binary")
	}
	first, err := convertUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands := []Expr{first}
	ops := make([]string, len(b.Right))
	for i, op := range b.Right {
		right, err := convertPostfix(op.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
		ops[i] = op.Op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				var expr Expr
				if ops[i] == "in" {
					expr = &ContainsExpr{Target: right, Elem: left}
				} else {
					expr = &BinaryExpr{Left: left, Op: ops[i], Right: right}
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	return operands[0], nil
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	ex, err := convertPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			ex = &UnaryExpr{Op: op, X: ex}
		default:
			return nil, fmt.Errorf("unary op %s not supported", op)
		}
	}
	return ex, nil
}

func convertPostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := convertPrimary(pf.Target)
	if err != nil {
		return nil, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				return nil, fmt.Errorf("slice not supported")
			}
			if op.Index.Start == nil {
				return nil, fmt.Errorf("nil index")
			}
			idx, err := convertExpr(op.Index.Start)
			if err != nil {
				return nil, err
			}
			expr = &IndexExpr{Target: expr, Index: idx}
		case op.Field != nil:
			// method call if next op is call
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				call := pf.Ops[i+1]
				var args []Expr
				for _, a := range call.Call.Args {
					ex, err := convertExpr(a)
					if err != nil {
						return nil, err
					}
					args = append(args, ex)
				}
				expr = &CallExpr{Func: &SelectorExpr{Receiver: expr, Field: op.Field.Name}, Args: args}
				i++
			} else {
				expr = &SelectorExpr{Receiver: expr, Field: op.Field.Name}
			}
		case op.Call != nil:
			var args []Expr
			for _, a := range op.Call.Args {
				ex, err := convertExpr(a)
				if err != nil {
					return nil, err
				}
				args = append(args, ex)
			}
			expr = &CallExpr{Func: expr, Args: args}
		case op.Cast != nil:
			// ignore casts
		default:
			return nil, fmt.Errorf("postfix op not supported")
		}
	}
	return expr, nil
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			arg, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &LenExpr{X: arg}, nil
		}
		if p.Call.Func == "append" && len(p.Call.Args) == 2 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			val, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			return &AppendExpr{List: list, Value: val}, nil
		}
		if p.Call.Func == "avg" && len(p.Call.Args) == 1 {
			list, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			return &AvgExpr{List: list}, nil
		}
		if p.Call.Func == "substring" && len(p.Call.Args) == 3 {
			s0, err := convertExpr(p.Call.Args[0])
			if err != nil {
				return nil, err
			}
			s1, err := convertExpr(p.Call.Args[1])
			if err != nil {
				return nil, err
			}
			s2, err := convertExpr(p.Call.Args[2])
			if err != nil {
				return nil, err
			}
			return &SubstringExpr{Str: s0, Start: s1, End: s2}, nil
		}
		ce := &CallExpr{Func: &Name{p.Call.Func}}
		for _, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			if p.Call.Func == "print" && len(p.Call.Args) == 1 && isBoolExpr(a) {
				ex = &CondExpr{Cond: ex, Then: &IntLit{Value: 1}, Else: &IntLit{Value: 0}}
			}
			ce.Args = append(ce.Args, ex)
		}
		return ce, nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return &BoolLit{Value: bool(*p.Lit.Bool)}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.List != nil:
		var elems []Expr
		for _, e := range p.List.Elems {
			ex, err := convertExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, ex)
		}
		return &ListLit{Elems: elems}, nil
	case p.Map != nil:
		var entries []MapEntry
		for _, it := range p.Map.Items {
			k, err := convertExpr(it.Key)
			if err != nil {
				return nil, err
			}
			v, err := convertExpr(it.Value)
			if err != nil {
				return nil, err
			}
			entries = append(entries, MapEntry{Key: k, Value: v})
		}
		return &MapLit{Entries: entries}, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.FunExpr != nil && p.FunExpr.ExprBody != nil:
		var params []string
		for _, pa := range p.FunExpr.Params {
			params = append(params, pa.Name)
		}
		body, err := convertExpr(p.FunExpr.ExprBody)
		if err != nil {
			return nil, err
		}
		return &LambdaExpr{Params: params, Body: body}, nil
	case p.Selector != nil:
		expr := Expr(&Name{Name: p.Selector.Root})
		for _, f := range p.Selector.Tail {
			expr = &SelectorExpr{Receiver: expr, Field: f}
		}
		return expr, nil
	case p.Group != nil:
		return convertExpr(p.Group)
	}
	return nil, fmt.Errorf("unsupported expression")
}

func isBoolExpr(e *parser.Expr) bool { return isBoolBinary(e.Binary) }

func isBoolBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return isBoolUnary(b.Left)
	}
	for _, op := range b.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return true
		}
		if isBoolPostfix(op.Right) {
			return true
		}
	}
	return isBoolUnary(b.Left)
}

func isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	for _, op := range u.Ops {
		if op == "!" {
			return true
		}
	}
	return isBoolPostfix(u.Value)
}

func isBoolPostfix(pf *parser.PostfixExpr) bool {
	if pf == nil || len(pf.Ops) > 0 {
		return false
	}
	return isBoolPrimary(pf.Target)
}

func isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Bool != nil:
		return true
	case p.Group != nil:
		return isBoolExpr(p.Group)
	default:
		return false
	}
}

// --- AST -> generic node (for debugging) ---
func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtNode(st))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprNode(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *VarStmt:
		node := &ast.Node{Kind: "var", Value: st.Name}
		if st.Value != nil {
			node.Children = []*ast.Node{exprNode(st.Value)}
		}
		return node
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{exprNode(st.Value)}}
	case *ReturnStmt:
		n := &ast.Node{Kind: "return"}
		if st.Value != nil {
			n.Children = []*ast.Node{exprNode(st.Value)}
		}
		return n
	case *FuncDecl:
		n := &ast.Node{Kind: "func", Value: st.Name}
		for _, p := range st.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		for _, b := range st.Body {
			n.Children = append(n.Children, stmtNode(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, c := range st.Then {
			thenNode.Children = append(thenNode.Children, stmtNode(c))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, c := range st.Else {
				elseNode.Children = append(elseNode.Children, stmtNode(c))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{exprNode(st.Cond)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForRangeStmt:
		n := &ast.Node{Kind: "for-range", Value: st.Name, Children: []*ast.Node{exprNode(st.Start), exprNode(st.End)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	case *ForInStmt:
		n := &ast.Node{Kind: "for-in", Value: st.Name, Children: []*ast.Node{exprNode(st.Iterable)}}
		body := &ast.Node{Kind: "body"}
		for _, b := range st.Body {
			body.Children = append(body.Children, stmtNode(b))
		}
		n.Children = append(n.Children, body)
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(ex.Func))
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case *Name:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *BoolLit:
		if ex.Value {
			return &ast.Node{Kind: "bool", Value: "true"}
		}
		return &ast.Node{Kind: "bool", Value: "false"}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.Left), exprNode(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{exprNode(ex.X)}}
	case *LambdaExpr:
		n := &ast.Node{Kind: "lambda"}
		for _, p := range ex.Params {
			n.Children = append(n.Children, &ast.Node{Kind: "param", Value: p})
		}
		n.Children = append(n.Children, exprNode(ex.Body))
		return n
	case *IndexExpr:
		return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Index)}}
	case *SubstringExpr:
		return &ast.Node{Kind: "substring", Children: []*ast.Node{exprNode(ex.Str), exprNode(ex.Start), exprNode(ex.End)}}
	case *AppendExpr:
		return &ast.Node{Kind: "append", Children: []*ast.Node{exprNode(ex.List), exprNode(ex.Value)}}
	case *ContainsExpr:
		return &ast.Node{Kind: "contains", Children: []*ast.Node{exprNode(ex.Target), exprNode(ex.Elem)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{exprNode(ex.List)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

// Print writes the AST in Lisp-like form to stdout.
func Print(p *Program) { toNode(p).Print("") }
