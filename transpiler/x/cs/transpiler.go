//go:build slow

package cstranspiler

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// --- C# AST ---

type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// LetStmt represents a variable declaration.
type LetStmt struct {
	Name  string
	Value Expr
}

func (s *LetStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s = ", s.Name)
	s.Value.emit(w)
}

// VarStmt represents a mutable variable declaration.
type VarStmt struct {
	Name  string
	Value Expr // optional
}

func (s *VarStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "var %s", s.Name)
	if s.Value != nil {
		fmt.Fprint(w, " = ")
		s.Value.emit(w)
	}
}

// AssignStmt represents simple assignment to a variable.
type AssignStmt struct {
	Name  string
	Value Expr
}

func (s *AssignStmt) emit(w io.Writer) {
	fmt.Fprintf(w, "%s = ", s.Name)
	s.Value.emit(w)
}

// WhileStmt represents a while loop.
type WhileStmt struct {
	Cond Expr
	Body []Stmt
}

func (ws *WhileStmt) emit(w io.Writer) {
	fmt.Fprint(w, "while (")
	if ws.Cond != nil {
		ws.Cond.emit(w)
		fmt.Fprint(w, " != 0")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range ws.Body {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
}

// IfStmt represents a conditional statement.
type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
}

func (i *IfStmt) emit(w io.Writer) {
	fmt.Fprint(w, "if (")
	if i.Cond != nil {
		i.Cond.emit(w)
		fmt.Fprint(w, " != 0")
	}
	fmt.Fprint(w, ") {\n")
	for _, st := range i.Then {
		fmt.Fprint(w, "    ")
		st.emit(w)
		fmt.Fprint(w, ";\n")
	}
	fmt.Fprint(w, "}")
	if len(i.Else) > 0 {
		fmt.Fprint(w, " else {\n")
		for _, st := range i.Else {
			fmt.Fprint(w, "    ")
			st.emit(w)
			fmt.Fprint(w, ";\n")
		}
		fmt.Fprint(w, "}")
	}
}

// IfExpr is a ternary conditional expression.
type IfExpr struct {
	Cond Expr
	Then Expr
	Else Expr
}

func (ie *IfExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	ie.Cond.emit(w)
	fmt.Fprint(w, " != 0 ? ")
	ie.Then.emit(w)
	fmt.Fprint(w, " : ")
	ie.Else.emit(w)
	fmt.Fprint(w, ")")
}

type Expr interface{ emit(io.Writer) }

// VarRef references a variable by name.
type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

// BinaryExpr represents a binary operation like addition or comparison.
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// UnaryExpr represents a unary prefix operation.
type UnaryExpr struct {
	Op  string
	Val Expr
}

func (u *UnaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, u.Op)
	u.Val.emit(w)
}

// CmpExpr emits comparison result as 1 or 0.
type CmpExpr struct {
	Op    string
	Left  Expr
	Right Expr
}

func (c *CmpExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	c.Left.emit(w)
	fmt.Fprintf(w, " %s ", c.Op)
	c.Right.emit(w)
	fmt.Fprint(w, " ? 1 : 0)")
}

// ExprStmt represents a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	fmt.Fprint(w, c.Func)
	fmt.Fprint(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		a.emit(w)
	}
	fmt.Fprint(w, ")")
}

type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

type IntLit struct{ Value int }

func (i *IntLit) emit(w io.Writer) { fmt.Fprintf(w, "%d", i.Value) }

type ListLit struct{ Elems []Expr }

func (l *ListLit) emit(w io.Writer) {
	fmt.Fprint(w, "new[]{")
	for i, e := range l.Elems {
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		e.emit(w)
	}
	fmt.Fprint(w, "}")
}

type CountExpr struct{ Arg Expr }

func (c *CountExpr) emit(w io.Writer) {
	c.Arg.emit(w)
	fmt.Fprint(w, ".Length")
}

type AvgExpr struct{ Arg Expr }

func (a *AvgExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	a.Arg.emit(w)
	fmt.Fprint(w, ".Average()).ToString(\"0.0\")")
}

type LenExpr struct{ Arg Expr }

func (l *LenExpr) emit(w io.Writer) {
	l.Arg.emit(w)
	fmt.Fprint(w, ".Length")
}

type SumExpr struct{ Arg Expr }

func (s *SumExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	s.Arg.emit(w)
	fmt.Fprint(w, ".Sum())")
}

type StrExpr struct{ Arg Expr }

func (s *StrExpr) emit(w io.Writer) {
	fmt.Fprint(w, "Convert.ToString(")
	s.Arg.emit(w)
	fmt.Fprint(w, ")")
}

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	for _, st := range p.Statements {
		s, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			prog.Stmts = append(prog.Stmts, s)
		}
	}
	_ = env // env reserved for future use
	return prog, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	expr := left
	for _, op := range e.Binary.Right {
		r, err := compilePostfix(op.Right)
		if err != nil {
			return nil, err
		}
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=":
			expr = &CmpExpr{Op: op.Op, Left: expr, Right: r}
		default:
			expr = &BinaryExpr{Op: op.Op, Left: expr, Right: r}
		}
	}
	return expr, nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = &UnaryExpr{Op: u.Ops[i], Val: expr}
	}
	return expr, nil
}

func compilePostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil || len(p.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return compilePrimary(p.Target)
}

func compileStmt(s *parser.Statement) (Stmt, error) {
	switch {
	case s.Expr != nil:
		e, err := compileExpr(s.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return &ExprStmt{Expr: e}, nil
	case s.Let != nil:
		val, err := compileExpr(s.Let.Value)
		if err != nil {
			return nil, err
		}
		return &LetStmt{Name: s.Let.Name, Value: val}, nil
	case s.Var != nil:
		var val Expr
		var err error
		if s.Var.Value != nil {
			val, err = compileExpr(s.Var.Value)
			if err != nil {
				return nil, err
			}
		}
		return &VarStmt{Name: s.Var.Name, Value: val}, nil
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			val, err := compileExpr(s.Assign.Value)
			if err != nil {
				return nil, err
			}
			return &AssignStmt{Name: s.Assign.Name, Value: val}, nil
		}
	case s.While != nil:
		cond, err := compileExpr(s.While.Cond)
		if err != nil {
			return nil, err
		}
		var body []Stmt
		for _, b := range s.While.Body {
			st, err := compileStmt(b)
			if err != nil {
				return nil, err
			}
			if st != nil {
				body = append(body, st)
			}
		}
		return &WhileStmt{Cond: cond, Body: body}, nil
	case s.If != nil:
		return compileIfStmt(s.If)
	case s.Test == nil && s.Import == nil && s.Type == nil:
		return nil, fmt.Errorf("unsupported statement at %d:%d", s.Pos.Line, s.Pos.Column)
	}
	return nil, nil
}

func compileIfStmt(i *parser.IfStmt) (Stmt, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	var thenStmts []Stmt
	for _, st := range i.Then {
		s, err := compileStmt(st)
		if err != nil {
			return nil, err
		}
		if s != nil {
			thenStmts = append(thenStmts, s)
		}
	}
	var elseStmts []Stmt
	if i.ElseIf != nil {
		s, err := compileIfStmt(i.ElseIf)
		if err != nil {
			return nil, err
		}
		elseStmts = []Stmt{s}
	} else if len(i.Else) > 0 {
		for _, st := range i.Else {
			s, err := compileStmt(st)
			if err != nil {
				return nil, err
			}
			if s != nil {
				elseStmts = append(elseStmts, s)
			}
		}
	}
	return &IfStmt{Cond: cond, Then: thenStmts, Else: elseStmts}, nil
}

func compilePrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := compileExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		switch name {
		case "print":
			name = "Console.WriteLine"
			return &CallExpr{Func: name, Args: args}, nil
		case "count":
			if len(args) == 1 {
				return &CountExpr{Arg: args[0]}, nil
			}
		case "avg":
			if len(args) == 1 {
				return &AvgExpr{Arg: args[0]}, nil
			}
		case "len":
			if len(args) == 1 {
				return &LenExpr{Arg: args[0]}, nil
			}
		case "sum":
			if len(args) == 1 {
				return &SumExpr{Arg: args[0]}, nil
			}
		case "str":
			if len(args) == 1 {
				return &StrExpr{Arg: args[0]}, nil
			}
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	case p.Lit != nil && p.Lit.Int != nil:
		return &IntLit{Value: int(*p.Lit.Int)}, nil
	case p.List != nil:
		elems := make([]Expr, len(p.List.Elems))
		for i, e := range p.List.Elems {
			ex, err := compileExpr(e)
			if err != nil {
				return nil, err
			}
			elems[i] = ex
		}
		return &ListLit{Elems: elems}, nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	case p.Group != nil:
		return compileExpr(p.Group)
	case p.If != nil:
		return compileIfExpr(p.If)
	}
	return nil, fmt.Errorf("unsupported primary")
}

func compileIfExpr(i *parser.IfExpr) (Expr, error) {
	cond, err := compileExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenExpr, err := compileExpr(i.Then)
	if err != nil {
		return nil, err
	}
	var elseExpr Expr
	if i.ElseIf != nil {
		elseExpr, err = compileIfExpr(i.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if i.Else != nil {
		elseExpr, err = compileExpr(i.Else)
		if err != nil {
			return nil, err
		}
	}
	return &IfExpr{Cond: cond, Then: thenExpr, Else: elseExpr}, nil
}

// Emit generates formatted C# source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("using System;\n")
	if needsLinq(prog) {
		buf.WriteString("using System.Linq;\n")
	}
	buf.WriteString("\n")
	buf.WriteString("class Program {\n")
	buf.WriteString("\tstatic void Main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("\t\t")
		s.emit(&buf)
		buf.WriteString(";\n")
	}
	buf.WriteString("\t}\n")
	buf.WriteString("}\n")
	return formatCS(buf.Bytes())
}

// formatCS performs very basic formatting and prepends a standard header.
func formatCS(src []byte) []byte {
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	header := fmt.Sprintf("// Mochi %s - generated %s\n", ver, ts)
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return append([]byte(header), src...)
}

func readVersion() string {
	_, file, _, _ := runtime.Caller(0)
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			v, err := os.ReadFile(filepath.Join(dir, "VERSION"))
			if err != nil {
				return "unknown"
			}
			return strings.TrimSpace(string(v))
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "unknown"
}

// print converts the custom AST to an ast.Node and prints it.
func print(prog *Program) {
	node := toNodeProg(prog)
	fmt.Print(node.String())
}

func toNodeProg(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, toNodeStmt(s))
	}
	return n
}

func toNodeStmt(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{toNodeExpr(st.Expr)}}
	case *LetStmt:
		return &ast.Node{Kind: "let", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *VarStmt:
		var child []*ast.Node
		if st.Value != nil {
			child = []*ast.Node{toNodeExpr(st.Value)}
		}
		return &ast.Node{Kind: "var", Value: st.Name, Children: child}
	case *AssignStmt:
		return &ast.Node{Kind: "assign", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
	case *WhileStmt:
		n := &ast.Node{Kind: "while", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		for _, b := range st.Body {
			n.Children = append(n.Children, toNodeStmt(b))
		}
		return n
	case *IfStmt:
		n := &ast.Node{Kind: "if", Children: []*ast.Node{toNodeExpr(st.Cond)}}
		thenNode := &ast.Node{Kind: "then"}
		for _, b := range st.Then {
			thenNode.Children = append(thenNode.Children, toNodeStmt(b))
		}
		n.Children = append(n.Children, thenNode)
		if len(st.Else) > 0 {
			elseNode := &ast.Node{Kind: "else"}
			for _, b := range st.Else {
				elseNode.Children = append(elseNode.Children, toNodeStmt(b))
			}
			n.Children = append(n.Children, elseNode)
		}
		return n
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func toNodeExpr(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, toNodeExpr(a))
		}
		return n
	case *VarRef:
		return &ast.Node{Kind: "var", Value: ex.Name}
	case *CountExpr:
		return &ast.Node{Kind: "count", Children: []*ast.Node{toNodeExpr(ex.Arg)}}
	case *AvgExpr:
		return &ast.Node{Kind: "avg", Children: []*ast.Node{toNodeExpr(ex.Arg)}}
	case *LenExpr:
		return &ast.Node{Kind: "len", Children: []*ast.Node{toNodeExpr(ex.Arg)}}
	case *SumExpr:
		return &ast.Node{Kind: "sum", Children: []*ast.Node{toNodeExpr(ex.Arg)}}
	case *StrExpr:
		return &ast.Node{Kind: "str", Children: []*ast.Node{toNodeExpr(ex.Arg)}}
	case *CmpExpr:
		return &ast.Node{Kind: "cmp", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *UnaryExpr:
		return &ast.Node{Kind: "unary", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Val)}}
	case *BinaryExpr:
		return &ast.Node{Kind: "binary", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	case *IfExpr:
		return &ast.Node{Kind: "ifexpr", Children: []*ast.Node{toNodeExpr(ex.Cond), toNodeExpr(ex.Then), toNodeExpr(ex.Else)}}
	case *IntLit:
		return &ast.Node{Kind: "int", Value: fmt.Sprint(ex.Value)}
	case *ListLit:
		n := &ast.Node{Kind: "list"}
		for _, el := range ex.Elems {
			n.Children = append(n.Children, toNodeExpr(el))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func needsLinq(p *Program) bool {
	var found bool
	for _, s := range p.Stmts {
		found = found || inspectLinq(s)
	}
	return found
}

func inspectLinq(e Expr) bool {
	switch ex := e.(type) {
	case *ExprStmt:
		return inspectLinq(ex.Expr)
	case *AvgExpr:
		return true
	case *SumExpr:
		return true
	case *IfExpr:
		return inspectLinq(ex.Cond) || inspectLinq(ex.Then) || inspectLinq(ex.Else)
	case *IfStmt:
		if inspectLinq(ex.Cond) {
			return true
		}
		for _, s := range ex.Then {
			if inspectLinq(s) {
				return true
			}
		}
		for _, s := range ex.Else {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *WhileStmt:
		if inspectLinq(ex.Cond) {
			return true
		}
		for _, s := range ex.Body {
			if inspectLinq(s) {
				return true
			}
		}
		return false
	case *CallExpr:
		for _, a := range ex.Args {
			if inspectLinq(a) {
				return true
			}
		}
	case *ListLit:
		for _, el := range ex.Elems {
			if inspectLinq(el) {
				return true
			}
		}
	case *CountExpr:
		return inspectLinq(ex.Arg)
	case *LenExpr:
		return inspectLinq(ex.Arg)
	case *UnaryExpr:
		return inspectLinq(ex.Val)
	case *BinaryExpr:
		return inspectLinq(ex.Left) || inspectLinq(ex.Right)
	}
	return false
}
