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
	for _, stmt := range p.Statements {
		switch {
		case stmt.Expr != nil:
			e, err := compileExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			prog.Stmts = append(prog.Stmts, &ExprStmt{Expr: e})
		case stmt.Let != nil:
			val, err := compileExpr(stmt.Let.Value)
			if err != nil {
				return nil, err
			}
			prog.Stmts = append(prog.Stmts, &LetStmt{Name: stmt.Let.Name, Value: val})
		case stmt.Test == nil && stmt.Import == nil && stmt.Type == nil:
			return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
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
	}
	return nil, fmt.Errorf("unsupported primary")
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
