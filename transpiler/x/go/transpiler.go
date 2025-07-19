//go:build slow

package gotranspiler

import (
	"bytes"
	"fmt"
	"go/format"
	"io"

	"mochi/ast"
	"mochi/parser"
	meta "mochi/transpiler/meta"
	"mochi/types"
)

// Program represents a Go program consisting of a sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type VarDecl struct {
	Name  string
	Value Expr
}

func (v *VarDecl) emit(w io.Writer) {
	fmt.Fprintf(w, "%s := ", v.Name)
	v.Value.emit(w)
}

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

type VarRef struct{ Name string }

func (v *VarRef) emit(w io.Writer) { fmt.Fprint(w, v.Name) }

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (b *BinaryExpr) emit(w io.Writer) {
	fmt.Fprint(w, "(")
	b.Left.emit(w)
	fmt.Fprintf(w, " %s ", b.Op)
	b.Right.emit(w)
	fmt.Fprint(w, ")")
}

// Transpile converts a Mochi program to a minimal Go AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	gp := &Program{}
	for _, stmt := range p.Statements {
		switch {
		case stmt.Expr != nil:
			e, err := compileExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			gp.Stmts = append(gp.Stmts, &ExprStmt{Expr: e})
		case stmt.Let != nil && stmt.Let.Value != nil:
			e, err := compileExpr(stmt.Let.Value)
			if err != nil {
				return nil, err
			}
			gp.Stmts = append(gp.Stmts, &VarDecl{Name: stmt.Let.Name, Value: e})
		default:
			if stmt.Test == nil && stmt.Import == nil && stmt.Type == nil {
				return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
			}
		}
	}
	_ = env // reserved for future use
	return gp, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}
	left, err := compileUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	if len(e.Binary.Right) == 0 {
		return left, nil
	}
	if len(e.Binary.Right) > 1 {
		return nil, fmt.Errorf("unsupported expression chain")
	}
	r, err := compilePostfix(e.Binary.Right[0].Right)
	if err != nil {
		return nil, err
	}
	return &BinaryExpr{Left: left, Op: e.Binary.Right[0].Op, Right: r}, nil
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	expr, err := compilePostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = &BinaryExpr{Left: &IntLit{Value: 0}, Op: "-", Right: expr}
		} else {
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func compilePostfix(pf *parser.PostfixExpr) (Expr, error) {
	if pf == nil || len(pf.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return compilePrimary(pf.Target)
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
		if name == "print" {
			name = "fmt.Println"
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return &StringLit{Value: *p.Lit.Str}, nil
		}
		if p.Lit.Int != nil {
			return &IntLit{Value: int(*p.Lit.Int)}, nil
		}
		return nil, fmt.Errorf("unsupported literal")
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		return &VarRef{Name: p.Selector.Root}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

// Emit formats the Go AST back into source code.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.Write(meta.Header("//"))
	buf.WriteString("package main\n\n")
	buf.WriteString("import \"fmt\"\n\n")
	buf.WriteString("func main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteString("\n")
	}
	buf.WriteString("}\n")
	out, err := format.Source(buf.Bytes())
	if err == nil {
		return out
	}
	return buf.Bytes()
}

// print converts prog to an ast.Node and prints it.
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
	case *VarDecl:
		return &ast.Node{Kind: "var", Value: st.Name, Children: []*ast.Node{toNodeExpr(st.Value)}}
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
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	case *IntLit:
		return &ast.Node{Kind: "int"}
	case *VarRef:
		return &ast.Node{Kind: "name", Value: ex.Name}
	case *BinaryExpr:
		return &ast.Node{Kind: "bin", Value: ex.Op, Children: []*ast.Node{toNodeExpr(ex.Left), toNodeExpr(ex.Right)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
