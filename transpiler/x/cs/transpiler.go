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

type Expr interface{ emit(io.Writer) }

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

// Transpile converts a Mochi AST to a simple C# AST.
func Transpile(p *parser.Program, env *types.Env) (*Program, error) {
	prog := &Program{}
	for _, stmt := range p.Statements {
		if stmt.Expr != nil {
			e, err := compileExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			prog.Stmts = append(prog.Stmts, &ExprStmt{Expr: e})
		} else if stmt.Test == nil && stmt.Import == nil && stmt.Type == nil {
			return nil, fmt.Errorf("unsupported statement at %d:%d", stmt.Pos.Line, stmt.Pos.Column)
		}
	}
	_ = env // env reserved for future use
	return prog, nil
}

func compileExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, fmt.Errorf("unsupported expression")
	}
	return compileUnary(e.Binary.Left)
}

func compileUnary(u *parser.Unary) (Expr, error) {
	if u == nil || len(u.Ops) > 0 {
		return nil, fmt.Errorf("unsupported unary")
	}
	return compilePostfix(u.Value)
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
		if name == "print" {
			name = "Console.WriteLine"
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	}
	return nil, fmt.Errorf("unsupported primary")
}

// Emit generates formatted C# source from the AST.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	buf.WriteString("using System;\n\n")
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
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
