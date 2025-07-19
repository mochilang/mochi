package kt

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

// Program represents a simple Kotlin program consisting of statements executed in main.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

type Expr interface{ emit(io.Writer) }

// ExprStmt is a statement that evaluates an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

// CallExpr represents a function call.
type CallExpr struct {
	Func string
	Args []Expr
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	io.WriteString(w, "(")
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	io.WriteString(w, ")")
}

// StringLit represents a quoted string literal.
type StringLit struct{ Value string }

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// Transpile converts a Mochi program to a simple Kotlin AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	_ = env
	p := &Program{}
	for _, st := range prog.Statements {
		if st.Expr != nil {
			e, err := convertExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: e})
		} else {
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	return p, nil
}

func convertExpr(e *parser.Expr) (Expr, error) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, fmt.Errorf("unsupported expression")
	}
	return convertUnary(e.Binary.Left)
}

func convertUnary(u *parser.Unary) (Expr, error) {
	if u == nil || len(u.Ops) > 0 {
		return nil, fmt.Errorf("unsupported unary")
	}
	return convertPostfix(u.Value)
}

func convertPostfix(p *parser.PostfixExpr) (Expr, error) {
	if p == nil || len(p.Ops) > 0 {
		return nil, fmt.Errorf("unsupported postfix")
	}
	return convertPrimary(p.Target)
}

func convertPrimary(p *parser.Primary) (Expr, error) {
	switch {
	case p.Call != nil:
		args := make([]Expr, len(p.Call.Args))
		for i, a := range p.Call.Args {
			ex, err := convertExpr(a)
			if err != nil {
				return nil, err
			}
			args[i] = ex
		}
		name := p.Call.Func
		if name == "print" {
			name = "println"
		}
		return &CallExpr{Func: name, Args: args}, nil
	case p.Lit != nil && p.Lit.Str != nil:
		return &StringLit{Value: *p.Lit.Str}, nil
	default:
		return nil, fmt.Errorf("unsupported expression")
	}
}

// Emit returns formatted Kotlin source code for prog.
func Emit(prog *Program) []byte {
	var buf bytes.Buffer
	ver := readVersion()
	ts := time.Now().Format("2006-01-02 15:04:05 MST")
	fmt.Fprintf(&buf, "// Mochi %s - generated %s\n", ver, ts)
	buf.WriteString("fun main() {\n")
	for _, s := range prog.Stmts {
		buf.WriteString("    ")
		s.emit(&buf)
		buf.WriteByte('\n')
	}
	buf.WriteString("}\n")
	return buf.Bytes()
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

// Print writes a Lisp-like representation of the Kotlin AST to stdout.
func Print(p *Program) {
	toNodeProg(p).Print("")
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
