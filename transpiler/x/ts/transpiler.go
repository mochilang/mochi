package tstranspiler

import (
	"bytes"
	"fmt"
	"io"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Simple TypeScript AST nodes used by the transpiler.

type Program struct {
	Stmts []Stmt
}

type Stmt interface {
	emit(io.Writer)
}

type Expr interface {
	emit(io.Writer)
}

type ExprStmt struct {
	Expr Expr
}

type CallExpr struct {
	Func string
	Args []Expr
}

type StringLit struct {
	Value string
}

func (s *ExprStmt) emit(w io.Writer) {
	if s == nil {
		return
	}
	s.Expr.emit(w)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(';')
	} else {
		io.WriteString(w, ";")
	}
}

func (c *CallExpr) emit(w io.Writer) {
	io.WriteString(w, c.Func)
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte('(')
	} else {
		io.WriteString(w, "(")
	}
	for i, a := range c.Args {
		if i > 0 {
			io.WriteString(w, ", ")
		}
		a.emit(w)
	}
	if b, ok := w.(interface{ WriteByte(byte) error }); ok {
		b.WriteByte(')')
	} else {
		io.WriteString(w, ")")
	}
}

func (s *StringLit) emit(w io.Writer) { fmt.Fprintf(w, "%q", s.Value) }

// Emit converts the AST back into TypeScript source code.
func Emit(p *Program) []byte {
	var b bytes.Buffer
	for i, s := range p.Stmts {
		if i > 0 {
			b.WriteByte('\n')
		}
		s.emit(&b)
		b.WriteByte('\n')
	}
	return b.Bytes()
}

// Transpile converts a Mochi program into a TypeScript AST. Only a very
// small subset of the language is supported: programs consisting of a single
// call to the builtin `print` with a string literal argument.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	_ = env // env not used by this minimal implementation
	tsProg := &Program{}

	for _, st := range prog.Statements {
		if st.Expr == nil {
			return nil, fmt.Errorf("unsupported statement")
		}
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call == nil || call.Func != "print" {
			return nil, fmt.Errorf("unsupported expression")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print expects one argument")
		}
		arg := call.Args[0]
		lit := arg.Binary.Left.Value.Target.Lit
		if lit == nil || lit.Str == nil {
			return nil, fmt.Errorf("unsupported argument")
		}
		tsProg.Stmts = append(tsProg.Stmts, &ExprStmt{
			Expr: &CallExpr{
				Func: "console.log",
				Args: []Expr{&StringLit{Value: *lit.Str}},
			},
		})
	}
	return tsProg, nil
}

// print converts the given TypeScript AST into a generic ast.Node tree and
// writes it to w. It is useful for debugging and tests.
func print(p *Program, w io.Writer) {
	if p == nil {
		return
	}
	node := progToNode(p)
	io.WriteString(w, node.String())
}

func progToNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, st := range p.Stmts {
		n.Children = append(n.Children, stmtToNode(st))
	}
	return n
}

func stmtToNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "expr", Children: []*ast.Node{exprToNode(st.Expr)}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}

func exprToNode(e Expr) *ast.Node {
	switch ex := e.(type) {
	case *CallExpr:
		n := &ast.Node{Kind: "call", Value: ex.Func}
		for _, a := range ex.Args {
			n.Children = append(n.Children, exprToNode(a))
		}
		return n
	case *StringLit:
		return &ast.Node{Kind: "string", Value: ex.Value}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
