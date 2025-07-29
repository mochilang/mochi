//go:build slow

package ocaml

import (
	"fmt"

	"mochi/ast"
)

func newNode(kind, value string, children ...*ast.Node) *ast.Node {
	n := &ast.Node{Kind: kind, Value: value}
	if len(children) > 0 {
		n.Children = append(n.Children, children...)
	}
	return n
}

func newProgram(children ...*ast.Node) *ast.Node {
	return newNode("program", "", children...)
}

func newCall(name string, args ...*ast.Node) *ast.Node {
	return newNode("call", name, args...)
}

func newString(s string) *ast.Node {
	return newNode("string", s)
}

func buildProgram(p *Program) *ast.Node {
	stmts := make([]*ast.Node, len(p.Prints))
	for i, pr := range p.Prints {
		stmts[i] = buildPrint(pr)
	}
	return newProgram(stmts...)
}

func buildPrint(pr Print) *ast.Node {
	return newCall("print", newString(pr.Expr))
}

// Transform builds a Mochi AST node from the parsed OCaml Program.
// Only a tiny subset of OCaml is recognized; currently just print statements
// extracted by Parse.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	return buildProgram(p), nil
}
