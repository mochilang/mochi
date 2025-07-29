package scala

import (
	"fmt"

	"mochi/ast"
)

func program(children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: "program", Children: children}
}

func let(name string) *ast.Node { return &ast.Node{Kind: "let", Value: name} }

func param(name string) *ast.Node { return &ast.Node{Kind: "param", Value: name} }

func fun(name string, params ...string) *ast.Node {
	n := &ast.Node{Kind: "fun", Value: name}
	for _, p := range params {
		n.Children = append(n.Children, param(p))
	}
	return n
}

// Transform converts a parsed Scala Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := program()
	for _, d := range p.Decls {
		switch d.Kind {
		case "val":
			root.Children = append(root.Children, let(d.Name))
		case "def":
			var params []string
			for _, prm := range d.Params {
				params = append(params, prm.Name)
			}
			root.Children = append(root.Children, fun(d.Name, params...))
		}
	}
	return root, nil
}
