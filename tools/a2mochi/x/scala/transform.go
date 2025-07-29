package scala

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
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
	var b strings.Builder
	for _, d := range p.Decls {
		switch d.Kind {
		case "val":
			if d.RHS != "" {
				fmt.Fprintf(&b, "let %s = %s\n", d.Name, d.RHS)
			} else {
				fmt.Fprintf(&b, "let %s\n", d.Name)
			}
		case "var":
			if d.RHS != "" {
				fmt.Fprintf(&b, "var %s = %s\n", d.Name, d.RHS)
			} else {
				fmt.Fprintf(&b, "var %s\n", d.Name)
			}
		case "def":
			fmt.Fprintf(&b, "fun %s(", d.Name)
			for i, prm := range d.Params {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(prm.Name)
			}
			b.WriteString(") {\n")
			if d.Body != "" {
				for _, line := range strings.Split(d.Body, "\n") {
					line = strings.TrimSpace(line)
					if line == "" {
						continue
					}
					b.WriteString("  " + line + "\n")
				}
			}
			b.WriteString("}\n")
		}
	}
	prog, err := parser.ParseString(b.String())
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}
