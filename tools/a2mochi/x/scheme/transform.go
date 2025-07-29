//go:build slow

package scheme

import (
	"fmt"
	"strings"

	"mochi/ast"
)

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	prog := newProgram()
	for _, it := range p.Items {
		switch it.Kind {
		case "func":
			prog.Children = append(prog.Children, newFunc(it.Name, it.Params))
		case "var":
			prog.Children = append(prog.Children, newLet(it.Name))
		case "import":
			prog.Children = append(prog.Children, newUse(it.Name))
		}
	}

	return prog, nil
}

func newProgram(children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: "program", Children: children}
}

func newFunc(name string, params []string) *ast.Node {
	fn := &ast.Node{Kind: "fun", Value: sanitizeName(name)}
	for _, p := range params {
		fn.Children = append(fn.Children, newParam(p))
	}
	fn.Children = append(fn.Children, &ast.Node{Kind: "block"})
	return fn
}

func newParam(name string) *ast.Node {
	return &ast.Node{Kind: "param", Value: sanitizeName(name), Children: []*ast.Node{{Kind: "type", Value: "any"}}}
}

func newLet(name string) *ast.Node {
	return &ast.Node{Kind: "let", Value: sanitizeName(name)}
}

func newUse(name string) *ast.Node {
	return &ast.Node{Kind: "selector", Value: sanitizeName(name), Children: []*ast.Node{{Kind: "selector", Value: "use"}}}
}

func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	return s
}
