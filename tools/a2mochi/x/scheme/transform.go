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
			switch it.Name {
			case "to-str", "upper", "lower", "fmod":
				continue
			}
			prog.Children = append(prog.Children, newFunc(it.Name, it.Params))
		case "var":
			prog.Children = append(prog.Children, newLet(it.Name, it.Value))
		case "import":
			// ignore imports
		case "assign":
			prog.Children = append(prog.Children, newAssign(it.Name, it.Value))
		case "print":
			prog.Children = append(prog.Children, newPrint(it.Value))
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

func newLet(name string, val interface{}) *ast.Node {
	n := &ast.Node{Kind: "let", Value: sanitizeName(name)}
	if val != nil {
		if c := exprNode(val); c != nil {
			n.Children = append(n.Children, c)
		}
	}
	return n
}

func newAssign(name string, val interface{}) *ast.Node {
	n := &ast.Node{Kind: "assign", Value: sanitizeName(name)}
	if c := exprNode(val); c != nil {
		n.Children = append(n.Children, c)
	} else {
		n.Children = append(n.Children, &ast.Node{Kind: "int", Value: 0})
	}
	return n
}

func newPrint(val interface{}) *ast.Node {
	n := &ast.Node{Kind: "print"}
	if c := exprNode(val); c != nil {
		n.Children = append(n.Children, c)
	}
	return n
}

func constNode(v interface{}) *ast.Node {
	switch val := v.(type) {
	case float64:
		if val == float64(int(val)) {
			return &ast.Node{Kind: "int", Value: int(val)}
		}
		return &ast.Node{Kind: "float", Value: val}
	case string:
		return &ast.Node{Kind: "string", Value: val}
	case bool:
		return &ast.Node{Kind: "bool", Value: val}
	default:
		return nil
	}
}

func exprNode(v interface{}) *ast.Node {
	if n := constNode(v); n != nil {
		return n
	}
	m, ok := v.(map[string]interface{})
	if !ok {
		return nil
	}
	if name, ok := m["var"].(string); ok {
		return &ast.Node{Kind: sanitizeName(name)}
	}
	if list, ok := m["list"].([]interface{}); ok {
		n := &ast.Node{Kind: "list"}
		for _, x := range list {
			if c := exprNode(x); c != nil {
				n.Children = append(n.Children, c)
			}
		}
		return n
	}
	if call, ok := m["call"].(string); ok {
		argsVal, _ := m["args"].([]interface{})
		switch call {
		case "+", "-", "*", "/", "<", ">", "<=", ">=", "=", "equal?":
			if call == "=" || call == "equal?" {
				call = "=="
			}
			n := &ast.Node{Kind: "binary", Value: call}
			for _, a := range argsVal {
				if c := exprNode(a); c != nil {
					n.Children = append(n.Children, c)
				}
			}
			return n
		case "list":
			n := &ast.Node{Kind: "list"}
			for _, a := range argsVal {
				if c := exprNode(a); c != nil {
					n.Children = append(n.Children, c)
				}
			}
			return n
		default:
			n := &ast.Node{Kind: "call", Value: sanitizeName(call)}
			for _, a := range argsVal {
				if c := exprNode(a); c != nil {
					n.Children = append(n.Children, c)
				}
			}
			return n
		}
	}
	return nil
}

func sanitizeName(s string) string {
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	return s
}
