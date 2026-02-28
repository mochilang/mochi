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
			prog.Children = append(prog.Children, newFunc(it.Name, it.Params, it.Body))
		case "var":
			prog.Children = append(prog.Children, newLet(it.Name, it.Value))
		case "import":
			// ignore imports
		case "assign":
			prog.Children = append(prog.Children, newAssign(it.Name, it.Value))
		case "print":
			nodes := printNodes(it.Value)
			prog.Children = append(prog.Children, nodes...)
		}
	}

	return prog, nil
}

func newProgram(children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: "program", Children: children}
}

func newFunc(name string, params []string, body []interface{}) *ast.Node {
	fn := &ast.Node{Kind: "fun", Value: sanitizeName(name)}
	for _, p := range params {
		fn.Children = append(fn.Children, newParam(p))
	}
	blk := &ast.Node{Kind: "block"}
	if len(body) == 1 {
		if n := exprNode(body[0]); n != nil {
			blk.Children = append(blk.Children, &ast.Node{Kind: "return", Children: []*ast.Node{n}})
		}
	}
	fn.Children = append(fn.Children, blk)
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
	n := &ast.Node{Kind: "call", Value: "print"}
	if c := exprNode(val); c != nil {
		n.Children = append(n.Children, c)
	}
	return n
}

type binding struct {
	name  string
	value interface{}
}

func parseLet(v map[string]interface{}) ([]binding, []interface{}, bool) {
	binds, ok := v["let"].([]interface{})
	if !ok {
		return nil, nil, false
	}
	var res []binding
	for _, b := range binds {
		m, ok := b.(map[string]interface{})
		if !ok {
			continue
		}
		name, _ := m["name"].(string)
		val := m["value"]
		res = append(res, binding{name: name, value: val})
	}
	body, _ := v["body"].([]interface{})
	return res, body, true
}

func printNodes(val interface{}) []*ast.Node {
	if m, ok := val.(map[string]interface{}); ok {
		if binds, body, ok := parseLet(m); ok {
			var nodes []*ast.Node
			for _, b := range binds {
				nodes = append(nodes, newLet(b.name, b.value))
			}
			if len(body) > 0 {
				nodes = append(nodes, newPrint(body[len(body)-1]))
			} else {
				nodes = append(nodes, newPrint(nil))
			}
			return nodes
		}
	}
	return []*ast.Node{newPrint(val)}
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
	if _, ok := m["let"]; ok {
		_, body, ok2 := parseLet(m)
		if ok2 && len(body) > 0 {
			return exprNode(body[len(body)-1])
		}
		return nil
	}
	if seq, ok := m["begin"].([]interface{}); ok {
		if len(seq) > 0 {
			return exprNode(seq[len(seq)-1])
		}
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
		case "if":
			if len(argsVal) >= 2 {
				n := &ast.Node{Kind: "if_expr"}
				n.Children = append(n.Children, exprNode(argsVal[0]))
				n.Children = append(n.Children, exprNode(argsVal[1]))
				if len(argsVal) > 2 {
					n.Children = append(n.Children, exprNode(argsVal[2]))
				}
				return n
			}
			return nil
		case "to-str":
			if len(argsVal) == 1 {
				return exprNode(argsVal[0])
			}
			return nil
		case "+", "-", "*", "/", "<", ">", "<=", ">=", "=", "equal?":
			if call == "-" && len(argsVal) == 1 {
				return &ast.Node{Kind: "unary", Value: "-", Children: []*ast.Node{exprNode(argsVal[0])}}
			}
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
		case "and", "or":
			if len(argsVal) >= 2 {
				op := "&&"
				if call == "or" {
					op = "||"
				}
				left := exprNode(argsVal[0])
				for _, a := range argsVal[1:] {
					right := exprNode(a)
					left = &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{left, right}}
				}
				return left
			}
			return nil
		case "length", "string-length", "hash-table-size":
			n := &ast.Node{Kind: "call", Value: "len"}
			for _, a := range argsVal {
				if c := exprNode(a); c != nil {
					n.Children = append(n.Children, c)
				}
			}
			return n
		case "string->number":
			if len(argsVal) == 1 {
				return &ast.Node{Kind: "call", Value: "int", Children: []*ast.Node{exprNode(argsVal[0])}}
			}
			return nil
		case "exact->inexact", "inexact->exact":
			if len(argsVal) == 1 {
				return exprNode(argsVal[0])
			}
			return nil
		case "append":
			n := &ast.Node{Kind: "call", Value: "append"}
			if len(argsVal) > 0 {
				if c := exprNode(argsVal[0]); c != nil {
					n.Children = append(n.Children, c)
				}
				argsVal = argsVal[1:]
			}
			for _, a := range argsVal {
				if m2, ok2 := a.(map[string]interface{}); ok2 {
					if lst, ok3 := m2["list"].([]interface{}); ok3 {
						for _, elem := range lst {
							if c := exprNode(elem); c != nil {
								n.Children = append(n.Children, c)
							}
						}
						continue
					}
				}
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
		case "apply":
			if len(argsVal) >= 2 {
				if m0, ok := argsVal[0].(map[string]interface{}); ok {
					if fname, ok2 := m0["var"].(string); ok2 {
						switch fname {
						case "+":
							return &ast.Node{Kind: "call", Value: "sum", Children: []*ast.Node{exprNode(argsVal[1])}}
						case "min", "max":
							return &ast.Node{Kind: "call", Value: fname, Children: []*ast.Node{exprNode(argsVal[1])}}
						}
					}
				}
			}
			n := &ast.Node{Kind: "call", Value: "apply"}
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
