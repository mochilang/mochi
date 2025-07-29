//go:build slow

package rkt

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/ast"
)

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	mutated := map[string]bool{}
	for _, f := range p.Forms {
		if list, ok := f.Datum.([]any); ok && len(list) >= 2 {
			if head, _ := list[0].(string); head == "set!" {
				if name, ok := list[1].(string); ok {
					mutated[name] = true
				}
			}
		}
	}

	root := &ast.Node{Kind: "program"}
	for _, f := range p.Forms {
		if n := formNodeWithMut(f.Datum, mutated); n != nil {
			root.Children = append(root.Children, n)
		}
	}
	return root, nil
}

func formNodeWithMut(d any, mutated map[string]bool) *ast.Node {
	list, ok := d.([]any)
	if !ok || len(list) == 0 {
		return nil
	}
	head, _ := list[0].(string)
	switch head {
	case "define":
		if fn, ok := list[1].([]any); ok {
			name, _ := fn[0].(string)
			n := &ast.Node{Kind: "fun", Value: name}
			for _, p := range fn[1:] {
				if s, ok := p.(string); ok {
					n.Children = append(n.Children, &ast.Node{
						Kind:     "param",
						Value:    s,
						Children: []*ast.Node{{Kind: "type", Value: "int"}},
					})
				}
			}
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: "int"})
			for _, body := range list[2:] {
				n.Children = append(n.Children, &ast.Node{
					Kind:     "return",
					Children: []*ast.Node{exprNode(body)},
				})
			}
			return n
		}
		if name, ok := list[1].(string); ok && len(list) >= 3 {
			kind := "let"
			if mutated[name] {
				kind = "var"
			}
			return &ast.Node{Kind: kind, Value: name, Children: []*ast.Node{exprNode(list[2])}}
		}
	case "set!":
		if len(list) == 3 {
			if call, ok := list[2].([]any); ok && len(call) >= 4 {
				ch, _ := call[0].(string)
				if ch == "list-set" || ch == "hash-set" {
					idx := &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(call[1]), exprNode(call[2])}}
					return &ast.Node{Kind: "assign", Children: []*ast.Node{idx, exprNode(call[3])}}
				}
			}
			if name, ok := list[1].(string); ok {
				lhs := &ast.Node{Kind: "selector", Value: name}
				return &ast.Node{Kind: "assign", Children: []*ast.Node{lhs, exprNode(list[2])}}
			}
		}
	case "displayln":
		var arg any
		if len(list) > 1 {
			arg = list[1]
		}
		return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode(arg)}}
	case "for":
		if len(list) >= 3 {
			if binds, ok := list[1].([]any); ok && len(binds) > 0 {
				if bind, ok := binds[0].([]any); ok && len(bind) == 2 {
					varName, _ := bind[0].(string)
					n := &ast.Node{Kind: "for", Value: varName}
					if bexpr, ok := bind[1].([]any); ok && len(bexpr) >= 1 {
						if h, _ := bexpr[0].(string); h == "in-range" && len(bexpr) >= 3 {
							n.Children = append(n.Children, &ast.Node{Kind: "range", Children: []*ast.Node{exprNode(bexpr[1]), exprNode(bexpr[2])}})
						} else {
							n.Children = append(n.Children, &ast.Node{Kind: "in", Children: []*ast.Node{exprNode(bind[1])}})
						}
					} else {
						n.Children = append(n.Children, &ast.Node{Kind: "in", Children: []*ast.Node{exprNode(bind[1])}})
					}
					block := &ast.Node{Kind: "block"}
					for _, st := range list[2:] {
						if c := formNodeWithMut(st, mutated); c != nil {
							block.Children = append(block.Children, c)
						}
					}
					n.Children = append(n.Children, block)
					return n
				}
			}
		}
	default:
		return exprNode(d)
	}
	return nil
}

func exprNode(d any) *ast.Node {
	switch v := d.(type) {
	case string:
		if v == "#t" {
			return &ast.Node{Kind: "bool", Value: true}
		}
		if v == "#f" {
			return &ast.Node{Kind: "bool", Value: false}
		}
		if strings.HasPrefix(v, "\"") && strings.HasSuffix(v, "\"") {
			unq, err := strconv.Unquote(v)
			if err != nil {
				unq = strings.Trim(v, "\"")
			}
			return &ast.Node{Kind: "string", Value: unq}
		}
		return &ast.Node{Kind: "selector", Value: v}
	case float64:
		if v == float64(int(v)) {
			return &ast.Node{Kind: "int", Value: int(v)}
		}
		return &ast.Node{Kind: "float", Value: v}
	case []any:
		if len(v) == 0 {
			return &ast.Node{Kind: "list"}
		}
		head, _ := v[0].(string)
		args := v[1:]
		switch head {
		case "list":
			n := &ast.Node{Kind: "list"}
			for _, e := range args {
				n.Children = append(n.Children, exprNode(e))
			}
			return n
		case "hash":
			n := &ast.Node{Kind: "map"}
			for i := 0; i+1 < len(args); i += 2 {
				n.Children = append(n.Children, &ast.Node{Kind: "entry", Children: []*ast.Node{exprNode(args[i]), exprNode(args[i+1])}})
			}
			return n
		case "string-ref", "list-ref", "hash-ref":
			return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
		case "substring":
			n := &ast.Node{Kind: "call", Value: "substr"}
			n.Children = append(n.Children, exprNode(v[1]), exprNode(v[2]))
			if len(v) > 3 {
				n.Children = append(n.Children, exprNode(v[3]))
			}
			return n
		case "if":
			if len(args) >= 2 {
				n := &ast.Node{Kind: "if_expr", Children: []*ast.Node{exprNode(args[0]), exprNode(args[1])}}
				if len(args) > 2 {
					n.Children = append(n.Children, exprNode(args[2]))
				}
				return n
			}
		case "+", "-", "*", "/", "<", ">", "<=", ">=", "=", "and", "or":
			op := head
			switch head {
			case "=":
				op = "=="
			case "and":
				op = "&&"
			case "or":
				op = "||"
			}
			if head == "-" && len(args) == 1 {
				return &ast.Node{Kind: "unary", Value: "-", Children: []*ast.Node{exprNode(args[0])}}
			}
			node := &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{exprNode(args[0]), exprNode(args[1])}}
			for _, a := range args[2:] {
				node = &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{node, exprNode(a)}}
			}
			return node
		default:
			n := &ast.Node{Kind: "call", Value: head}
			for _, e := range args {
				n.Children = append(n.Children, exprNode(e))
			}
			return n
		}
	}
	return &ast.Node{Kind: "unknown"}
}
