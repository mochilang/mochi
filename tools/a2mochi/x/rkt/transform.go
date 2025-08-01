//go:build slow

package rkt

import (
	"fmt"
	"reflect"

	"mochi/ast"
)

func symString(v any) (string, bool) {
	if s, ok := v.(string); ok {
		return s, true
	}
	if m, ok := v.(map[string]any); ok {
		if sym, ok := m["sym"].(string); ok {
			return sym, true
		}
	}
	return "", false
}

func groupIfUnary(n *ast.Node) *ast.Node {
	if n != nil && n.Kind == "unary" {
		return &ast.Node{Kind: "group", Children: []*ast.Node{n}}
	}
	return n
}

func precedence(op string) int {
	switch op {
	case "*", "/", "%":
		return 3
	case "+", "-":
		return 2
	case "<", ">", "<=", ">=", "==":
		return 1
	case "&&", "||":
		return 0
	}
	return -1
}

func groupIfPrecedence(n *ast.Node, op string) *ast.Node {
	n = groupIfUnary(n)
	if n != nil && n.Kind == "binary" {
		if val, ok := n.Value.(string); ok && precedence(val) < precedence(op) {
			return &ast.Node{Kind: "group", Children: []*ast.Node{n}}
		}
	}
	return n
}

func condLen(args []any) *ast.Node {
	if len(args) != 3 {
		return nil
	}
	c1, ok1 := args[0].([]any)
	c2, ok2 := args[1].([]any)
	c3, ok3 := args[2].([]any)
	if !ok1 || !ok2 || !ok3 {
		return nil
	}
	if len(c1) != 2 || len(c2) != 2 || len(c3) != 2 {
		return nil
	}
	t1, ok := c1[0].([]any)
	if !ok || len(t1) != 2 {
		return nil
	}
	t2, ok := c2[0].([]any)
	if !ok || len(t2) != 2 {
		return nil
	}
	h1, _ := symString(t1[0])
	h2, _ := symString(t2[0])
	if h1 != "string?" || h2 != "hash?" {
		return nil
	}
	e1 := t1[1]
	e2 := t2[1]
	if !reflect.DeepEqual(e1, e2) {
		return nil
	}
	b1, ok := c1[1].([]any)
	if !ok || len(b1) != 2 {
		return nil
	}
	b2, ok := c2[1].([]any)
	if !ok || len(b2) != 2 {
		return nil
	}
	b3, ok := c3[1].([]any)
	if !ok || len(b3) != 2 {
		return nil
	}
	h1b, _ := symString(b1[0])
	h2b, _ := symString(b2[0])
	h3, _ := symString(c3[0])
	if h1b != "string-length" || h2b != "hash-count" || h3 != "else" {
		return nil
	}
	e1b := b1[1]
	e2b := b2[1]
	e3 := b3[1]
	if reflect.DeepEqual(e1b, e2b) && reflect.DeepEqual(e1b, e3) {
		return &ast.Node{Kind: "call", Value: "len", Children: []*ast.Node{exprNode(e1)}}
	}
	return nil
}

func condExpr(args []any) *ast.Node {
	if len(args) == 0 {
		return &ast.Node{Kind: "unknown"}
	}
	clause, ok := args[0].([]any)
	if !ok || len(clause) < 2 {
		return exprNode(clause)
	}
	head, _ := symString(clause[0])
	if head == "else" {
		return exprNode(clause[1])
	}
	cond := exprNode(clause[0])
	thenExpr := exprNode(clause[1])
	n := &ast.Node{Kind: "if_expr", Children: []*ast.Node{cond, thenExpr}}
	if len(args) > 1 {
		n.Children = append(n.Children, condExpr(args[1:]))
	}
	return n
}

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	mutated := map[string]bool{}
	var collect func(any)
	collect = func(d any) {
		if list, ok := d.([]any); ok {
			if len(list) >= 2 {
				if head, _ := symString(list[0]); head == "set!" {
					if name, ok := symString(list[1]); ok {
						mutated[name] = true
					}
				}
			}
			for _, e := range list {
				collect(e)
			}
		}
	}
	for _, f := range p.Forms {
		collect(f.Datum)
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
	head, _ := symString(list[0])
	switch head {
	case "require", "struct":
		return nil
	case "when":
		if len(list) >= 2 {
			block := &ast.Node{Kind: "block"}
			for _, st := range list[2:] {
				if c := formNodeWithMut(st, mutated); c != nil {
					block.Children = append(block.Children, c)
				}
			}
			return &ast.Node{Kind: "if", Children: []*ast.Node{exprNode(list[1]), block}}
		}
	case "define":
		if fn, ok := list[1].([]any); ok {
			var name string
			params := fn[1:]
			if n, ok := symString(fn[0]); ok {
				name = n
			} else if firstList, ok := fn[0].([]any); ok {
				name, _ = symString(firstList[0])
				params = firstList[1:]
			}
			n := &ast.Node{Kind: "fun", Value: name}
			for _, p := range params {
				if s, ok := symString(p); ok {
					n.Children = append(n.Children, &ast.Node{
						Kind:     "param",
						Value:    s,
						Children: []*ast.Node{{Kind: "type", Value: "int"}},
					})
				}
			}
			retType := "int"
			if len(list) > 2 {
				if _, ok := list[len(list)-1].(bool); ok {
					retType = "bool"
				}
			}
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: retType})
			bodies := list[2:]
			for i, body := range bodies {
				if i == len(bodies)-1 {
					n.Children = append(n.Children, &ast.Node{
						Kind:     "return",
						Children: []*ast.Node{exprNode(body)},
					})
				} else if stmt := formNodeWithMut(body, mutated); stmt != nil {
					n.Children = append(n.Children, stmt)
				}
			}
			return n
		}
		if name, ok := symString(list[1]); ok && len(list) >= 3 {
			// special case for typed declarations generated by the transpiler
			if v, ok := list[2].(float64); ok && v == 0 && !mutated[name] {
				return &ast.Node{Kind: "var", Value: name, Children: []*ast.Node{{Kind: "type", Value: "int"}}}
			}
			kind := "let"
			if mutated[name] {
				kind = "var"
			}
			return &ast.Node{Kind: kind, Value: name, Children: []*ast.Node{exprNode(list[2])}}
		}
	case "set!":
		if len(list) == 3 {
			if call, ok := list[2].([]any); ok && len(call) >= 4 {
				ch, _ := symString(call[0])
				if ch == "list-set" || ch == "hash-set" {
					idx := &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(call[1]), exprNode(call[2])}}
					return &ast.Node{Kind: "assign", Children: []*ast.Node{idx, exprNode(call[3])}}
				}
			}
			if name, ok := symString(list[1]); ok {
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
	case "if":
		if len(list) >= 3 {
			cond := exprNode(list[1])
			thenBlock := &ast.Node{Kind: "block"}
			if c := formNodeWithMut(list[2], mutated); c != nil {
				thenBlock.Children = append(thenBlock.Children, c)
			}
			n := &ast.Node{Kind: "if", Children: []*ast.Node{cond, thenBlock}}
			if len(list) >= 4 {
				elseBlock := &ast.Node{Kind: "block"}
				if c := formNodeWithMut(list[3], mutated); c != nil {
					elseBlock.Children = append(elseBlock.Children, c)
				}
				n.Children = append(n.Children, elseBlock)
			}
			return n
		}
	case "let":
		if len(list) == 4 {
			if name, ok := symString(list[1]); ok {
				if params, ok := list[2].([]any); ok && len(params) == 0 {
					if body, ok := list[3].([]any); ok && len(body) >= 1 {
						if head, _ := symString(body[0]); head == "when" && len(body) >= 2 {
							whenBody := body[2:]
							last := whenBody[len(whenBody)-1]
							if call, ok := last.([]any); ok {
								if h, _ := symString(call[0]); h == name {
									cond := exprNode(body[1])
									block := &ast.Node{Kind: "block"}
									for _, st := range whenBody[:len(whenBody)-1] {
										if c := formNodeWithMut(st, mutated); c != nil {
											block.Children = append(block.Children, c)
										}
									}
									return &ast.Node{Kind: "while", Children: []*ast.Node{cond, block}}
								}
							}
						}
					}
				}
			}
		}
	case "for":
		if len(list) >= 3 {
			if binds, ok := list[1].([]any); ok && len(binds) > 0 {
				if bind, ok := binds[0].([]any); ok && len(bind) >= 2 {
					varName, _ := symString(bind[0])
					n := &ast.Node{Kind: "for", Value: varName}
					if bexpr, ok := bind[1].([]any); ok && len(bexpr) >= 1 {
						if h, _ := symString(bexpr[0]); h == "in-range" && len(bexpr) >= 3 {
							n.Children = append(n.Children, &ast.Node{Kind: "range", Children: []*ast.Node{exprNode(bexpr[1]), exprNode(bexpr[2])}})
						} else if (h == "in-hash-keys" || h == "in-hash-values") && len(bexpr) == 2 {
							n.Children = append(n.Children, &ast.Node{Kind: "in", Children: []*ast.Node{exprNode(bexpr[1])}})
						} else {
							n.Children = append(n.Children, &ast.Node{Kind: "in", Children: []*ast.Node{exprNode(bind[1])}})
						}
					} else {
						n.Children = append(n.Children, &ast.Node{Kind: "in", Children: []*ast.Node{exprNode(bind[1])}})
					}
					block := &ast.Node{Kind: "block"}
					// handle loop keywords like #:break and #:unless
					if len(bind) > 2 {
						for i := 2; i+1 < len(bind); i += 2 {
							kw, _ := symString(bind[i])
							cond := exprNode(bind[i+1])
							switch kw {
							case "break":
								block.Children = append(block.Children, &ast.Node{Kind: "if", Children: []*ast.Node{cond, &ast.Node{Kind: "break"}}})
							case "unless":
								block.Children = append(block.Children, &ast.Node{Kind: "if", Children: []*ast.Node{cond, &ast.Node{Kind: "continue"}}})
							}
						}
					}
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
		return &ast.Node{Kind: "string", Value: v}
	case bool:
		return &ast.Node{Kind: "bool", Value: v}
	case map[string]any:
		if sym, ok := v["sym"].(string); ok {
			return &ast.Node{Kind: "selector", Value: sym}
		}
		return &ast.Node{Kind: "unknown"}
	case float64:
		if v == float64(int(v)) {
			return &ast.Node{Kind: "int", Value: int(v)}
		}
		return &ast.Node{Kind: "float", Value: v}
	case []any:
		if len(v) == 0 {
			return &ast.Node{Kind: "list"}
		}
		head, _ := symString(v[0])
		args := v[1:]
		switch head {
		case "displayln":
			var arg any
			if len(args) > 0 {
				arg = args[0]
			}
			return &ast.Node{Kind: "call", Value: "print", Children: []*ast.Node{exprNode(arg)}}
		case "quote":
			if len(args) == 1 {
				if list, ok := args[0].([]any); ok && len(list) == 0 {
					return &ast.Node{Kind: "list"}
				}
				if s, ok := symString(args[0]); ok {
					return &ast.Node{Kind: "string", Value: s}
				}
				return exprNode(args[0])
			}
		case "sublist":
			if len(args) >= 3 {
				start := &ast.Node{Kind: "start", Children: []*ast.Node{exprNode(args[1])}}
				end := &ast.Node{Kind: "end", Children: []*ast.Node{exprNode(args[2])}}
				return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(args[0]), start, end}}
			}
		case "lambda":
			if len(args) >= 1 {
				fn := &ast.Node{Kind: "funexpr"}
				if params, ok := args[0].([]any); ok {
					for _, p := range params {
						if s, ok := symString(p); ok {
							fn.Children = append(fn.Children, &ast.Node{Kind: "param", Value: s, Children: []*ast.Node{{Kind: "type", Value: "int"}}})
						}
					}
				}
				retType := "int"
				if len(args) > 1 {
					if _, ok := args[len(args)-1].(bool); ok {
						retType = "bool"
					}
					fn.Children = append(fn.Children, &ast.Node{Kind: "type", Value: retType})
					fn.Children = append(fn.Children, exprNode(args[len(args)-1]))
				}
				return fn
			}
		case "begin":
			if len(args) > 0 {
				return exprNode(args[len(args)-1])
			}
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
		case "string-append":
			if len(args) == 2 {
				return &ast.Node{Kind: "binary", Value: "+", Children: []*ast.Node{exprNode(args[0]), exprNode(args[1])}}
			}
		case "string-join":
			if len(args) == 2 {
				return &ast.Node{Kind: "call", Value: "join", Children: []*ast.Node{exprNode(args[0]), exprNode(args[1])}}
			}
		case "string-contains?":
			if len(v) == 3 {
				sel := &ast.Node{Kind: "selector", Value: "contains", Children: []*ast.Node{exprNode(v[1])}}
				return &ast.Node{Kind: "call", Children: []*ast.Node{sel, exprNode(v[2])}}
			}
		case "quotient":
			if len(v) == 3 {
				div := &ast.Node{Kind: "binary", Value: "/", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
				typ := &ast.Node{Kind: "type", Value: "int"}
				return &ast.Node{Kind: "cast", Children: []*ast.Node{div, typ}}
			}
		case "modulo":
			if len(v) == 3 {
				return &ast.Node{Kind: "binary", Value: "%", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
			}
		case "string-ref", "list-ref":
			return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
		case "hash-ref":
			if len(v) == 3 {
				if s, ok := v[2].(string); ok {
					return &ast.Node{Kind: "selector", Value: s, Children: []*ast.Node{exprNode(v[1])}}
				}
			}
			return &ast.Node{Kind: "index", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
		case "substring":
			n := &ast.Node{Kind: "call", Value: "substring"}
			n.Children = append(n.Children, exprNode(v[1]), exprNode(v[2]))
			if len(v) > 3 {
				n.Children = append(n.Children, exprNode(v[3]))
			}
			return n
		case "string=?":
			if len(v) == 3 {
				return &ast.Node{Kind: "binary", Value: "==", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
			}
		case "string<?", "string>?", "string<=?", "string>=?":
			op := map[string]string{"string<?": "<", "string>?": ">", "string<=?": "<=", "string>=?": ">="}[head]
			return &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
		case "not":
			if len(v) == 2 {
				return &ast.Node{Kind: "unary", Value: "!", Children: []*ast.Node{exprNode(v[1])}}
			}
		case "member":
			if len(v) == 3 {
				return &ast.Node{Kind: "binary", Value: "in", Children: []*ast.Node{exprNode(v[1]), exprNode(v[2])}}
			}
		case "if":
			if len(args) >= 2 {
				// special case: (if (null? xs) 0 (apply min xs)) -> min(xs)
				if cond, ok := args[0].([]any); ok && len(cond) == 2 {
					if h, _ := symString(cond[0]); h == "null?" {
						if zero, ok := args[1].(float64); ok && zero == 0 && len(args) == 3 {
							if apply, ok := args[2].([]any); ok && len(apply) == 3 {
								if ah, _ := symString(apply[0]); ah == "apply" {
									if op, _ := symString(apply[1]); op == "min" || op == "max" {
										return &ast.Node{Kind: "call", Value: op, Children: []*ast.Node{exprNode(apply[2])}}
									}
								}
							}
						}
					}
				}
				n := &ast.Node{Kind: "if_expr", Children: []*ast.Node{exprNode(args[0]), exprNode(args[1])}}
				if len(args) > 2 {
					n.Children = append(n.Children, exprNode(args[2]))
				}
				return n
			}
		case "cond":
			if len(args) > 0 {
				if n := condLen(args); n != nil {
					return n
				}
				return condExpr(args)
			}
		case "apply":
			if len(v) == 3 {
				if op, _ := symString(v[1]); op == "+" {
					return &ast.Node{Kind: "call", Value: "sum", Children: []*ast.Node{exprNode(v[2])}}
				}
			}
		case "format":
			if len(v) == 3 {
				if fmtStr, ok := v[1].(string); ok && fmtStr == "~a" {
					return &ast.Node{Kind: "call", Value: "str", Children: []*ast.Node{exprNode(v[2])}}
				}
			}
		case "exact->inexact":
			if len(v) == 2 {
				return &ast.Node{Kind: "call", Value: "float", Children: []*ast.Node{exprNode(v[1])}}
			}
		case "length":
			if len(v) == 2 {
				return &ast.Node{Kind: "call", Value: "len", Children: []*ast.Node{exprNode(v[1])}}
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
			left := groupIfPrecedence(exprNode(args[0]), op)
			right := groupIfPrecedence(exprNode(args[1]), op)
			node := &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{left, right}}
			for _, a := range args[2:] {
				node = &ast.Node{Kind: "binary", Value: op, Children: []*ast.Node{node, groupIfPrecedence(exprNode(a), op)}}
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
