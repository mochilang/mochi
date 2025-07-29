//go:build slow

package clj

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/ast"
)

// sanitizeName converts Clojure identifiers into valid Mochi identifiers.
func sanitizeName(s string) string {
	s = strings.TrimLeft(s, "-")
	s = strings.ReplaceAll(s, "->", "_to_")
	s = strings.ReplaceAll(s, "-", "_")
	s = strings.ReplaceAll(s, "?", "_p")
	s = strings.ReplaceAll(s, "!", "_bang")
	s = strings.ReplaceAll(s, "/", "_")
	s = strings.ReplaceAll(s, ".", "_")
	if s == "" {
		return "_"
	}
	if s[0] >= '0' && s[0] <= '9' {
		s = "_" + s
	}
	return s
}

func isNumber(s string) bool {
	if _, err := strconv.ParseFloat(s, 64); err == nil {
		return true
	}
	return false
}

// newNode creates a new AST node with the given kind, value and children.
func newNode(kind string, val any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: val, Children: children}
}

// toSexpr converts the parsed node to a simple S-expression representation.
func toSexpr(n node) any {
	if len(n.List) > 0 {
		out := make([]any, len(n.List))
		for i, c := range n.List {
			out[i] = toSexpr(c)
		}
		return out
	}
	return n.Atom
}

// sexprToNode converts the generic S-expression to a Mochi AST node.
func sexprToNode(x any) *ast.Node {
	switch v := x.(type) {
	case string:
		if strings.HasPrefix(v, "\"") && strings.HasSuffix(v, "\"") {
			return newNode("string", strings.Trim(v, "\""))
		}
		if v == "true" || v == "false" {
			return newNode("bool", v == "true")
		}
		if i, err := strconv.Atoi(v); err == nil {
			return newNode("int", i)
		}
		if f, err := strconv.ParseFloat(v, 64); err == nil {
			return newNode("float", f)
		}
		if strings.HasPrefix(v, ":") {
			v = strings.TrimPrefix(v, ":")
		}
		return newNode("selector", sanitizeName(v))
	case []any:
		if len(v) == 0 {
			return nil
		}
		head, ok := v[0].(string)
		if !ok {
			return nil
		}
		switch head {
		case "println":
			call := newNode("call", "print")
			for _, a := range v[1:] {
				if n := sexprToNode(a); n != nil {
					call.Children = append(call.Children, n)
				}
			}
			return call
		case "+", "-", "*", "/", "mod", "quot", "<", "<=", ">", ">=", "=":
			if len(v) == 3 {
				op := head
				if op == "mod" {
					op = "%"
				} else if op == "quot" {
					op = "/"
				} else if op == "=" {
					op = "=="
				}
				return newNode("binary", op, sexprToNode(v[1]), sexprToNode(v[2]))
			}
		case "and", "or":
			if len(v) >= 3 {
				op := "&&"
				if head == "or" {
					op = "||"
				}
				left := sexprToNode(v[1])
				for i := 2; i < len(v); i++ {
					right := sexprToNode(v[i])
					left = newNode("binary", op, left, right)
				}
				return left
			}
		case "not":
			if len(v) == 2 {
				return newNode("unary", "!", sexprToNode(v[1]))
			}
		case "if":
			if len(v) >= 3 {
				cond := sexprToNode(v[1])
				thenN := sexprToNode(v[2])
				if len(v) == 4 {
					return newNode("if", nil, cond, thenN, sexprToNode(v[3]))
				}
				return newNode("if", nil, cond, thenN)
			}
		case "dotimes":
			if len(v) == 3 {
				bind, ok := v[1].([]any)
				if ok && len(bind) == 2 {
					name, _ := bind[0].(string)
					limit := sexprToNode(bind[1])
					body := sexprToNode(v[2])
					r := newNode("range", newNode("int", 0), limit)
					blk := newNode("block", nil)
					if body != nil {
						blk.Children = append(blk.Children, body)
					}
					return newNode("for", sanitizeName(name), r, blk)
				}
			}
		case "doseq":
			if len(v) >= 3 {
				bind, ok := v[1].([]any)
				if ok && len(bind) == 2 {
					name, _ := bind[0].(string)
					coll := sexprToNode(bind[1])
					blk := newNode("block", nil)
					for _, b := range v[2:] {
						if n := sexprToNode(b); n != nil {
							blk.Children = append(blk.Children, n)
						}
					}
					in := newNode("in", coll)
					return newNode("for", sanitizeName(name), in, blk)
				}
			}
		case "fn*":
			if len(v) >= 3 {
				params, ok := v[1].([]any)
				if ok {
					fn := newNode("fun", "")
					for _, p := range params {
						if ps, ok := p.(string); ok {
							fn.Children = append(fn.Children, newNode("param", sanitizeName(ps)))
						}
					}
					blk := newNode("block", nil)
					for _, b := range v[2:] {
						if n := sexprToNode(b); n != nil {
							blk.Children = append(blk.Children, n)
						}
					}
					fn.Children = append(fn.Children, blk)
					return fn
				}
			}
		case "swap!":
			if len(v) == 4 {
				name, _ := v[1].(string)
				opStr, _ := v[2].(string)
				op := opStr
				if op == "-" || op == "+" || op == "*" || op == "/" {
					assign := newNode("assign", sanitizeName(name))
					bin := newNode("binary", op, newNode("selector", sanitizeName(name)), sexprToNode(v[3]))
					assign.Children = append(assign.Children, bin)
					return assign
				}
			}
		}
		call := newNode("call", sanitizeName(head))
		for _, a := range v[1:] {
			if n := sexprToNode(a); n != nil {
				call.Children = append(call.Children, n)
			}
		}
		return call
	default:
		return nil
	}
}

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := newNode("program", nil)
	for _, f := range p.Forms {
		switch f.Type {
		case "defn":
			fn := newNode("fun", sanitizeName(f.Name))
			for _, prm := range f.Params {
				fn.Children = append(fn.Children, newNode("param", sanitizeName(prm)))
			}
			body := newNode("block", nil)
			for _, b := range f.Body {
				if n := sexprToNode(toSexpr(b)); n != nil {
					body.Children = append(body.Children, n)
				}
			}
			fn.Children = append(fn.Children, body)
			root.Children = append(root.Children, fn)
		case "def":
			node := newNode("let", sanitizeName(f.Name))
			if val := sexprToNode(toSexpr(f.Value)); val != nil {
				node.Children = append(node.Children, val)
			}
			root.Children = append(root.Children, node)
		case "expr":
			if len(f.Body) > 0 {
				if n := sexprToNode(toSexpr(f.Body[0])); n != nil {
					root.Children = append(root.Children, n)
				}
			}
		}
	}
	return root, nil
}
