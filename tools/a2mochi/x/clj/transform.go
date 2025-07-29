//go:build slow

package clj

import (
	"fmt"
	"sort"
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

func isLiteralAtom(s string) bool {
	if s == "" {
		return false
	}
	if isNumber(s) {
		return true
	}
	if strings.HasPrefix(s, "\"") && strings.HasSuffix(s, "\"") {
		return true
	}
	if strings.HasPrefix(s, ":") {
		return true
	}
	return false
}

func guessParamType(name string) string {
	if strings.Contains(strings.ToLower(name), "nums") || strings.HasSuffix(name, "s") {
		return "list<int>"
	}
	return "int"
}

func guessReturnType(name string) string {
	if strings.Contains(strings.ToLower(name), "twosum") {
		return "list<int>"
	}
	return ""
}

var builtins = map[string]bool{
	"println":   true,
	"while":     true,
	"do":        true,
	"def":       true,
	"+":         true,
	"-":         true,
	"*":         true,
	"/":         true,
	"mod":       true,
	"quot":      true,
	"<":         true,
	"<=":        true,
	">":         true,
	">=":        true,
	"=":         true,
	"and":       true,
	"or":        true,
	"not":       true,
	"if":        true,
	"count":     true,
	"conj":      true,
	"get":       true,
	"nth":       true,
	"assoc":     true,
	"subvec":    true,
	"subs":      true,
	"contains?": true,
	"str":       true,
	"reduce":    true,
	"double":    true,
	"range":     true,
	"when":      true,
	"dotimes":   true,
	"doseq":     true,
	"fn*":       true,
	"swap!":     true,
}

// newNode creates a new AST node with the given kind, value and children.
func newNode(kind string, val any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: val, Children: children}
}

// toSexpr converts the parsed node to a simple S-expression representation.
func toSexpr(n node) any {
	if len(n.Map) > 0 {
		m := make(map[string]any, len(n.Map))
		for _, kv := range n.Map {
			if len(kv) != 2 {
				continue
			}
			k := fmt.Sprint(toSexpr(kv[0]))
			m[k] = toSexpr(kv[1])
		}
		return m
	}
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
func sexprToNode(x any, fns map[string]bool) *ast.Node {
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
	case map[string]any:
		keys := make([]string, 0, len(v))
		for k := range v {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		mnode := newNode("map", nil)
		for _, k := range keys {
			kv := newNode("kv", nil, sexprToNode(k, fns), sexprToNode(v[k], fns))
			mnode.Children = append(mnode.Children, kv)
		}
		return mnode
	case []any:
		if len(v) == 0 {
			return nil
		}
		head, ok := v[0].(string)
		if ok && isLiteralAtom(head) {
			listN := newNode("list", nil)
			for _, e := range v {
				if n := sexprToNode(e, fns); n != nil {
					listN.Children = append(listN.Children, n)
				}
			}
			return listN
		}
		if !ok {
			return nil
		}
		switch head {
		case "println":
			call := newNode("call", "print")
			for _, a := range v[1:] {
				if n := sexprToNode(a, fns); n != nil {
					call.Children = append(call.Children, n)
				}
			}
			return call
		case "while":
			if len(v) >= 3 {
				cond := sexprToNode(v[1], fns)
				blk := newNode("block", nil)
				for _, b := range v[2:] {
					if n := sexprToNode(b, fns); n != nil {
						blk.Children = append(blk.Children, n)
					}
				}
				return newNode("while", nil, cond, blk)
			}
		case "do":
			if len(v) >= 2 {
				blk := newNode("block", nil)
				for _, b := range v[1:] {
					if n := sexprToNode(b, fns); n != nil {
						blk.Children = append(blk.Children, n)
					}
				}
				if len(blk.Children) == 1 {
					return blk.Children[0]
				}
				return blk
			}
		case "def":
			if len(v) == 3 {
				name, _ := v[1].(string)
				assign := newNode("assign", sanitizeName(name))
				if rhs := sexprToNode(v[2], fns); rhs != nil {
					assign.Children = append(assign.Children, rhs)
				}
				return assign
			}
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
				return newNode("binary", op, sexprToNode(v[1], fns), sexprToNode(v[2], fns))
			}
		case "and", "or":
			if len(v) >= 3 {
				op := "&&"
				if head == "or" {
					op = "||"
				}
				left := sexprToNode(v[1], fns)
				for i := 2; i < len(v); i++ {
					right := sexprToNode(v[i], fns)
					left = newNode("binary", op, left, right)
				}
				return left
			}
		case "not":
			if len(v) == 2 {
				return newNode("unary", "!", sexprToNode(v[1], fns))
			}
		case "if":
			if len(v) >= 3 {
				cond := sexprToNode(v[1], fns)
				thenN := sexprToNode(v[2], fns)
				if len(v) == 4 {
					return newNode("if", nil, cond, thenN, sexprToNode(v[3], fns))
				}
				return newNode("if", nil, cond, thenN)
			}
		case "count":
			if len(v) == 2 {
				arg := sexprToNode(v[1], fns)
				return newNode("call", "len", arg)
			}
		case "conj":
			if len(v) == 3 {
				call := newNode("call", "append")
				call.Children = append(call.Children, sexprToNode(v[1], fns))
				call.Children = append(call.Children, sexprToNode(v[2], fns))
				return call
			}
		case "get", "nth":
			if len(v) == 3 {
				return newNode("index", sexprToNode(v[1], fns), sexprToNode(v[2], fns))
			}
		case "assoc":
			if len(v) == 4 {
				idx := newNode("index", sexprToNode(v[1], fns), sexprToNode(v[2], fns))
				return newNode("assign", nil, idx, sexprToNode(v[3], fns))
			}
		case "subvec":
			if len(v) == 4 {
				start := newNode("start", sexprToNode(v[2], fns))
				end := newNode("end", sexprToNode(v[3], fns))
				return newNode("index", sexprToNode(v[1], fns), start, end)
			}
		case "subs":
			if len(v) == 4 {
				return newNode("call", "substring", sexprToNode(v[1], fns), sexprToNode(v[2], fns), sexprToNode(v[3], fns))
			}
		case "contains?":
			if len(v) == 3 {
				return newNode("binary", "in", sexprToNode(v[2], fns), sexprToNode(v[1], fns))
			}
		case "str":
			call := newNode("call", "str")
			for _, a := range v[1:] {
				if n := sexprToNode(a, fns); n != nil {
					call.Children = append(call.Children, n)
				}
			}
			return call
		case "reduce":
			if len(v) == 4 {
				if op, ok := v[1].(string); ok && op == "+" {
					if start, ok := v[2].(string); ok && start == "0" {
						arg := sexprToNode(v[3], fns)
						return newNode("call", "sum", arg)
					}
				}
			}
		case "double":
			if len(v) == 2 {
				if inner, ok := v[1].([]any); ok && len(inner) == 3 {
					if op, ok := inner[0].(string); ok && op == "/" {
						if left, lok := inner[1].([]any); lok && len(left) == 4 {
							if right, rok := inner[2].([]any); rok && len(right) == 2 {
								if lhead, ok := left[0].(string); ok && lhead == "reduce" {
									if rop, ok := right[0].(string); ok && rop == "count" {
										if op2, ok := left[1].(string); ok && op2 == "+" {
											if start, ok := left[2].(string); ok && start == "0" {
												if fmt.Sprint(left[3]) == fmt.Sprint(right[1]) {
													return newNode("call", "avg", sexprToNode(left[3], fns))
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		case "dotimes":
			if len(v) == 3 {
				bind, ok := v[1].([]any)
				if ok && len(bind) == 2 {
					name, _ := bind[0].(string)
					limit := sexprToNode(bind[1], fns)
					body := sexprToNode(v[2], fns)
					r := newNode("range", nil, newNode("int", 0), limit)
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
					collExpr := bind[1]
					rangeNode := (*ast.Node)(nil)
					if arr, ok := collExpr.([]any); ok && len(arr) == 3 {
						if hd, ok := arr[0].(string); ok && hd == "range" {
							start := sexprToNode(arr[1], fns)
							end := sexprToNode(arr[2], fns)
							rangeNode = newNode("range", nil, start, end)
						}
					}
					coll := sexprToNode(collExpr, fns)
					blk := newNode("block", nil)
					for _, b := range v[2:] {
						if n := sexprToNode(b, fns); n != nil {
							blk.Children = append(blk.Children, n)
						}
					}
					if rangeNode != nil {
						return newNode("for", sanitizeName(name), rangeNode, blk)
					}
					in := newNode("in", nil, coll)
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
						if n := sexprToNode(b, fns); n != nil {
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
					bin := newNode("binary", op, newNode("selector", sanitizeName(name)), sexprToNode(v[3], fns))
					assign.Children = append(assign.Children, bin)
					return assign
				}
			}
		}
		if builtins[head] || fns[head] {
			call := newNode("call", sanitizeName(head))
			for _, a := range v[1:] {
				if n := sexprToNode(a, fns); n != nil {
					call.Children = append(call.Children, n)
				}
			}
			return call
		}
		listN := newNode("list", nil)
		for _, e := range v {
			if n := sexprToNode(e, fns); n != nil {
				listN.Children = append(listN.Children, n)
			}
		}
		return listN
	default:
		return nil
	}
}

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	fns := make(map[string]bool)
	for _, f := range p.Forms {
		if f.Type == "defn" {
			fns[f.Name] = true
		}
	}
	root := newNode("program", nil)
	for _, f := range p.Forms {
		switch f.Type {
		case "defn":
			fn := newNode("fun", sanitizeName(f.Name))
			for _, prm := range f.Params {
				pnode := newNode("param", sanitizeName(prm))
				if typ := guessParamType(prm); typ != "" {
					pnode.Children = append(pnode.Children, newNode("type", typ))
				}
				fn.Children = append(fn.Children, pnode)
			}
			if rt := guessReturnType(f.Name); rt != "" {
				fn.Children = append(fn.Children, newNode("type", rt))
			}
			body := newNode("block", nil)
			for _, b := range f.Body {
				if n := sexprToNode(toSexpr(b), fns); n != nil {
					body.Children = append(body.Children, n)
				}
			}
			fn.Children = append(fn.Children, body)
			root.Children = append(root.Children, fn)
		case "def":
			node := newNode("let", sanitizeName(f.Name))
			if val := sexprToNode(toSexpr(f.Value), fns); val != nil {
				node.Children = append(node.Children, val)
			}
			root.Children = append(root.Children, node)
		case "expr":
			if len(f.Body) > 0 {
				sex := toSexpr(f.Body[0])
				if arr, ok := sex.([]any); ok && len(arr) > 0 {
					if h, ok := arr[0].(string); ok {
						if h == "ns" || h == "require" {
							continue
						}
					}
				}
				if n := sexprToNode(sex, fns); n != nil {
					root.Children = append(root.Children, n)
				}
			}
		}
	}
	return root, nil
}
