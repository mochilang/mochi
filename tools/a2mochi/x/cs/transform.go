//go:build slow

package cs

import (
	"fmt"
	"strconv"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

func parenBalanced(s string) bool {
	depth := 0
	for _, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
			if depth < 0 {
				return false
			}
		}
	}
	return depth == 0
}

func stripOuterParens(s string) string {
	for len(s) > 1 && strings.HasPrefix(s, "(") && strings.HasSuffix(s, ")") {
		inner := s[1 : len(s)-1]
		if parenBalanced(inner) {
			s = strings.TrimSpace(inner)
			continue
		}
		break
	}
	return s
}

// TestRewrite exposes rewriteExpr for tests and debugging.

func rewriteExpr(s string) string {
	s = strings.TrimSpace(s)
	for {
		orig := s
		s = stripOuterParens(s)

		// remove ToString calls
		s = strings.ReplaceAll(s, `.ToString("0.0")`, "")
		s = strings.ReplaceAll(s, `.ToString()`, "")
		if strings.HasPrefix(s, "ToString(") && strings.HasSuffix(s, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(s, "ToString("), ")")
			parts := splitArgs(inner)
			if len(parts) > 0 {
				s = strings.TrimSpace(parts[0])
			}
		}

		// lambda (int x) => (x*x) -> fun(x: int) => x*x
		if arrow := strings.Index(s, "=>"); arrow != -1 {
			left := strings.TrimSpace(s[:arrow])
			right := strings.TrimSpace(s[arrow+2:])
			if strings.HasPrefix(left, "(") && strings.HasSuffix(left, ")") {
				params := strings.Trim(left, "()")
				var out []string
				for _, p := range strings.Split(params, ",") {
					p = strings.TrimSpace(p)
					if p == "" {
						continue
					}
					parts := strings.Fields(p)
					if len(parts) == 2 {
						if t := mapType(parts[0]); t != "" {
							out = append(out, fmt.Sprintf("%s: %s", parts[1], t))
						} else {
							out = append(out, parts[1])
						}
					} else {
						out = append(out, p)
					}
				}
				s = fmt.Sprintf("fun(%s) => %s", strings.Join(out, ", "), rewriteExpr(right))
			}
		}

		// string.Join with comma separator -> list expression
		if strings.HasPrefix(s, "string.Join(") {
			inner := strings.TrimPrefix(s, "string.Join(")
			if end := strings.LastIndex(inner, ")"); end != -1 {
				inner = inner[:end]
				rest := strings.TrimSpace(inner)
				if strings.HasPrefix(rest, "\",\"") {
					rest = strings.TrimSpace(rest[len("\",\""):])
					if strings.HasPrefix(rest, ",") {
						rest = strings.TrimSpace(rest[1:])
					}
					arg := stripOuterParens(rest)
					s = arg
				} else {
					parts := splitArgs(rest)
					if len(parts) > 0 {
						arg := stripOuterParens(parts[len(parts)-1])
						s = fmt.Sprintf("str(%s)", arg)
					}
				}
			}
		}

		// remove surrounding "[" + expr + "]" patterns
		if strings.HasPrefix(s, "\"[\" +") && strings.HasSuffix(s, "+ \"]\"") {
			inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(s, "\"[\" +"), "+ \"]\""))
			s = inner
		}

		// <x>.Append(y).ToArray() -> append(x, y)
		if strings.Contains(s, ".Append(") && strings.HasSuffix(s, ").ToArray()") {
			before := stripOuterParens(strings.TrimSpace(s[:strings.Index(s, ".Append(")]))
			rest := s[strings.Index(s, ".Append(")+len(".Append("):]
			if idx := strings.Index(rest, ")"); idx != -1 {
				arg := rest[:idx]
				s = fmt.Sprintf("append(%s, %s)", strings.TrimSpace(before), strings.TrimSpace(arg))
			}
		}

		// new T[]{...} -> [...]
		for {
			start := strings.Index(s, "new ")
			if start == -1 {
				break
			}
			arr := s[start:]
			open := strings.Index(arr, "{")
			close := strings.Index(arr, "}")
			if open == -1 || close == -1 || close < open {
				break
			}
			inner := strings.ReplaceAll(arr[open+1:close], " ", "")
			s = s[:start] + "[" + inner + "]" + arr[close+1:]
		}

		// Average() -> avg()
		if idx := strings.Index(s, ".Average()"); idx != -1 {
			before := strings.TrimSpace(s[:idx])
			after := s[idx+len(".Average()"):]
			for strings.HasPrefix(before, "(") && strings.HasPrefix(after, ")") {
				before = strings.TrimPrefix(before, "(")
				after = strings.TrimPrefix(after, ")")
			}
			before = strings.TrimSuffix(before, ")")
			s = "avg(" + strings.TrimSpace(before) + ")" + after
		}
		if strings.HasPrefix(s, "Average(") && strings.HasSuffix(s, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(s, "Average("), ")")
			s = fmt.Sprintf("avg(%s)", stripOuterParens(inner))
		}

		// ToArray() -> expression
		if strings.Contains(s, ".ToArray()") {
			s = strings.ReplaceAll(s, ".ToArray()", "")
		}

		// ToList() -> expression
		if strings.Contains(s, ".ToList()") {
			s = strings.ReplaceAll(s, ".ToList()", "")
		}
		if strings.HasPrefix(s, "ToList(") && strings.HasSuffix(s, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(s, "ToList("), ")")
			s = stripOuterParens(inner)
		}

		// Any() -> exists()
		if strings.HasSuffix(s, ".Any()") {
			before := strings.TrimSuffix(s, ".Any()")
			s = fmt.Sprintf("exists(%s)", stripOuterParens(before))
		}

		// Sum() -> sum()
		if strings.HasSuffix(s, ".Sum()") {
			before := strings.TrimSuffix(s, ".Sum()")
			s = fmt.Sprintf("sum(%s)", stripOuterParens(before))
		}

		// Contains(x) -> x in expr
		if idx := strings.LastIndex(s, ".Contains("); idx != -1 && strings.HasSuffix(s, ")") {
			before := stripOuterParens(strings.TrimSpace(s[:idx]))
			arg := strings.TrimSuffix(s[idx+len(".Contains("):], ")")
			if parenBalanced(arg) {
				s = fmt.Sprintf("%s in %s", stripOuterParens(arg), before)
			}
		}

		// string.Compare(a, b) -> if a < b { -1 } else if a > b { 1 } else { 0 }
		if strings.HasPrefix(s, "string.Compare(") && strings.HasSuffix(s, ")") {
			args := splitArgs(strings.TrimSuffix(strings.TrimPrefix(s, "string.Compare("), ")"))
			if len(args) >= 2 {
				a := stripOuterParens(args[0])
				b := stripOuterParens(args[1])
				s = fmt.Sprintf("if %s < %s { -1 } else if %s > %s { 1 } else { 0 }", a, b, a, b)
			}
		}

		// Add(x) -> append(expr, x)
		if idx := strings.LastIndex(s, ".Add("); idx != -1 && strings.HasSuffix(s, ")") {
			before := stripOuterParens(strings.TrimSpace(s[:idx]))
			arg := strings.TrimSuffix(s[idx+len(".Add("):], ")")
			if parenBalanced(arg) {
				s = fmt.Sprintf("append(%s, %s)", before, strings.TrimSpace(arg))
			}
		}

		// ToUnixTimeMilliseconds() -> now()
		if strings.Contains(s, ".ToUnixTimeMilliseconds()") {
			s = strings.ReplaceAll(s, "DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()", "now()")
			s = strings.ReplaceAll(s, ".ToUnixTimeMilliseconds()", "now()")
		}
		if strings.HasPrefix(s, "ToUnixTimeMilliseconds(") && strings.HasSuffix(s, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(s, "ToUnixTimeMilliseconds("), ")")
			if strings.TrimSpace(inner) == "DateTimeOffset.UtcNow" {
				s = "now()"
			}
		}

		// ternary operator -> if
		if q := strings.Index(s, "?"); q != -1 {
			if c := strings.Index(s[q+1:], ":"); c != -1 {
				cond := stripOuterParens(strings.TrimSpace(s[:q]))
				thenPart := stripOuterParens(strings.TrimSpace(s[q+1 : q+1+c]))
				elsePart := stripOuterParens(strings.TrimSpace(s[q+1+c+1:]))
				s = fmt.Sprintf("if %s { %s } else { %s }", cond, thenPart, elsePart)
			}
		}

		s = strings.TrimSpace(s)
		s = stripOuterParens(s)
		if s == orig {
			break
		}
	}
	return s
}

// Transform converts the parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	return programToNode(p)
}

func programToNode(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}
	root := &ast.Node{Kind: "program"}
	for _, t := range p.Types {
		if strings.EqualFold(t.Name, "Program") {
			var otherMethods []Func
			otherFields := make([]Field, 0)
			for _, f := range t.Fields {
				if f.Static && (f.Ast != nil || f.Value != "") {
					var val *ast.Node
					if f.Ast != nil {
						val = exprNodeFromAST(f.Ast)
					} else if v, err := exprNode(f.Value); err == nil {
						val = v
					}
					if val != nil {
						n := &ast.Node{Kind: "let", Value: f.Name}
						if ft := mapType(f.Type); ft != "" {
							n.Children = append(n.Children, &ast.Node{Kind: "type", Value: ft})
						}
						n.Children = append(n.Children, val)
						root.Children = append(root.Children, n)
						continue
					}
				}
				if !f.Static {
					otherFields = append(otherFields, f)
				}
			}
			t.Fields = otherFields
			for _, m := range t.Methods {
				if strings.EqualFold(m.Name, "Main") && m.Static {
					stmts, err := blockToNodes(m.Ast)
					if err != nil {
						return nil, err
					}
					root.Children = append(root.Children, stmts...)
				} else if m.Static {
					if fn, err := funcToNode(&m); err == nil {
						root.Children = append(root.Children, fn)
					}
				} else {
					otherMethods = append(otherMethods, m)
				}
			}
			if len(t.Fields) > 0 || len(otherMethods) > 0 {
				t.Methods = otherMethods
				root.Children = append(root.Children, typeToNode(&t))
			}
			continue
		}
		root.Children = append(root.Children, typeToNode(&t))
	}
	if len(root.Children) == 0 {
		return nil, fmt.Errorf("no convertible symbols found")
	}
	return root, nil
}

// --- Implementation below mostly ported from archived any2mochi ---

func typeToNode(t *Type) *ast.Node {
	n := &ast.Node{Kind: "type", Value: t.Name}
	for _, f := range t.Fields {
		fn := &ast.Node{Kind: "field", Value: f.Name}
		if ft := mapType(f.Type); ft != "" {
			fn.Children = append(fn.Children, &ast.Node{Kind: "type", Value: ft})
		}
		n.Children = append(n.Children, fn)
	}
	for _, m := range t.Methods {
		if mn, err := funcToNode(&m); err == nil {
			n.Children = append(n.Children, mn)
		}
	}
	return n
}

func funcToNode(f *Func) (*ast.Node, error) {
	n := &ast.Node{Kind: "fun", Value: f.Name}
	for _, p := range f.Params {
		pn := &ast.Node{Kind: "param", Value: p.Name}
		if pt := mapType(p.Type); pt != "" {
			pn.Children = append(pn.Children, &ast.Node{Kind: "type", Value: pt})
		}
		n.Children = append(n.Children, pn)
	}
	if rt := mapType(f.Ret); rt != "" {
		n.Children = append(n.Children, &ast.Node{Kind: "type", Value: rt})
	}
	body, err := blockToNodes(f.Ast)
	if err != nil {
		return nil, err
	}
	n.Children = append(n.Children, body...)
	return n, nil
}

func blockToNodes(block *Node) ([]*ast.Node, error) {
	if block == nil {
		return nil, nil
	}
	var out []*ast.Node
	for _, st := range block.Children {
		if n := stmtNode(st); n != nil {
			out = append(out, n)
		}
	}
	return out, nil
}

func stmtNode(n *Node) *ast.Node {
	switch n.Kind {
	case "var":
		v := &ast.Node{Kind: "var", Value: n.Value}
		if len(n.Children) > 0 {
			v.Children = append(v.Children, exprNodeFromAST(n.Children[0]))
		}
		return v
	case "assign":
		a := &ast.Node{Kind: "assign"}
		if len(n.Children) == 2 {
			a.Children = append(a.Children, exprNodeFromAST(n.Children[0]))
			a.Children = append(a.Children, exprNodeFromAST(n.Children[1]))
		} else if len(n.Children) == 1 {
			// fallback for older AST format where left side was stored in Value
			lhs, err := exprNode(n.Value)
			if err == nil {
				a.Children = append(a.Children, lhs)
			}
			a.Children = append(a.Children, exprNodeFromAST(n.Children[0]))
		}
		return a
	case "call":
		c := &ast.Node{Kind: "call", Value: n.Value}
		for _, ch := range n.Children {
			c.Children = append(c.Children, exprNodeFromAST(ch))
		}
		if c.Value == "print" && len(c.Children) == 1 && c.Children[0].Kind == "list" {
			c.Children = c.Children[0].Children
		}
		return c
	case "for":
		if len(n.Children) >= 2 {
			rng := rangeNode(n.Children[0])
			body := &ast.Node{Kind: "block"}
			for _, s := range n.Children[1].Children {
				body.Children = append(body.Children, stmtNode(s))
			}
			return &ast.Node{Kind: "for", Value: n.Value, Children: []*ast.Node{rng, body}}
		}
	case "while":
		if len(n.Children) == 2 {
			return &ast.Node{Kind: "while", Children: []*ast.Node{exprNodeFromAST(n.Children[0]), blockNode(n.Children[1])}}
		}
	case "if":
		cond := exprNodeFromAST(n.Children[0])
		thenBlk := blockNode(n.Children[1])
		out := &ast.Node{Kind: "if", Children: []*ast.Node{cond, thenBlk}}
		if len(n.Children) > 2 {
			out.Children = append(out.Children, blockNode(n.Children[2]))
		}
		return out
	case "return":
		r := &ast.Node{Kind: "return"}
		if len(n.Children) > 0 {
			r.Children = append(r.Children, exprNodeFromAST(n.Children[0]))
		}
		return r
	case "break":
		return &ast.Node{Kind: "break"}
	case "continue":
		return &ast.Node{Kind: "continue"}
	case "empty":
		return nil
	case "block":
		return blockNode(n)
	}
	return &ast.Node{Kind: "unknown"}
}

func rangeNode(n *Node) *ast.Node {
	if n == nil || n.Kind != "range" {
		return &ast.Node{Kind: "range"}
	}
	if len(n.Children) == 1 {
		return &ast.Node{Kind: "range", Children: []*ast.Node{exprNodeFromAST(n.Children[0])}}
	}
	if len(n.Children) >= 2 {
		return &ast.Node{Kind: "range", Children: []*ast.Node{exprNodeFromAST(n.Children[0]), exprNodeFromAST(n.Children[1])}}
	}
	return &ast.Node{Kind: "range"}
}

func blockNode(n *Node) *ast.Node {
	blk := &ast.Node{Kind: "block"}
	if n != nil {
		for _, c := range n.Children {
			blk.Children = append(blk.Children, stmtNode(c))
		}
	}
	return blk
}

func exprNodeFromAST(n *Node) *ast.Node {
	if n == nil {
		return &ast.Node{Kind: "nil"}
	}
	switch n.Kind {
	case "literal":
		if i, err := strconv.Atoi(n.Value); err == nil {
			return &ast.Node{Kind: "int", Value: i}
		}
		if f, err := strconv.ParseFloat(n.Value, 64); err == nil {
			return &ast.Node{Kind: "float", Value: f}
		}
		if n.Value == "true" || n.Value == "false" {
			return &ast.Node{Kind: "bool", Value: n.Value == "true"}
		}
		return &ast.Node{Kind: "string", Value: n.Value}
	case "ident":
		return &ast.Node{Kind: "selector", Value: n.Value}
	case "binary":
		if n.Value == "+" && len(n.Children) == 2 {
			l := n.Children[0]
			r := n.Children[1]
			if l.Kind == "literal" && l.Value == "[" && r.Kind == "binary" && r.Value == "+" && len(r.Children) == 2 {
				if rc := r.Children[1]; rc.Kind == "literal" && rc.Value == "]" {
					return exprNodeFromAST(r.Children[0])
				}
			}
		}
		return &ast.Node{Kind: "binary", Value: n.Value, Children: []*ast.Node{exprNodeFromAST(n.Children[0]), exprNodeFromAST(n.Children[1])}}
	case "unary":
		return &ast.Node{Kind: "unary", Value: n.Value, Children: []*ast.Node{exprNodeFromAST(n.Children[0])}}
	case "call":
		if n.Value == "TrimEnd" && len(n.Children) == 1 {
			return exprNodeFromAST(n.Children[0])
		}
		if n.Value == "ToArray" && len(n.Children) == 1 && n.Children[0].Kind == "call" && n.Children[0].Value == "Append" && len(n.Children[0].Children) == 2 {
			return &ast.Node{Kind: "call", Value: "append", Children: []*ast.Node{exprNodeFromAST(n.Children[0].Children[0]), exprNodeFromAST(n.Children[0].Children[1])}}
		}
		if n.Value == "ToInt32" && len(n.Children) == 2 && n.Children[0].Kind == "ident" && n.Children[0].Value == "Convert" {
			return &ast.Node{Kind: "cast", Children: []*ast.Node{exprNodeFromAST(n.Children[1]), &ast.Node{Kind: "type", Value: "int"}}}
		}
		if n.Value == "Join" && len(n.Children) >= 3 {
			if n.Children[2].Kind == "array" {
				lst := &ast.Node{Kind: "list"}
				for _, a := range n.Children[2].Children {
					lst.Children = append(lst.Children, exprNodeFromAST(a))
				}
				return lst
			}
			return &ast.Node{Kind: "call", Value: "str", Children: []*ast.Node{exprNodeFromAST(n.Children[len(n.Children)-1])}}
		}
		c := &ast.Node{Kind: "call", Value: n.Value}
		for _, ch := range n.Children {
			c.Children = append(c.Children, exprNodeFromAST(ch))
		}
		return c
	case "array":
		arr := &ast.Node{Kind: "list"}
		for _, ch := range n.Children {
			arr.Children = append(arr.Children, exprNodeFromAST(ch))
		}
		return arr
	case "new":
		if len(n.Children) > 0 && n.Children[0].Kind == "init" {
			init := n.Children[0]
			// handle dictionary and struct in initializer
			if strings.HasPrefix(n.Value, "Dictionary") || strings.HasPrefix(n.Value, "SortedDictionary") {
				m := &ast.Node{Kind: "map"}
				for _, p := range init.Children {
					if len(p.Children) == 2 {
						m.Children = append(m.Children, &ast.Node{Kind: "pair", Children: []*ast.Node{exprNodeFromAST(p.Children[0]), exprNodeFromAST(p.Children[1])}})
					}
				}
				return m
			}
			s := &ast.Node{Kind: "struct"}
			if n.Value != "" {
				s.Value = n.Value
			}
			for _, f := range init.Children {
				if f.Kind == "field" && len(f.Children) == 1 {
					s.Children = append(s.Children, &ast.Node{Kind: "field", Value: f.Value, Children: []*ast.Node{exprNodeFromAST(f.Children[0])}})
				}
			}
			return s
		}
		return &ast.Node{Kind: "unknown"}
	case "index":
		if len(n.Children) == 2 {
			return &ast.Node{Kind: "index", Children: []*ast.Node{exprNodeFromAST(n.Children[0]), exprNodeFromAST(n.Children[1])}}
		}
		idx := &ast.Node{Kind: "index"}
		for _, ch := range n.Children {
			idx.Children = append(idx.Children, exprNodeFromAST(ch))
		}
		return idx
	case "member":
		if n.Value == "Keys" && len(n.Children) > 0 {
			return exprNodeFromAST(n.Children[0])
		}
		if n.Value == "Length" && len(n.Children) > 0 {
			return &ast.Node{Kind: "call", Value: "len", Children: []*ast.Node{exprNodeFromAST(n.Children[0])}}
		}
		if len(n.Children) > 0 {
			return &ast.Node{Kind: "selector", Value: n.Value, Children: []*ast.Node{exprNodeFromAST(n.Children[0])}}
		}
		return &ast.Node{Kind: "selector", Value: n.Value}
	case "cond":
		return &ast.Node{Kind: "if_expr", Children: []*ast.Node{exprNodeFromAST(n.Children[0]), exprNodeFromAST(n.Children[1]), exprNodeFromAST(n.Children[2])}}
	}
	return &ast.Node{Kind: "unknown"}
}

func exprNode(expr string) (*ast.Node, error) {
	expr = rewriteExpr(expr)
	src := "return " + expr + "\n"
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Return == nil {
		return nil, fmt.Errorf("expr parse failed")
	}
	ret := ast.FromStatement(prog.Statements[0])
	if len(ret.Children) > 0 {
		return ret.Children[0], nil
	}
	return nil, fmt.Errorf("expr parse failed")
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	for strings.HasSuffix(t, "[]") {
		inner := mapType(strings.TrimSuffix(t, "[]"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if idx := strings.LastIndex(t, "."); idx != -1 {
		t = t[idx+1:]
	}
	switch t {
	case "void", "":
		return ""
	case "int", "long", "short", "uint", "ulong", "ushort", "byte", "sbyte":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "string", "char":
		return "string"
	case "bool":
		return "bool"
	case "dynamic", "object":
		return "any"
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := t[:open]
			inner := t[open+1 : len(t)-1]
			args := splitArgs(inner)
			switch outer {
			case "List", "IEnumerable", "IList", "ICollection", "IReadOnlyList":
				a := "any"
				if len(args) > 0 {
					if at := mapType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Dictionary", "IDictionary":
				if len(args) == 2 {
					k := mapType(args[0])
					if k == "" {
						k = "any"
					}
					v := mapType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Nullable":
				if len(args) == 1 {
					return mapType(args[0])
				}
			}
		}
	}
	return ""
}

func splitArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '[', '(', '<':
			depth++
		case ']', ')', '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}
