//go:build slow

package cs

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var longLit = regexp.MustCompile(`(\d+)L\b`)

func stripLong(s string) string { return longLit.ReplaceAllString(s, "$1") }

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

		// string.Join -> str()
		if strings.HasPrefix(s, "string.Join(") {
			inner := strings.TrimPrefix(s, "string.Join(")
			if end := strings.LastIndex(inner, ")"); end != -1 {
				inner = inner[:end]
				if strings.HasPrefix(inner, "\"") {
					if i := strings.Index(inner[1:], "\""); i != -1 {
						inner = inner[i+2:]
					}
				}
				if idx := strings.Index(inner, ","); idx != -1 {
					inner = strings.TrimSpace(inner[idx+1:])
				}
				s = "str(" + inner + ")"
			}
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
				if f.Static && f.Value != "" {
					if v, err := exprNode(f.Value); err == nil {
						n := &ast.Node{Kind: "let", Value: f.Name}
						if ft := mapType(f.Type); ft != "" {
							n.Children = append(n.Children, &ast.Node{Kind: "type", Value: ft})
						}
						n.Children = append(n.Children, v)
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
					stmts, err := bodyToNodes(m.Body)
					if err != nil {
						return nil, err
					}
					root.Children = append(root.Children, stmts...)
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
	body, err := bodyToNodes(f.Body)
	if err != nil {
		return nil, err
	}
	n.Children = append(n.Children, body...)
	return n, nil
}

func bodyToNodes(body []string) ([]*ast.Node, error) {
	lines := convertBodyLines(body)
	if len(lines) == 0 {
		return nil, nil
	}
	src := strings.Join(lines, "\n") + "\n"
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	nodes := make([]*ast.Node, len(prog.Statements))
	for i, st := range prog.Statements {
		nodes[i] = ast.FromStatement(st)
	}
	return nodes, nil
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

func convertBodyLines(body []string) []string {
	var out []string
	for _, ln := range body {
		l := strings.TrimSpace(ln)
		if l == "" {
			continue
		}
		if strings.HasSuffix(l, ";") {
			l = strings.TrimSuffix(l, ";")
		}
		l = stripLong(l)
		switch {
		case strings.HasPrefix(l, "Console.WriteLine("):
			inner := strings.TrimPrefix(l, "Console.WriteLine(")
			inner = strings.TrimSuffix(inner, ")")
			inner = rewriteExpr(inner)
			l = "print(" + inner + ")"
		case strings.HasPrefix(l, "return "):
			l = "return " + strings.TrimSpace(strings.TrimPrefix(l, "return "))
		case strings.HasPrefix(l, "foreach ("):
			inner := strings.TrimPrefix(l, "foreach (")
			inner = strings.TrimSuffix(inner, ") {")
			parts := strings.SplitN(inner, " in ", 2)
			if len(parts) == 2 {
				varName := strings.TrimSpace(parts[0])
				fs := strings.Fields(varName)
				if len(fs) > 1 {
					varName = fs[len(fs)-1]
				}
				iter := strings.TrimSpace(parts[1])
				l = fmt.Sprintf("for %s in %s {", varName, iter)
			}
		case strings.HasPrefix(l, "for (") && strings.Contains(l, ";") && strings.Contains(l, ")"):
			l = strings.TrimPrefix(l, "for (")
			l = strings.TrimSuffix(l, ") {")
			parts := strings.Split(l, ";")
			if len(parts) >= 2 {
				init := strings.TrimSpace(parts[0])
				cond := strings.TrimSpace(parts[1])
				if strings.HasPrefix(init, "var ") {
					init = strings.TrimPrefix(init, "var ")
				}
				if eq := strings.Index(init, "="); eq != -1 {
					name := strings.TrimSpace(init[:eq])
					startVal := strings.TrimSpace(init[eq+1:])
					startVal = stripLong(startVal)
					endVal := ""
					if idx := strings.Index(cond, "<"); idx != -1 {
						endVal = strings.TrimSpace(cond[idx+1:])
						endVal = stripLong(endVal)
					}
					l = fmt.Sprintf("for %s in %s..%s {", name, startVal, endVal)
				}
			}
		case strings.HasPrefix(l, "while ("):
			l = strings.TrimPrefix(l, "while (")
			l = strings.TrimSpace(strings.TrimSuffix(l, "{"))
			if strings.HasSuffix(l, ")") {
				idx := strings.LastIndex(l, ")")
				l = l[:idx]
			}
			l = strings.TrimSpace(l)
			l = stripLong(l)
			l = "while " + l + " {"
		case strings.HasPrefix(l, "if ("):
			l = strings.TrimPrefix(l, "if (")
			l = strings.TrimSuffix(l, ") {")
			l = stripLong(l)
			l = "if " + l + " {"
		case l == "}" || l == "} else {":
			// keep as is
		default:
			if idx := strings.Index(l, "="); idx != -1 {
				before := strings.TrimSpace(l[:idx])
				rest := strings.TrimSpace(l[idx+1:])
				parts := strings.Fields(before)
				if len(parts) >= 2 {
					name := parts[len(parts)-1]
					l = "var " + name + " = " + rest
				} else {
					l = stripLong(l)
				}
			} else {
				for _, t := range []string{"long ", "int ", "float ", "double ", "string ", "bool "} {
					if strings.HasPrefix(l, t) {
						l = strings.TrimPrefix(l, t)
						if strings.HasPrefix(t, "string") {
							l = "var " + l
						} else {
							l = "var " + stripLong(l)
						}
						break
					}
				}
				l = stripLong(l)
			}
		}
		l = rewriteExpr(l)
		out = append(out, l)
	}
	return out
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
