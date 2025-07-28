//go:build slow

package cs

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

func stripLong(s string) string { return strings.ReplaceAll(s, "L", "") }

func rewriteExpr(s string) string {
	s = strings.TrimSpace(s)
	if strings.Contains(s, "string.Join(") {
		s = strings.Replace(s, "string.Join", "join", 1)
	}
	if idx := strings.Index(s, ".Append("); idx != -1 {
		end := strings.Index(s[idx:], ")")
		if end != -1 && strings.HasSuffix(s, ").ToArray()") {
			arg := s[idx+len(".Append(") : idx+end]
			target := s[:idx]
			s = fmt.Sprintf("append(%s, %s)", target, arg)
		}
	}
	if q := strings.Index(s, "?"); q != -1 {
		if c := strings.Index(s[q+1:], ":"); c != -1 {
			cond := strings.TrimSpace(s[:q])
			thenPart := strings.TrimSpace(s[q+1 : q+1+c])
			elsePart := strings.TrimSpace(s[q+1+c+1:])
			s = fmt.Sprintf("if %s { %s } else { %s }", cond, thenPart, elsePart)
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
			l = "print(" + strings.TrimPrefix(strings.TrimSuffix(l, ")"), "Console.WriteLine(") + ")"
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
