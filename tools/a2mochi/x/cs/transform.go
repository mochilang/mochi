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
	src, err := programToMochi(p)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return nodeFromProgram(prog), nil
}

func nodeFromProgram(prog *parser.Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	if prog.Package != "" {
		n.Value = prog.Package
	}
	for _, st := range prog.Statements {
		n.Children = append(n.Children, ast.FromStatement(st))
	}
	return n
}

// --- Implementation below mostly ported from archived any2mochi ---

// programToMochi converts a parsed Program into Mochi source code.
func programToMochi(p *Program) (string, error) {
	if p == nil {
		return "", fmt.Errorf("nil program")
	}
	var out strings.Builder
	for _, t := range p.Types {
		if t.Doc != "" {
			for _, ln := range strings.Split(t.Doc, "\n") {
				out.WriteString("# ")
				out.WriteString(strings.TrimSpace(ln))
				out.WriteByte('\n')
			}
		}
		out.WriteString("type ")
		out.WriteString(t.Name)
		out.WriteString(" {\n")
		for _, f := range t.Fields {
			if f.Doc != "" {
				for _, ln := range strings.Split(f.Doc, "\n") {
					out.WriteString("  # ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("  ")
			out.WriteString(f.Name)
			if ft := mapType(f.Type); ft != "" {
				out.WriteString(": ")
				out.WriteString(ft)
			}
			out.WriteByte('\n')
		}
		for _, fn := range t.Methods {
			if fn.Doc != "" {
				for _, ln := range strings.Split(fn.Doc, "\n") {
					out.WriteString("  # ")
					out.WriteString(strings.TrimSpace(ln))
					out.WriteByte('\n')
				}
			}
			out.WriteString("  fun ")
			out.WriteString(fn.Name)
			out.WriteByte('(')
			for i, p := range fn.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.Name)
				if pt := mapType(p.Type); pt != "" {
					out.WriteString(": ")
					out.WriteString(pt)
				}
			}
			out.WriteByte(')')
			if rt := mapType(fn.Ret); rt != "" {
				out.WriteString(": ")
				out.WriteString(rt)
			}
			body := convertBodyLines(fn.Body)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, b := range body {
					out.WriteString("    ")
					out.WriteString(b)
					out.WriteByte('\n')
				}
				out.WriteString("  }\n")
			}
		}
		out.WriteString("}\n")
	}
	if out.Len() == 0 {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return out.String(), nil
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
