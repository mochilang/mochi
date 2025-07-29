package swift

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	var out strings.Builder
	src := p.Src
	for _, it := range p.Items {
		switch it.Kind {
		case "func_decl":
			if it.Name != nil && strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
			out.WriteString("fun ")
			if it.Name != nil {
				out.WriteString(it.Name.BaseName.Name)
			}
			out.WriteByte('(')
			if it.Params != nil {
				for i, pr := range it.Params.Params {
					if i > 0 {
						out.WriteString(", ")
					}
					out.WriteString(pr.Name.BaseName.Name)
					if t := interfaceTypeToMochi(pr.InterfaceType); t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
				}
			}
			out.WriteByte(')')
			if t := interfaceTypeToMochi(it.Result); t != "" {
				out.WriteString(": ")
				out.WriteString(t)
			}
			if it.Body != nil {
				body := bodyFromRange(src, it.Body.Range)
				if len(body) == 0 {
					out.WriteString(" {}\n")
				} else {
					out.WriteString(" {\n")
					for _, st := range body {
						out.WriteString("  ")
						out.WriteString(st)
						out.WriteByte('\n')
					}
					out.WriteString("}\n")
				}
			} else {
				out.WriteString(" {}\n")
			}
		case "struct_decl", "class_decl":
			if it.Name == nil {
				continue
			}
			if strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
			out.WriteString("type ")
			out.WriteString(it.Name.BaseName.Name)
			out.WriteString(" {\n")
			for _, m := range it.Members {
				if m.Kind != "var_decl" || m.Name == nil {
					continue
				}
				out.WriteString("  ")
				out.WriteString(m.Name.BaseName.Name)
				if t := interfaceTypeToMochi(m.InterfaceType); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "enum_decl":
			if it.Name == nil {
				continue
			}
			if strings.HasPrefix(it.Name.BaseName.Name, "_") {
				continue
			}
			out.WriteString("type ")
			out.WriteString(it.Name.BaseName.Name)
			out.WriteString(" =\n")
			els := gatherEnumElements(it.Members)
			for i, el := range els {
				if i > 0 {
					out.WriteString("  | ")
				} else {
					out.WriteString("  ")
				}
				out.WriteString(el.Name.BaseName.Name)
				if el.Params != nil && len(el.Params.Params) > 0 {
					out.WriteByte('(')
					for j, p := range el.Params.Params {
						if j > 0 {
							out.WriteString(", ")
						}
						out.WriteString(p.Name.BaseName.Name)
						if t := interfaceTypeToMochi(p.InterfaceType); t != "" {
							out.WriteString(": ")
							out.WriteString(t)
						}
					}
					out.WriteByte(')')
				}
				out.WriteByte('\n')
			}
		case "top_level_code_decl":
			code := extractRange(src, it.Range)
			for _, st := range parseStatementsIndent(code, 0) {
				out.WriteString(st)
				out.WriteByte('\n')
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	prog, err := parser.ParseString(out.String())
	if err != nil {
		return nil, err
	}
	return buildNode(prog), nil
}

// TransformFile parses and converts a Swift file.
func TransformFile(path string) (*ast.Node, error) {
	p, err := ParseFile(path)
	if err != nil {
		return nil, err
	}
	return Transform(p)
}

func buildNode(p *parser.Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	if p.Package != "" {
		n.Value = p.Package
	}
	for _, st := range p.Statements {
		n.Children = append(n.Children, ast.FromStatement(st))
	}
	return n
}

func interfaceTypeToMochi(t string) string {
	if strings.HasPrefix(t, "$sS") && strings.HasSuffix(t, "D") {
		mid := strings.TrimSuffix(strings.TrimPrefix(t, "$sS"), "D")
		switch mid {
		case "i":
			return "int"
		case "d":
			return "float"
		case "b":
			return "bool"
		case "S":
			return "string"
		}
	}
	return ""
}

func extractRange(src string, r offsetRange) string {
	if r.Start < 0 {
		r.Start = 0
	}
	end := r.End
	if end < r.Start {
		end = r.Start
	}
	if end+1 > len(src) {
		end = len(src) - 1
	}
	return src[r.Start : end+1]
}

func bodyFromRange(src string, r offsetRange) []string {
	text := extractRange(src, r)
	start := strings.Index(text, "{")
	end := strings.LastIndex(text, "}")
	if start == -1 || end == -1 || end <= start {
		return nil
	}
	return parseStatementsIndent(text[start+1:end], 1)
}

func parseStatements(body string) []string { return parseStatementsIndent(body, 1) }

func parseStatementsIndent(body string, indent int) []string {
	lines := strings.Split(body, "\n")
	var out []string
	for _, line := range lines {
		for _, part := range strings.Split(line, ";") {
			l := strings.TrimSpace(part)
			if l == "" {
				continue
			}
			l = strings.TrimSuffix(l, ";")
			l = rewriteMapLiteral(l)
			l = rewriteStructLiteral(l)
			l = rewriteCasts(l)
			l = rewriteCount(l)
			l = rewriteMinMax(l)
			l = rewriteRanges(l)
			l = rewriteStrBuiltin(l)
			l = strings.ReplaceAll(l, ")!", ")")
			l = strings.ReplaceAll(l, "_append(", "append(")
			l = strings.ReplaceAll(l, "_values(", "values(")
			l = strings.ReplaceAll(l, "_exists(", "exists(")
			l = strings.ReplaceAll(l, "_sliceString(", "substring(")
			switch {
			case l == "}":
				if indent > 0 {
					indent--
				}
				out = append(out, strings.Repeat("  ", indent)+"}")
			case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, "{"):
				cond := strings.TrimSpace(strings.TrimSuffix(l[3:], "{"))
				out = append(out, strings.Repeat("  ", indent)+"if "+cond+" {")
				indent++
			case l == "else {":
				if indent > 0 {
					indent--
				}
				out = append(out, strings.Repeat("  ", indent)+"else {")
				indent++
			case strings.HasPrefix(l, "for ") && strings.HasSuffix(l, "{"):
				head := strings.TrimSpace(strings.TrimSuffix(l[4:], "{"))
				out = append(out, strings.Repeat("  ", indent)+"for "+head+" {")
				indent++
			case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, "{"):
				cond := strings.TrimSpace(strings.TrimSuffix(l[len("while "):], "{"))
				out = append(out, strings.Repeat("  ", indent)+"while "+cond+" {")
				indent++
			case strings.HasPrefix(l, "return "):
				expr := rewriteStructLiteral(strings.TrimSpace(l[len("return "):]))
				out = append(out, strings.Repeat("  ", indent)+"return "+expr)
			case strings.HasPrefix(l, "let ") || strings.HasPrefix(l, "var "):
				keyword := "let"
				decl := strings.TrimPrefix(l, "let ")
				if decl == l {
					keyword = "var"
					decl = strings.TrimPrefix(l, "var ")
				}
				parts := strings.SplitN(decl, "=", 2)
				if len(parts) == 2 {
					name := strings.TrimSpace(parts[0])
					if colon := strings.Index(name, ":"); colon != -1 {
						name = strings.TrimSpace(name[:colon])
					}
					expr := rewriteStructLiteral(strings.TrimSpace(parts[1]))
					out = append(out, strings.Repeat("  ", indent)+keyword+" "+name+" = "+expr)
				}
			case l == "break":
				out = append(out, strings.Repeat("  ", indent)+"break")
			case l == "continue":
				out = append(out, strings.Repeat("  ", indent)+"continue")
			default:
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		}
	}
	return out
}

var structLitRE = regexp.MustCompile(`^([A-Z][A-Za-z0-9_]*)\((.*)\)$`)
var mapLitRE = regexp.MustCompile(`^\[(.*:.+)\]$`)
var countPropRE = regexp.MustCompile(`\(([^()]+)\)\.count\b`)
var identCountRE = regexp.MustCompile(`([A-Za-z0-9_]+)\.count\b`)
var parenMinRE = regexp.MustCompile(`\(([^()]+)\)\.min\(\)`)
var identMinRE = regexp.MustCompile(`([A-Za-z0-9_]+)\.min\(\)`)
var parenMaxRE = regexp.MustCompile(`\(([^()]+)\)\.max\(\)`)
var identMaxRE = regexp.MustCompile(`([A-Za-z0-9_]+)\.max\(\)`)
var castRE = regexp.MustCompile(`\((.+?)\s+as!\s+[A-Za-z0-9_<>.]+\)`)

func rewriteMapLiteral(expr string) string {
	m := mapLitRE.FindStringSubmatch(expr)
	if len(m) != 2 {
		return expr
	}
	inner := strings.TrimSpace(m[1])
	return "{ " + inner + " }"
}

func rewriteStructLiteral(expr string) string {
	m := structLitRE.FindStringSubmatch(expr)
	if len(m) != 3 || !strings.Contains(m[2], ":") {
		return expr
	}
	name := m[1]
	parts := strings.Split(m[2], ",")
	var fields []string
	for _, p := range parts {
		kv := strings.SplitN(strings.TrimSpace(p), ":", 2)
		if len(kv) != 2 {
			return expr
		}
		fields = append(fields, fmt.Sprintf("%s: %s", strings.TrimSpace(kv[0]), strings.TrimSpace(kv[1])))
	}
	return name + " { " + strings.Join(fields, ", ") + " }"
}

func rewriteCasts(expr string) string {
	for {
		m := castRE.FindStringSubmatchIndex(expr)
		if m == nil {
			break
		}
		expr = expr[:m[0]] + expr[m[2]:m[3]] + expr[m[1]:]
	}
	return expr
}

func rewriteCount(expr string) string {
	for {
		if m := countPropRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "count(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		if m := identCountRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "count(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		break
	}
	return expr
}

func rewriteMinMax(expr string) string {
	for {
		if m := parenMinRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "min(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		if m := identMinRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "min(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		if m := parenMaxRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "max(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		if m := identMaxRE.FindStringSubmatchIndex(expr); m != nil {
			expr = expr[:m[0]] + "max(" + expr[m[2]:m[3]] + ")" + expr[m[1]:]
			continue
		}
		break
	}
	return expr
}

func rewriteRanges(expr string) string {
	return strings.ReplaceAll(expr, "..<", "..")
}

func rewriteStrBuiltin(expr string) string {
	return strings.ReplaceAll(expr, "String(", "str(")
}

func gatherEnumElements(ms []item) []item {
	var out []item
	for _, m := range ms {
		switch m.Kind {
		case "enum_case_decl":
			out = append(out, m.Elements...)
		case "enum_element_decl":
			out = append(out, m)
		}
	}
	return out
}
