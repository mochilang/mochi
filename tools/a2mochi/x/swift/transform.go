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
	end := r.End + 1
	if end < r.Start {
		end = r.Start
	}
	if end > len(src) {
		end = len(src)
	}
	for end < len(src) && src[end-1] != '\n' {
		if src[end] == '\n' {
			end++
			break
		}
		end++
	}
	if end > len(src) {
		end = len(src)
	}
	return src[r.Start:end]
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
			l = rewriteAvg(l)
			l = rewriteSum(l)
			l = rewriteMapLiteral(l)
			l = rewriteStructLiteral(l)
			l = rewriteCasts(l)
			l = rewriteCount(l)
			l = rewriteMinMax(l)
			l = rewriteRanges(l)
			l = rewriteStrBuiltin(l)
			l = rewriteTernary(l)
			l = rewriteStringArrays(l)
			l = rewriteContains(l)
			l = rewriteMapContains(l)
			l = strings.ReplaceAll(l, ")!", ")")
			l = strings.ReplaceAll(l, "]!", "]")
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
				expr := strings.TrimSpace(l[len("return "):])
				expr = rewriteClosure(expr)
				expr = rewriteStructLiteral(expr)
				expr = rewriteMapLiteral(expr)
				expr = rewriteCasts(expr)
				expr = rewriteAvg(expr)
				expr = rewriteSum(expr)
				expr = rewriteCount(expr)
				expr = rewriteMinMax(expr)
				expr = rewriteRanges(expr)
				expr = rewriteStrBuiltin(expr)
				expr = rewriteTernary(expr)
				expr = rewriteStringArrays(expr)
				expr = rewriteContains(expr)
				expr = rewriteMapContains(expr)
				out = append(out, strings.Repeat("  ", indent)+"return "+expr)
			case strings.HasPrefix(l, "print(") && strings.HasSuffix(l, ")"):
				expr := strings.TrimSpace(l[len("print(") : len(l)-1])
				expr = rewriteClosure(expr)
				expr = rewriteAppendExpr(expr)
				expr = rewriteStructLiteral(expr)
				expr = rewriteMapLiteral(expr)
				expr = rewriteCasts(expr)
				expr = rewriteAvg(expr)
				expr = rewriteSum(expr)
				expr = rewriteCount(expr)
				expr = rewriteMinMax(expr)
				expr = rewriteRanges(expr)
				expr = rewriteStrBuiltin(expr)
				expr = rewriteTernary(expr)
				expr = rewriteStringArrays(expr)
				expr = rewriteContains(expr)
				expr = rewriteMapContains(expr)
				out = append(out, strings.Repeat("  ", indent)+"print("+expr+")")
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
					expr := rewriteClosure(strings.TrimSpace(parts[1]))
					expr = rewriteAppendExpr(expr)
					expr = rewriteStructLiteral(expr)
					expr = rewriteMapLiteral(expr)
					expr = rewriteCasts(expr)
					expr = rewriteAvg(expr)
					expr = rewriteSum(expr)
					expr = rewriteCount(expr)
					expr = rewriteMinMax(expr)
					expr = rewriteRanges(expr)
					expr = rewriteStrBuiltin(expr)
					expr = rewriteTernary(expr)
					expr = rewriteStringArrays(expr)
					expr = rewriteContains(expr)
					expr = rewriteMapContains(expr)
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
var stringSliceRE = regexp.MustCompile(`^\(?\s*(?:str|String)\(Array\((.+?)\)\[\s*([^\]]+?)\s*\.\.\s*([^\]]+?)\s*\]\)\s*\)?$`)
var stringIndexRE = regexp.MustCompile(`^\(?\s*(?:str|String)\(Array\((.+?)\)\[\s*([^\]]+?)\s*\]\)\s*\)?$`)
var containsRE = regexp.MustCompile(`([A-Za-z0-9_]+)\.contains\(([^()]+)\)`)
var mapContainsRE = regexp.MustCompile(`([A-Za-z0-9_]+)\[([^\]]+)\]\s*!=\s*nil`)

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

var ternaryBoolRE = regexp.MustCompile(`\(([^()]+)\)\s*\?\s*1\s*:\s*0`)

func rewriteTernary(expr string) string {
	return ternaryBoolRE.ReplaceAllString(expr, "if $1 then true else false")
}

func rewriteStringArrays(expr string) string {
	if m := stringSliceRE.FindStringSubmatch(expr); m != nil {
		return "substring(" + strings.TrimSpace(m[1]) + ", " + strings.TrimSpace(m[2]) + ", " + strings.TrimSpace(m[3]) + ")"
	}
	if m := stringIndexRE.FindStringSubmatch(expr); m != nil {
		idx := strings.TrimSpace(m[2])
		return "substring(" + strings.TrimSpace(m[1]) + ", " + idx + ", " + idx + " + 1)"
	}
	return expr
}

func rewriteContains(expr string) string {
	return containsRE.ReplaceAllString(expr, "$2 in $1")
}

func rewriteMapContains(expr string) string {
	return mapContainsRE.ReplaceAllString(expr, "$2 in $1")
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

var appendExprRE = regexp.MustCompile(`"\[" \+ \(([^+]+) \+ \[([^\]]+)\]\)\.map\{\s*String\(describing: \$0\)\s*\}\.joined\(separator: ","\) \+ "\]"`)
var sumRe = regexp.MustCompile(`\(?([^()]*)\)?\.reduce\(0,\+\)`)
var avgRe = regexp.MustCompile(`Double\(\(?([^()]*)\)?\.reduce\(0,\+\)\)\s*/\s*Double\(\(?([^()]*)\)?\.count\)`)
var closureRe = regexp.MustCompile(`^\{\s*\(([^)]*)\)\s*->\s*([A-Za-z0-9_<>.?]+)\s*in\s*(.+)\s*\}$`)

func rewriteAppendExpr(expr string) string {
	m := appendExprRE.FindStringSubmatch(expr)
	if len(m) == 3 {
		return fmt.Sprintf("append(%s, %s)", strings.TrimSpace(m[1]), strings.TrimSpace(m[2]))
	}
	return expr
}

func rewriteSum(expr string) string {
	if m := sumRe.FindStringSubmatch(expr); m != nil {
		return "sum(" + strings.TrimSpace(m[1]) + ")"
	}
	return expr
}

func rewriteAvg(expr string) string {
	if m := avgRe.FindStringSubmatch(expr); m != nil {
		return "avg(" + strings.TrimSpace(m[1]) + ")"
	}
	return expr
}

func rewriteClosure(expr string) string {
	m := closureRe.FindStringSubmatch(expr)
	if len(m) != 4 {
		return expr
	}
	params := []string{}
	for _, p := range strings.Split(strings.TrimSpace(m[1]), ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		parts := strings.SplitN(p, ":", 2)
		name := strings.TrimSpace(parts[0])
		typ := ""
		if len(parts) == 2 {
			typ = interfaceTypeToMochi(strings.TrimSpace(parts[1]))
		}
		if typ != "" {
			params = append(params, name+": "+typ)
		} else {
			params = append(params, name)
		}
	}
	body := strings.TrimSpace(m[3])
	body = rewriteStructLiteral(body)
	body = rewriteMapLiteral(body)
	body = rewriteCasts(body)
	body = rewriteAvg(body)
	body = rewriteSum(body)
	body = rewriteCount(body)
	body = rewriteMinMax(body)
	body = rewriteRanges(body)
	body = rewriteStrBuiltin(body)
	body = rewriteTernary(body)
	body = rewriteStringArrays(body)
	body = rewriteContains(body)
	body = rewriteMapContains(body)
	ret := interfaceTypeToMochi(m[2])
	if ret == "" {
		ret = "any"
	}
	return fmt.Sprintf("fun(%s): %s { %s }", strings.Join(params, ", "), ret, body)
}
