//go:build slow

package zig

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// Transform converts a parsed Program into a Mochi AST node. The implementation
// reuses the lightweight translator that existed in earlier versions of the
// repository and covers a handful of simple constructs used in tests.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	code, err := translate(p.Src)
	if err != nil {
		return nil, err
	}

	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}

	return ast.FromProgram(prog), nil
}

// TransformFile reads the Zig file at path and converts it to a Mochi AST node.
func TransformFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	prog, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Transform(prog)
}

var headerRE = regexp.MustCompile(`^(?:pub\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*([^\{]*)\{`)
var printRE = regexp.MustCompile(`print\("\{[^}]+\}\\n",\s*\.\{(.*)\}\)`)

// declRE matches simple variable declarations with an optional type.
var declRE = regexp.MustCompile(`^(const|var)\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*:\s*([A-Za-z_][A-Za-z0-9_]*))?\s*=\s*(.*)$`)

// typedArrRE matches array literals like [_]i64{1, 2} or [2]i64{1, 2}
var typedArrRE = regexp.MustCompile(`^\[[^\]]*\]\w+\{(.*)\}$`)
var numberRE = regexp.MustCompile(`^\d+(?:\.\d+)?$`)
var orderRE = regexp.MustCompile(`std\.mem\.order\(u8,\s*([^,]+),\s*([^\)]+)\)\s*([!=]=)\s*\.(lt|gt)`)

func mapType(t string) string {
	switch strings.TrimSpace(t) {
	case "i64", "i32", "u64", "u32":
		return "int"
	case "f64", "f32":
		return "float"
	case "void", "":
		return ""
	default:
		return "any"
	}
}

func convertParams(params string) string {
	parts := strings.Split(strings.TrimSpace(params), ",")
	var out []string
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.SplitN(p, ":", 2)
		name := strings.TrimSpace(fields[0])
		typ := ""
		if len(fields) == 2 {
			typ = mapType(fields[1])
		}
		if typ != "" {
			out = append(out, fmt.Sprintf("%s: %s", name, typ))
		} else {
			out = append(out, name)
		}
	}
	return strings.Join(out, ", ")
}

func convertFunction(lines []string) string {
	first := strings.TrimSpace(lines[0])
	m := headerRE.FindStringSubmatch(first)
	if m == nil {
		return ""
	}
	name := m[1]
	params := convertParams(m[2])
	ret := mapType(strings.TrimSpace(m[3]))

	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(name)
	b.WriteByte('(')
	b.WriteString(params)
	b.WriteByte(')')
	if ret != "" {
		b.WriteString(": ")
		b.WriteString(ret)
	}
	b.WriteString(" {\n")

	for i := 1; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if i == len(lines)-1 && t == "}" {
			break
		}
		t = strings.TrimSuffix(t, ";")
		if m := declRE.FindStringSubmatch(t); m != nil {
			kw := m[1]
			name := m[2]
			typ := strings.TrimSpace(m[3])
			val := strings.TrimSpace(m[4])
			val = transformExpr(val)
			if typ != "" {
				if strings.HasPrefix(val, ".{") && strings.HasSuffix(val, "}") {
					inner := strings.TrimSuffix(strings.TrimPrefix(val, ".{"), "}")
					inner = strings.TrimSpace(inner)
					inner = strings.ReplaceAll(inner, " = ", ": ")
					inner = strings.ReplaceAll(inner, ".", "")
					val = typ + " {" + inner + "}"
				} else {
					// skip typed declarations not using struct literals
					continue
				}
			}
			decl := "let "
			if kw == "var" {
				decl = "var "
			}
			b.WriteString("  " + decl + name + " = " + val + "\n")
			continue
		}
		if p := extractPrint(t); p != "" {
			b.WriteString("  print(" + p + ")\n")
			continue
		}
		if strings.HasPrefix(t, "if (") && strings.HasSuffix(t, ") {") {
			cond := strings.TrimSuffix(strings.TrimPrefix(t, "if ("), ") {")
			b.WriteString("  if " + cond + " {\n")
			continue
		}
		if t == "}" {
			b.WriteString("  }\n")
			continue
		}
		b.WriteString("  ")
		b.WriteString(t)
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
	return b.String()
}

func convertLoop(lines []string) string {
	first := strings.TrimSpace(lines[0])
	open := strings.Index(first, "(")
	close := strings.Index(first, ")")
	if open < 0 || close < open {
		return ""
	}
	rng := strings.TrimSpace(first[open+1 : close])
	rest := first[close+1:]
	p1 := strings.Index(rest, "|")
	if p1 < 0 {
		return ""
	}
	p2 := strings.Index(rest[p1+1:], "|")
	if p2 < 0 {
		return ""
	}
	varName := strings.TrimSpace(rest[p1+1 : p1+1+p2])

	var b strings.Builder
	fmt.Fprintf(&b, "for %s in %s {\n", varName, rng)
	for i := 1; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if i == len(lines)-1 && t == "}" {
			break
		}
		if p := extractPrint(t); p != "" {
			b.WriteString("  print(" + p + ")\n")
			continue
		}
		t = strings.TrimSuffix(t, ";")
		if m := declRE.FindStringSubmatch(t); m != nil {
			kw := m[1]
			name := m[2]
			typ := strings.TrimSpace(m[3])
			val := strings.TrimSpace(m[4])
			val = transformExpr(val)
			if typ != "" {
				if strings.HasPrefix(val, ".{") && strings.HasSuffix(val, "}") {
					inner := strings.TrimSuffix(strings.TrimPrefix(val, ".{"), "}")
					inner = strings.TrimSpace(inner)
					inner = strings.ReplaceAll(inner, " = ", ": ")
					inner = strings.ReplaceAll(inner, ".", "")
					val = typ + " {" + inner + "}"
				} else {
					continue
				}
			}
			decl := "let "
			if kw == "var" {
				decl = "var "
			}
			b.WriteString("  " + decl + name + " = " + val + "\n")
			continue
		}
		if t == "}" {
			b.WriteString("}\n")
			continue
		}
		b.WriteString("  ")
		b.WriteString(t)
		b.WriteByte('\n')
	}
	b.WriteString("}\n")
	return b.String()
}

func transformExpr(expr string) string {
	if m := typedArrRE.FindStringSubmatch(expr); m != nil {
		return "[" + strings.TrimSpace(m[1]) + "]"
	}
	if m := orderRE.FindStringSubmatch(expr); m != nil {
		a, b, op, cmp := strings.TrimSpace(m[1]), strings.TrimSpace(m[2]), m[3], m[4]
		switch cmp {
		case "lt":
			if op == "==" {
				return fmt.Sprintf("%s < %s", a, b)
			}
			return fmt.Sprintf("%s >= %s", a, b)
		case "gt":
			if op == "==" {
				return fmt.Sprintf("%s > %s", a, b)
			}
			return fmt.Sprintf("%s <= %s", a, b)
		}
	}
	return expr
}

func extractPrint(line string) string {
	m := printRE.FindStringSubmatch(strings.TrimSpace(line))
	if len(m) == 2 {
		return transformExpr(strings.TrimSpace(m[1]))
	}
	return ""
}

func translate(src string) (string, error) {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "//") || strings.HasPrefix(line, "const ") {
			continue
		}
		if strings.HasPrefix(line, "pub fn main") || strings.HasPrefix(line, "fn main") {
			brace := strings.Count(line, "{") - strings.Count(line, "}")
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				brace += strings.Count(l, "{") - strings.Count(l, "}")
				if p := extractPrint(l); p != "" {
					out.WriteString("print(" + p + ")\n")
					continue
				}
				t := strings.TrimSpace(strings.TrimSuffix(l, ";"))
				if m := declRE.FindStringSubmatch(t); m != nil {
					kw := m[1]
					name := m[2]
					typ := strings.TrimSpace(m[3])
					val := strings.TrimSpace(m[4])
					val = transformExpr(val)
					if typ != "" {
						if strings.HasPrefix(val, ".{") && strings.HasSuffix(val, "}") {
							inner := strings.TrimSuffix(strings.TrimPrefix(val, ".{"), "}")
							inner = strings.TrimSpace(inner)
							inner = strings.ReplaceAll(inner, " = ", ": ")
							inner = strings.ReplaceAll(inner, ".", "")
							val = typ + " {" + inner + "}"
						} else {
							continue
						}
					}
					decl := "let "
					if kw == "var" {
						decl = "var "
					}
					out.WriteString(decl + name + " = " + val + "\n")
					continue
				}
				if t != "}" {
					out.WriteString(t + "\n")
				}
			}
			continue
		}
		if strings.HasPrefix(line, "for (") && strings.Contains(line, "|") {
			brace := strings.Count(line, "{") - strings.Count(line, "}")
			loopLines := []string{line}
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				loopLines = append(loopLines, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			out.WriteString(convertLoop(loopLines))
			continue
		}
		if strings.HasPrefix(line, "fn ") || strings.HasPrefix(line, "pub fn ") {
			brace := strings.Count(line, "{") - strings.Count(line, "}")
			fnLines := []string{line}
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				fnLines = append(fnLines, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			out.WriteString(convertFunction(fnLines))
			continue
		}
	}
	res := out.String()
	if strings.TrimSpace(res) == "" {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return res, nil
}
