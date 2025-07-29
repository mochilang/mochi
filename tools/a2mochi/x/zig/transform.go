//go:build slow

package zig

import (
	"fmt"
	"os"
	"strconv"
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

func parseDecl(line string) (name, val string, ok bool) {
	line = strings.TrimSpace(line)
	if strings.HasPrefix(line, "const ") {
		line = strings.TrimPrefix(line, "const ")
	} else if strings.HasPrefix(line, "var ") {
		line = strings.TrimPrefix(line, "var ")
	} else {
		return
	}
	eq := strings.Index(line, "=")
	if eq < 0 {
		return
	}
	name = strings.TrimSpace(line[:eq])
	val = strings.TrimSpace(line[eq+1:])
	ok = name != "" && val != ""
	return
}

func isTypedArray(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "[_]") {
		open := strings.Index(expr, "{")
		close := strings.LastIndex(expr, "}")
		if open >= 0 && close > open {
			return strings.TrimSpace(expr[open+1 : close]), true
		}
	}
	return "", false
}

func isNumber(expr string) bool {
	if _, err := strconv.ParseFloat(expr, 64); err == nil {
		return true
	}
	return false
}

func parseOrder(expr string) (string, bool) {
	prefix := "std.mem.order(u8,"
	if !strings.HasPrefix(expr, prefix) {
		return "", false
	}
	rest := strings.TrimSpace(expr[len(prefix):])
	end := strings.Index(rest, ")")
	if end < 0 {
		return "", false
	}
	args := strings.Split(rest[:end], ",")
	if len(args) != 2 {
		return "", false
	}
	a := strings.TrimSpace(args[0])
	b := strings.TrimSpace(args[1])
	comp := strings.TrimSpace(rest[end+1:])
	switch comp {
	case "== .lt":
		return fmt.Sprintf("%s < %s", a, b), true
	case "!= .lt":
		return fmt.Sprintf("%s >= %s", a, b), true
	case "== .gt":
		return fmt.Sprintf("%s > %s", a, b), true
	case "!= .gt":
		return fmt.Sprintf("%s <= %s", a, b), true
	}
	return "", false
}

func parseFuncHeader(src string) (name, params, ret string, ok bool) {
	src = strings.TrimSpace(src)
	if strings.HasPrefix(src, "pub ") {
		src = strings.TrimSpace(strings.TrimPrefix(src, "pub "))
	}
	if !strings.HasPrefix(src, "fn ") {
		return
	}
	src = strings.TrimPrefix(src, "fn ")
	open := strings.Index(src, "(")
	close := strings.Index(src, ")")
	if open < 0 || close < open {
		return
	}
	name = strings.TrimSpace(src[:open])
	params = src[open+1 : close]
	rest := strings.TrimSpace(src[close+1:])
	if idx := strings.Index(rest, "{"); idx >= 0 {
		ret = strings.TrimSpace(rest[:idx])
	} else {
		ret = strings.TrimSpace(rest)
	}
	ok = true
	return
}

func convertFunction(lines []string) string {
	first := strings.TrimSpace(lines[0])
	name, params, ret, ok := parseFuncHeader(first)
	if !ok {
		return ""
	}
	params = convertParams(params)
	ret = mapType(ret)

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

	if len(lines) > 2 {
		body := convertLines(lines[1:len(lines)-1], "  ")
		b.WriteString(body)
	}
	b.WriteString("}\n")
	return b.String()
}

func convertLoop(lines []string, indent string) string {
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
	fmt.Fprintf(&b, "%sfor %s in %s {\n", indent, varName, rng)
	for i := 1; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if i == len(lines)-1 && t == "}" {
			break
		}
		t = strings.TrimSuffix(t, ";")
		if strings.HasPrefix(t, "for (") && strings.Contains(t, "|") {
			brace := strings.Count(t, "{") - strings.Count(t, "}")
			loopLines := []string{t}
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				loopLines = append(loopLines, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			b.WriteString(convertLoop(loopLines, indent+"  "))
			continue
		}
		if strings.HasPrefix(t, "if (") && strings.HasSuffix(t, ") {") {
			brace := strings.Count(t, "{") - strings.Count(t, "}")
			cond := strings.TrimSuffix(strings.TrimPrefix(t, "if ("), ") {")
			var inner []string
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				inner = append(inner, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			b.WriteString(indent + "  if " + cond + " {\n")
			b.WriteString(convertLines(inner[:len(inner)-1], indent+"    "))
			b.WriteString(indent + "  }\n")
			continue
		}
		if name, val, ok := parseDecl(t); ok {
			b.WriteString(indent + "  let " + name + " = " + val + "\n")
			continue
		}
		if p := extractPrint(t); p != "" {
			b.WriteString(indent + "  print(" + p + ")\n")
			continue
		}
		if t == "continue" {
			b.WriteString(indent + "  continue\n")
			continue
		}
		if t == "break" {
			b.WriteString(indent + "  break\n")
			continue
		}
		if t == "}" {
			b.WriteString(indent + "}\n")
			continue
		}
		b.WriteString(indent + "  " + t + "\n")
	}
	b.WriteString(indent + "}\n")
	return b.String()
}

func convertLines(lines []string, indent string) string {
	var b strings.Builder
	for i := 0; i < len(lines); i++ {
		t := strings.TrimSpace(lines[i])
		if t == "" || strings.HasPrefix(t, "//") {
			continue
		}
		if strings.HasPrefix(t, "for (") && strings.Contains(t, "|") {
			brace := strings.Count(t, "{") - strings.Count(t, "}")
			loopLines := []string{t}
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				loopLines = append(loopLines, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			b.WriteString(convertLoop(loopLines, indent))
			continue
		}
		if strings.HasPrefix(t, "if (") && strings.HasSuffix(t, ") {") {
			brace := strings.Count(t, "{") - strings.Count(t, "}")
			cond := strings.TrimSuffix(strings.TrimPrefix(t, "if ("), ") {")
			var inner []string
			for brace > 0 && i+1 < len(lines) {
				i++
				l := lines[i]
				inner = append(inner, l)
				brace += strings.Count(l, "{") - strings.Count(l, "}")
			}
			b.WriteString(indent + "if " + cond + " {\n")
			b.WriteString(convertLines(inner[:len(inner)-1], indent+"  "))
			b.WriteString(indent + "}\n")
			continue
		}
		if name, val, ok := parseDecl(t); ok {
			b.WriteString(indent + "let " + name + " = " + val + "\n")
			continue
		}
		if p := extractPrint(t); p != "" {
			b.WriteString(indent + "print(" + p + ")\n")
			continue
		}
		if t == "continue" || t == "break" {
			b.WriteString(indent + t + "\n")
			continue
		}
	}
	return b.String()
}

func transformExpr(expr string) string {
	if inner, ok := isTypedArray(expr); ok {
		return "[" + inner + "]"
	}
	if res, ok := parseOrder(expr); ok {
		return res
	}
	return expr
}

func extractPrint(line string) string {
	line = strings.TrimSpace(line)
	idx := strings.Index(line, ".print(")
	if idx < 0 {
		return ""
	}
	start := strings.Index(line[idx:], ".{")
	end := strings.LastIndex(line, "})")
	if start < 0 || end < start {
		return ""
	}
	start += idx + 2
	expr := strings.TrimSpace(line[start:end])
	return transformExpr(expr)
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
				if name, val, ok := parseDecl(t); ok {
					if _, ok2 := isTypedArray(val); ok2 || isNumber(val) {
						val = transformExpr(val)
						out.WriteString("let " + name + " = " + val + "\n")
					}
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
			out.WriteString(convertLoop(loopLines, ""))
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
