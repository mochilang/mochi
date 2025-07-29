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
	if !strings.HasPrefix(expr, "[") {
		return "", false
	}
	open := strings.Index(expr, "{")
	close := strings.LastIndex(expr, "}")
	if open < 0 || close <= open {
		return "", false
	}
	return strings.TrimSpace(expr[open+1 : close]), true
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

type stmt struct {
	line     string
	children []stmt
}

func parseBlock(lines []string, i *int) []stmt {
	var res []stmt
	for *i < len(lines) {
		t := strings.TrimSpace(lines[*i])
		*i++
		if t == "" || strings.HasPrefix(t, "//") || strings.HasPrefix(t, "const std") {
			continue
		}
		if t == "}" {
			break
		}
		if strings.HasSuffix(t, "{") {
			header := strings.TrimSpace(strings.TrimSuffix(t, "{"))
			body := parseBlock(lines, i)
			res = append(res, stmt{line: header, children: body})
			continue
		}
		res = append(res, stmt{line: strings.TrimSuffix(t, ";")})
	}
	return res
}

func parseAssign(line string) (lhs, rhs string, ok bool) {
	if strings.Contains(line, "==") || strings.Contains(line, "!=") || strings.Contains(line, ">=") || strings.Contains(line, "<=") {
		return
	}
	if strings.Count(line, "=") != 1 {
		return
	}
	parts := strings.SplitN(line, "=", 2)
	lhs = strings.TrimSpace(parts[0])
	rhs = strings.TrimSpace(parts[1])
	if strings.HasPrefix(lhs, "var ") || strings.HasPrefix(lhs, "const ") {
		return
	}
	ok = lhs != "" && rhs != ""
	return
}

func gen(st stmt, indent string, b *strings.Builder) {
	line := strings.TrimSpace(st.line)
	switch {
	case strings.HasPrefix(line, "pub fn main") || strings.HasPrefix(line, "fn main"):
		for _, ch := range st.children {
			gen(ch, indent, b)
		}
	case strings.HasPrefix(line, "fn ") || strings.HasPrefix(line, "pub fn "):
		name, params, ret, ok := parseFuncHeader(line)
		if !ok {
			return
		}
		fmt.Fprintf(b, "%sfun %s(%s)", indent, name, convertParams(params))
		rt := mapType(ret)
		if rt != "" {
			fmt.Fprintf(b, ": %s", rt)
		}
		b.WriteString(" {\n")
		for _, ch := range st.children {
			gen(ch, indent+"  ", b)
		}
		fmt.Fprintf(b, "%s}\n", indent)
	case strings.HasPrefix(line, "for (") && strings.Contains(line, "|"):
		open := strings.Index(line, "(")
		close := strings.Index(line, ")")
		if open < 0 || close < open {
			return
		}
		rng := strings.TrimSpace(line[open+1 : close])
		rest := line[close+1:]
		p1 := strings.Index(rest, "|")
		if p1 < 0 {
			return
		}
		p2 := strings.Index(rest[p1+1:], "|")
		if p2 < 0 {
			return
		}
		varName := strings.TrimSpace(rest[p1+1 : p1+1+p2])
		fmt.Fprintf(b, "%sfor %s in %s {\n", indent, varName, rng)
		for _, ch := range st.children {
			gen(ch, indent+"  ", b)
		}
		fmt.Fprintf(b, "%s}\n", indent)
	case strings.HasPrefix(line, "if ("):
		cond := strings.TrimSuffix(strings.TrimPrefix(line, "if ("), ")")
		fmt.Fprintf(b, "%sif %s {\n", indent, cond)
		for _, ch := range st.children {
			gen(ch, indent+"  ", b)
		}
		fmt.Fprintf(b, "%s}\n", indent)
	default:
		if name, val, ok := parseDecl(line); ok {
			fmt.Fprintf(b, "%slet %s = %s\n", indent, name, transformExpr(val))
			return
		}
		if lhs, rhs, ok := parseAssign(line); ok {
			fmt.Fprintf(b, "%s%s = %s\n", indent, lhs, transformExpr(rhs))
			return
		}
		if p := extractPrint(line); p != "" {
			fmt.Fprintf(b, "%sprint(%s)\n", indent, p)
			return
		}
		if line == "continue" || line == "break" {
			fmt.Fprintf(b, "%s%s\n", indent, line)
			return
		}
	}
}

func translate(src string) (string, error) {
	lines := strings.Split(src, "\n")
	idx := 0
	stmts := parseBlock(lines, &idx)
	var out strings.Builder
	for _, st := range stmts {
		gen(st, "", &out)
	}
	res := out.String()
	if strings.TrimSpace(res) == "" {
		return "", fmt.Errorf("no convertible symbols found")
	}
	return res, nil
}

// TranslateForTest exposes translate for test helpers.
func TranslateForTest(src string) (string, error) { return translate(src) }
