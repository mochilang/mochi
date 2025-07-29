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
	tt := strings.TrimSpace(t)
	if strings.HasPrefix(tt, "[]") || strings.HasPrefix(tt, "[_]") {
		inner := strings.TrimPrefix(tt, "[]")
		inner = strings.TrimPrefix(inner, "[_]")
		return "list<" + mapType(inner) + ">"
	}
	switch tt {
	case "i64", "i32", "u64", "u32":
		return "int"
	case "f64", "f32":
		return "float"
	case "bool":
		return "bool"
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

func findClosingBrace(s string, start int) int {
	depth := 0
	for i := start; i < len(s); i++ {
		switch s[i] {
		case '{':
			depth++
		case '}':
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

func stripTypedArrays(expr string) string {
	for {
		idx := strings.Index(expr, "[_]")
		if idx < 0 {
			return expr
		}
		brace := strings.Index(expr[idx:], "{")
		if brace < 0 {
			return expr
		}
		brace += idx
		end := findClosingBrace(expr, brace)
		if end < 0 {
			return expr
		}
		inner := strings.TrimSpace(expr[brace+1 : end])
		expr = expr[:idx] + "[" + inner + "]" + expr[end+1:]
	}
}

func replaceBuiltins(expr string) string {
	expr = strings.ReplaceAll(expr, "std.mem.len(", "len(")
	return expr
}

func extractParens(s string) (inner, rest string, ok bool) {
	if !strings.HasPrefix(strings.TrimSpace(s), "(") {
		return "", s, false
	}
	depth := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				inner = strings.TrimSpace(s[1:i])
				rest = s[i+1:]
				return inner, rest, true
			}
		}
	}
	return "", s, false
}

func splitElseExpr(s string) (thenPart, elsePart string, ok bool) {
	depth := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '(', '{':
			depth++
		case ')', '}':
			depth--
		}
		if depth == 0 && strings.HasPrefix(s[i:], "else ") {
			thenPart = strings.TrimSpace(s[:i])
			elsePart = strings.TrimSpace(s[i+5:])
			return thenPart, elsePart, true
		}
	}
	return "", "", false
}

func parseIfExpr(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	if !strings.HasPrefix(expr, "if ") {
		return "", false
	}
	expr = strings.TrimSpace(strings.TrimPrefix(expr, "if "))
	cond, rest, ok := extractParens(expr)
	if !ok {
		return "", false
	}
	thenPart, elsePart, ok := splitElseExpr(strings.TrimSpace(rest))
	if !ok {
		return "", false
	}
	thenPart = transformExpr(thenPart)
	if strings.HasPrefix(elsePart, "if ") {
		elsePart, _ = parseIfExpr(elsePart)
	} else {
		elsePart = transformExpr(elsePart)
	}
	return fmt.Sprintf("if %s then %s else %s", transformExpr(cond), thenPart, elsePart), true
}

func parseContains(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	var not bool
	switch {
	case strings.HasSuffix(expr, " != null"):
		expr = strings.TrimSpace(expr[:len(expr)-len(" != null")])
	case strings.HasSuffix(expr, " == null"):
		not = true
		expr = strings.TrimSpace(expr[:len(expr)-len(" == null")])
	default:
		return "", false
	}
	prefix := "std.mem.indexOf(u8,"
	if !strings.HasPrefix(expr, prefix) || !strings.HasSuffix(expr, ")") {
		return "", false
	}
	args := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), ")")
	parts := strings.Split(args, ",")
	if len(parts) != 2 {
		return "", false
	}
	s := strings.TrimSpace(parts[0])
	sub := strings.TrimSpace(parts[1])
	out := fmt.Sprintf("%s.contains(%s)", s, sub)
	if not {
		out = "!" + out
	}
	return out, true
}

func parseScalarContains(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	var not bool
	switch {
	case strings.HasSuffix(expr, " != null"):
		expr = strings.TrimSpace(expr[:len(expr)-len(" != null")])
	case strings.HasSuffix(expr, " == null"):
		not = true
		expr = strings.TrimSpace(expr[:len(expr)-len(" == null")])
	default:
		return "", false
	}
	prefix := "std.mem.indexOfScalar("
	if !strings.HasPrefix(expr, prefix) || !strings.HasSuffix(expr, ")") {
		return "", false
	}
	inner := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), ")")
	parts := strings.Split(inner, ",")
	if len(parts) != 3 {
		return "", false
	}
	arr := strings.TrimSpace(parts[1])
	val := strings.TrimSpace(parts[2])
	out := fmt.Sprintf("%s.contains(%s)", arr, val)
	if not {
		out = "!" + out
	}
	return out, true
}

// parseParseInt converts calls like `std.fmt.parseInt(i64, expr, 10)` optionally
// followed by a `catch` clause into Mochi cast expressions.
func parseParseInt(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	if idx := strings.Index(expr, " catch "); idx >= 0 {
		expr = strings.TrimSpace(expr[:idx])
	}
	const prefix = "std.fmt.parseInt("
	if !strings.HasPrefix(expr, prefix) || !strings.HasSuffix(expr, ")") {
		return "", false
	}
	inner := strings.TrimSpace(expr[len(prefix) : len(expr)-1])
	parts := strings.Split(inner, ",")
	if len(parts) != 3 {
		return "", false
	}
	typ := mapType(strings.TrimSpace(parts[0]))
	val := transformExpr(strings.TrimSpace(parts[1]))
	if typ == "" {
		typ = "any"
	}
	return fmt.Sprintf("%s as %s", val, typ), true
}

// replaceSliceExprs converts occurrences of "[start..end]" within the given
// expression to Mochi's slice syntax "[start:end]". It performs a simple text
// replacement without understanding nested structures but is sufficient for the
// test programs which use straightforward slices.
func replaceSliceExprs(expr string) string {
	for {
		idx := strings.Index(expr, "..")
		if idx < 0 {
			return expr
		}
		open := strings.LastIndex(expr[:idx], "[")
		close := strings.Index(expr[idx+2:], "]")
		if open < 0 || close < 0 {
			return expr
		}
		close += idx + 2
		start := strings.TrimSpace(expr[open+1 : idx])
		end := strings.TrimSpace(expr[idx+2 : close])
		expr = expr[:open] + "[" + start + ":" + end + "]" + expr[close+1:]
	}
}

func parseSlice(expr string) (string, bool) {
	idxDots := strings.Index(expr, "..")
	if idxDots < 0 {
		return "", false
	}
	idxOpen := strings.LastIndex(expr[:idxDots], "[")
	if idxOpen < 0 {
		return "", false
	}
	idxClose := strings.Index(expr[idxDots:], "]")
	if idxClose < 0 {
		return "", false
	}
	idxClose += idxDots
	base := strings.TrimSpace(expr[:idxOpen])
	start := strings.TrimSpace(expr[idxOpen+1 : idxDots])
	end := strings.TrimSpace(expr[idxDots+2 : idxClose])
	if rest := strings.TrimSpace(expr[idxClose+1:]); rest != "" {
		return "", false
	}
	start = transformExpr(start)
	end = transformExpr(end)
	return fmt.Sprintf("%s[%s:%s]", transformExpr(base), start, end), true
}

func parseRecordLiteral(expr string) (string, bool) {
	expr = strings.TrimSpace(expr)
	if !strings.HasPrefix(expr, ".{") || !strings.HasSuffix(expr, "}") {
		return "", false
	}
	inner := strings.TrimSpace(expr[2 : len(expr)-1])
	if inner == "" {
		return "{}", true
	}
	parts := strings.Split(inner, ",")
	var fields []string
	for _, p := range parts {
		fv := strings.SplitN(p, "=", 2)
		if len(fv) != 2 {
			continue
		}
		key := strings.TrimSpace(strings.TrimPrefix(fv[0], "."))
		val := transformExpr(strings.TrimSpace(fv[1]))
		fields = append(fields, fmt.Sprintf("%s: %s", key, val))
	}
	return "{" + strings.Join(fields, ", ") + "}", true
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
	expr = stripTypedArrays(expr)
	expr = replaceBuiltins(expr)
	expr = replaceSliceExprs(expr)
	if res, ok := parseRecordLiteral(expr); ok {
		return res
	}
	if res, ok := parseSlice(expr); ok {
		return res
	}
	if inner, ok := isTypedArray(expr); ok {
		return "[" + inner + "]"
	}
	if res, ok := parseIfExpr(expr); ok {
		return res
	}
	if res, ok := parseContains(expr); ok {
		return res
	}
	if res, ok := parseScalarContains(expr); ok {
		return res
	}
	if res, ok := parseParseInt(expr); ok {
		return res
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
			if !strings.Contains(header, "struct") {
				res = append(res, stmt{line: header, children: body})
			}
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
	if strings.Contains(line, "struct {") {
		return
	}
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
		rng := transformExpr(strings.TrimSpace(line[open+1 : close]))
		rest := line[close+1:]
		p1 := strings.Index(rest, "|")
		if p1 < 0 {
			return
		}
		p2 := strings.Index(rest[p1+1:], "|")
		if p2 < 0 {
			return
		}
		vars := strings.Split(strings.TrimSpace(rest[p1+1:p1+1+p2]), ",")
		iter := strings.TrimSpace(vars[0])
		idx := ""
		if len(vars) > 1 {
			idx = strings.TrimSpace(vars[1])
		}
		if idx != "" {
			fmt.Fprintf(b, "%sfor %s, %s in %s {\n", indent, iter, idx, rng)
		} else {
			fmt.Fprintf(b, "%sfor %s in %s {\n", indent, iter, rng)
		}
		for _, ch := range st.children {
			gen(ch, indent+"  ", b)
		}
		fmt.Fprintf(b, "%s}\n", indent)
	case strings.HasPrefix(line, "while ("):
		cond := strings.TrimSuffix(strings.TrimPrefix(line, "while ("), ")")
		fmt.Fprintf(b, "%swhile %s {\n", indent, transformExpr(cond))
		for _, ch := range st.children {
			gen(ch, indent+"  ", b)
		}
		fmt.Fprintf(b, "%s}\n", indent)
	case strings.HasPrefix(line, "if (") && strings.Contains(line, ")") && !strings.HasSuffix(line, "{"):
		cond, rest, ok := extractParens(strings.TrimPrefix(strings.TrimSpace(line), "if"))
		if ok && strings.TrimSpace(rest) != "" {
			fmt.Fprintf(b, "%sif %s {\n", indent, transformExpr(cond))
			gen(stmt{line: strings.TrimSpace(rest)}, indent+"  ", b)
			fmt.Fprintf(b, "%s}\n", indent)
			return
		}
		fallthrough
	case strings.HasPrefix(line, "if ("):
		cond := strings.TrimSuffix(strings.TrimPrefix(line, "if ("), ")")
		fmt.Fprintf(b, "%sif %s {\n", indent, transformExpr(cond))
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
		if strings.HasPrefix(line, "return") {
			rest := strings.TrimSpace(strings.TrimPrefix(line, "return"))
			if rest != "" {
				fmt.Fprintf(b, "%sreturn %s\n", indent, transformExpr(rest))
			} else {
				fmt.Fprintf(b, "%sreturn\n", indent)
			}
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
