package scala

import (
	any2mochi "mochi/tools/any2mochi"

	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// Convert converts Scala source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	ls := any2mochi.Servers["scala"]
	if ls.Command != "" {
		if out, err := convertWithLS(src, ""); err == nil {
			return out, nil
		}
	}
	return convertFallback(src)
}

func convertWithLS(src, root string) ([]byte, error) {
	ls := any2mochi.Servers["scala"]
	syms, diags, err := any2mochi.EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}

	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	var funcs []any2mochi.DocumentSymbol
	var walk func([]any2mochi.DocumentSymbol)
	walk = func(list []any2mochi.DocumentSymbol) {
		for _, s := range list {
			if s.Kind == any2mochi.SymbolKindFunction || s.Kind == any2mochi.SymbolKindMethod {
				funcs = append(funcs, s)
			}
			if len(s.Children) > 0 {
				walk(s.Children)
			}
		}
	}
	walk(syms)

	var out strings.Builder
	for _, s := range funcs {
		code := convertFunc(lines, s, root)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.TrimSuffix(out.String(), "\n")), nil
}

// ConvertFile reads the Scala file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	ls := any2mochi.Servers["scala"]
	if ls.Command != "" {
		if out, err := convertWithLS(string(data), filepath.Dir(path)); err == nil {
			return out, nil
		}
	}
	return convertFallback(string(data))
}

func convertFunc(lines []string, sym any2mochi.DocumentSymbol, root string) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	sigParams, ret := parseSignature(header, name)
	if len(sigParams) == 0 && sym.Detail != nil {
		sigParams, ret = parseSignature(*sym.Detail, name)
	}
	if len(sigParams) == 0 {
		if hov, err := any2mochi.EnsureAndHoverWithRoot(any2mochi.Servers["scala"].Command, any2mochi.Servers["scala"].Args, any2mochi.Servers["scala"].LangID, strings.Join(lines, "\n"), sym.SelectionRange.Start, root); err == nil {
			if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
				for _, l := range strings.Split(mc.Value, "\n") {
					if strings.Contains(l, name+"(") {
						sigParams, ret = parseSignature(l, name)
						break
					}
				}
			}
		}
	}

	var b strings.Builder
	b.WriteString("fun ")
	b.WriteString(name)
	b.WriteByte('(')
	for i, p := range sigParams {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(p)
	}
	b.WriteString(")")
	if ret != "" && ret != "Unit" {
		b.WriteString(": ")
		b.WriteString(ret)
	}
	b.WriteString(" {\n")

	indent := 1
	write := func(s string) {
		if indent < 0 {
			indent = 0
		}
		b.WriteString(strings.Repeat("  ", indent))
		b.WriteString(s)
		b.WriteByte('\n')
	}

	reWhile := regexp.MustCompile(`^while\s*\((.*)\)\s*{`)
	reFor := regexp.MustCompile(`^for \(([^<-]+)<-([^\)]+)\)\s*{`)
	reIf := regexp.MustCompile(`^if\s*\((.*)\)\s*{`)
	reElseIf := regexp.MustCompile(`^}\s*else\s*if\s*\((.*)\)\s*{`)
	reElse := regexp.MustCompile(`^}\s*else\s*{`)

	for _, l := range seg[1:] {
		line := strings.TrimSpace(l)
		if line == "" {
			continue
		}
		if line == "}" {
			indent--
			write("}")
			if indent == 0 {
				break
			}
			continue
		}

		if m := reElseIf.FindStringSubmatch(line); m != nil {
			indent--
			write("else if " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if reElse.MatchString(line) {
			indent--
			write("else {")
			indent++
			continue
		}
		if m := reWhile.FindStringSubmatch(line); m != nil {
			write("while " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if m := reFor.FindStringSubmatch(line); m != nil {
			name := strings.TrimSpace(m[1])
			expr := convertExpr(strings.TrimSpace(m[2]))
			write("for " + name + " in " + expr + " {")
			indent++
			continue
		}
		if m := reIf.FindStringSubmatch(line); m != nil {
			write("if " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}

		switch {
		case strings.HasPrefix(line, "println("):
			expr := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
			write("print(" + convertExpr(expr) + ")")
		case strings.HasPrefix(line, "return "):
			expr := strings.TrimSpace(strings.TrimPrefix(line, "return "))
			write("return " + convertExpr(expr))
		case strings.HasPrefix(line, "var "):
			content := strings.TrimPrefix(line, "var ")
			if idx := strings.Index(content, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(content[:idx]))[0]
				expr := strings.TrimSpace(content[idx+1:])
				write("var " + n + " = " + convertExpr(expr))
			} else {
				write("var " + content)
			}
		case strings.HasPrefix(line, "val "):
			if idx := strings.Index(line, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(line[4:idx]))[0]
				n = strings.TrimSuffix(n, ":")
				expr := strings.TrimSpace(line[idx+1:])
				write("let " + n + " = " + convertExpr(expr))
			}
		case strings.Contains(line, ".append("):
			idx := strings.Index(line, ".append(")
			recv := strings.TrimSpace(line[:idx])
			arg := strings.TrimSuffix(line[idx+8:], ")")
			write("append(" + recv + ", " + convertExpr(arg) + ")")
		case strings.Contains(line, "="):
			if strings.Contains(line, "=>") {
				// lambda or pattern match arrow, keep as-is
				write(line)
				continue
			}
			parts := strings.SplitN(line, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if left != "" && right != "" {
				write(left + " = " + convertExpr(right))
			}
		}
	}
	b.WriteString("}")
	return b.String()
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasSuffix(expr, ".toString()") {
		expr = "str(" + strings.TrimSuffix(expr, ".toString()") + ")"
	}
	prefix := "scala.collection.mutable.ArrayBuffer("
	if strings.HasPrefix(expr, prefix) && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), ")")
		expr = "[" + inner + "]"
	}
	mapPrefix := "scala.collection.mutable.Map("
	if strings.HasPrefix(expr, mapPrefix) && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, mapPrefix), ")")
		parts := splitArgs(inner)
		items := make([]string, len(parts))
		for i, p := range parts {
			kv := strings.SplitN(p, "->", 2)
			if len(kv) == 2 {
				k := convertExpr(strings.TrimSpace(kv[0]))
				v := convertExpr(strings.TrimSpace(kv[1]))
				items[i] = k + ": " + v
			} else {
				items[i] = convertExpr(p)
			}
		}
		return "{" + strings.Join(items, ", ") + "}"
	}
	if strings.HasPrefix(expr, "Map(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "Map("), ")")
		parts := splitArgs(inner)
		items := make([]string, len(parts))
		for i, p := range parts {
			kv := strings.SplitN(p, "->", 2)
			if len(kv) == 2 {
				k := convertExpr(strings.TrimSpace(kv[0]))
				v := convertExpr(strings.TrimSpace(kv[1]))
				items[i] = k + ": " + v
			} else {
				items[i] = convertExpr(p)
			}
		}
		return "{" + strings.Join(items, ", ") + "}"
	}
	if strings.HasSuffix(expr, ".size") {
		expr = "len(" + strings.TrimSuffix(expr, ".size") + ")"
	}
	if strings.HasSuffix(expr, ".length") {
		expr = "len(" + strings.TrimSuffix(expr, ".length") + ")"
	}
	if strings.HasSuffix(expr, ".sum") {
		expr = "sum(" + strings.TrimSuffix(expr, ".sum") + ")"
	}
	if strings.HasPrefix(expr, "_indexList(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "_indexList("), ")")
		parts := splitArgs(inner)
		if len(parts) == 2 {
			return convertExpr(parts[0]) + "[" + convertExpr(parts[1]) + "]"
		}
	}
	if strings.HasPrefix(expr, "_sliceString(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "_sliceString("), ")")
		parts := splitArgs(inner)
		if len(parts) == 3 {
			return convertExpr(parts[0]) + "[" + convertExpr(parts[1]) + ": " + convertExpr(parts[2]) + "]"
		}
	}
	return expr
}

// parseSignature extracts parameter names and return type from a Scala
// function signature string. The provided name is used to locate the opening
// parenthesis when necessary.
func parseSignature(sig, name string) ([]string, string) {
	sig = strings.ReplaceAll(sig, "\n", " ")
	idx := strings.Index(sig, name+"(")
	if idx == -1 {
		idx = strings.Index(sig, "(")
		if idx == -1 {
			return nil, ""
		}
	} else {
		idx += len(name)
	}
	open := strings.Index(sig[idx:], "(")
	if open == -1 {
		return nil, ""
	}
	open += idx
	depth := 0
	close := -1
	for i := open; i < len(sig); i++ {
		switch sig[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				close = i
				i = len(sig)
			}
		}
	}
	if close == -1 {
		return nil, ""
	}
	paramsPart := sig[open+1 : close]
	var params []string
	for _, p := range splitArgs(paramsPart) {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if c := strings.Index(p, ":"); c != -1 {
			p = p[:c]
		}
		params = append(params, strings.TrimSpace(p))
	}
	rest := strings.TrimSpace(sig[close+1:])
	ret := ""
	if strings.HasPrefix(rest, ":") {
		rest = strings.TrimSpace(rest[1:])
		if eq := strings.Index(rest, "="); eq != -1 {
			rest = strings.TrimSpace(rest[:eq])
		}
		ret = strings.TrimSpace(rest)
	}
	return params, ret
}

func splitArgs(s string) []string {
	var parts []string
	var b strings.Builder
	depth := 0
	inStr := false
	for i := 0; i < len(s); i++ {
		ch := s[i]
		if inStr {
			b.WriteByte(ch)
			if ch == '"' {
				inStr = false
			}
			continue
		}
		switch ch {
		case '"':
			inStr = true
			b.WriteByte(ch)
		case '(', '{', '[':
			depth++
			b.WriteByte(ch)
		case ')', '}', ']':
			depth--
			b.WriteByte(ch)
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(b.String()))
				b.Reset()
			} else {
				b.WriteByte(ch)
			}
		default:
			b.WriteByte(ch)
		}
	}
	if b.Len() > 0 {
		parts = append(parts, strings.TrimSpace(b.String()))
	}
	return parts
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if idx := strings.LastIndex(t, "."); idx != -1 {
		t = t[idx+1:]
	}
	switch t {
	case "Int", "Long", "Short", "Byte":
		return "int"
	case "Double", "Float":
		return "float"
	case "Boolean":
		return "bool"
	case "String":
		return "string"
	default:
		return t
	}
}

func convertTypeDecl(t TypeDecl) string {
	if len(t.Variants) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("type ")
	b.WriteString(t.Name)
	b.WriteString(" =\n")
	for i, v := range t.Variants {
		if i > 0 {
			b.WriteString("  | ")
		} else {
			b.WriteString("  ")
		}
		b.WriteString(v.Name)
		if len(v.Fields) > 0 {
			b.WriteString("(")
			for j, f := range v.Fields {
				if j > 0 {
					b.WriteString(", ")
				}
				b.WriteString(f.Name)
				typ := f.Type
				if typ == "Any" || typ == "" {
					typ = t.Name
				} else {
					typ = mapType(typ)
				}
				if typ != "" {
					b.WriteString(": ")
					b.WriteString(typ)
				}
			}
			b.WriteString(")")
		}
		b.WriteByte('\n')
	}
	return strings.TrimSuffix(b.String(), "\n")
}

// convertFallback parses Scala source using the scalaast CLI and converts
// the resulting AST to Mochi.
func convertFallback(src string) ([]byte, error) {
	file, err := runParse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, t := range file.Types {
		code := convertTypeDecl(t)
		if code != "" {
			out.WriteString(code)
			out.WriteByte('\n')
		}
	}
	for _, fn := range file.Funcs {
		code := convertFromAST(fn)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.TrimSuffix(out.String(), "\n")), nil
}

// convertFromAST converts a parsed Func into Mochi code.
func convertFromAST(fn Func) string {
	var b strings.Builder
	topLevel := fn.Name == "main"
	if !topLevel {
		b.WriteString("fun ")
		b.WriteString(fn.Name)
		b.WriteByte('(')
		for i, p := range fn.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(p)
		}
		b.WriteString(")")
		b.WriteString(" {")
		if len(fn.Body) > 0 {
			b.WriteByte('\n')
		}
	}
	indent := 1
	if topLevel {
		indent = 0
	}
	write := func(s string) {
		b.WriteString(strings.Repeat("  ", indent))
		b.WriteString(s)
		b.WriteByte('\n')
	}

	reWhile := regexp.MustCompile(`^while\s*\((.*)\)\s*{`)
	reFor := regexp.MustCompile(`^for \(([^<-]+)<-([^\)]+)\)\s*{`)
	reIf := regexp.MustCompile(`^if\s*\((.*)\)\s*{`)
	reElseIf := regexp.MustCompile(`^}\s*else\s*if\s*\((.*)\)\s*{`)
	reElse := regexp.MustCompile(`^}\s*else\s*{`)
	reMatch := regexp.MustCompile(`^(?:return\s+)?\(?(.+)\s+match\s*{`)
	reCase := regexp.MustCompile(`^case\s+(.+)\s+=>\s*(.*)$`)
	inMatch := false

	for _, l := range fn.Body {
		line := strings.TrimSpace(l)
		if line == "" {
			continue
		}
		if line == "}" || line == "})" {
			if indent > 0 {
				indent--
			}
			write("}")
			if line == "})" && inMatch {
				inMatch = false
			}
			continue
		}
		if m := reElseIf.FindStringSubmatch(line); m != nil {
			if indent > 0 {
				indent--
			}
			write("else if " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if reElse.MatchString(line) {
			if indent > 0 {
				indent--
			}
			write("else {")
			indent++
			continue
		}
		if m := reWhile.FindStringSubmatch(line); m != nil {
			write("while " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if m := reFor.FindStringSubmatch(line); m != nil {
			name := strings.TrimSpace(m[1])
			expr := convertExpr(strings.TrimSpace(m[2]))
			write("for " + name + " in " + expr + " {")
			indent++
			continue
		}
		if m := reIf.FindStringSubmatch(line); m != nil {
			write("if " + convertExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if m := reMatch.FindStringSubmatch(line); m != nil {
			expr := convertExpr(strings.TrimSpace(m[1]))
			if strings.HasPrefix(line, "return ") {
				write("return match " + expr + " {")
			} else {
				write("match " + expr + " {")
			}
			indent++
			inMatch = true
			continue
		}
		if m := reCase.FindStringSubmatch(line); m != nil && inMatch {
			pat := strings.TrimSpace(m[1])
			body := strings.TrimSpace(m[2])
			write(pat + " => " + convertExpr(body))
			continue
		}

		switch {
		case strings.HasPrefix(line, "println("):
			expr := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
			write("print(" + convertExpr(expr) + ")")
		case strings.HasPrefix(line, "return "):
			expr := strings.TrimSpace(strings.TrimPrefix(line, "return "))
			write("return " + convertExpr(expr))
		case strings.HasPrefix(line, "var "):
			content := strings.TrimPrefix(line, "var ")
			if idx := strings.Index(content, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(content[:idx]))[0]
				expr := strings.TrimSpace(content[idx+1:])
				write("var " + n + " = " + convertExpr(expr))
			} else {
				write("var " + content)
			}
		case strings.HasPrefix(line, "val "):
			if idx := strings.Index(line, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(line[4:idx]))[0]
				n = strings.TrimSuffix(n, ":")
				expr := strings.TrimSpace(line[idx+1:])
				write("let " + n + " = " + convertExpr(expr))
			}
		case strings.Contains(line, ".append("):
			idx := strings.Index(line, ".append(")
			recv := strings.TrimSpace(line[:idx])
			arg := strings.TrimSuffix(line[idx+8:], ")")
			write("append(" + recv + ", " + convertExpr(arg) + ")")
		case strings.Contains(line, "="):
			parts := strings.SplitN(line, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if left != "" && right != "" {
				write(left + " = " + convertExpr(right))
			}
		}
	}
	if !topLevel {
		b.WriteString("}")
	}
	return b.String()
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		msg := d.Message
		from := start - 2
		if from < 0 {
			from = 0
		}
		to := start + 2
		if to >= len(lines) {
			to = len(lines) - 1
		}
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", start+1, col+1, msg))
		for i := from; i <= to; i++ {
			out.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
			if i == start {
				out.WriteString("     " + strings.Repeat(" ", col) + "^\n")
			}
		}
	}
	return strings.TrimSpace(out.String())
}
