package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// ConvertScala converts scala source code to Mochi using the language server.
func ConvertScala(src string) ([]byte, error) {
	ls := Servers["scala"]
	if ls.Command != "" {
		if out, err := convertScalaWithLS(src, ""); err == nil {
			return out, nil
		}
	}
	return convertScalaFallback(src)
}

func convertScalaWithLS(src, root string) ([]byte, error) {
	ls := Servers["scala"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	lines := strings.Split(strings.ReplaceAll(src, "\r\n", "\n"), "\n")
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != SymbolKindFunction {
			continue
		}
		code := convertScalaFunc(lines, s, root)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(strings.TrimSuffix(out.String(), "\n")), nil
}

// ConvertScalaFile reads the scala file and converts it to Mochi.
func ConvertScalaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	ls := Servers["scala"]
	if ls.Command != "" {
		if out, err := convertScalaWithLS(string(data), filepath.Dir(path)); err == nil {
			return out, nil
		}
	}
	return convertScalaFallback(string(data))
}

func convertScalaFunc(lines []string, sym DocumentSymbol, root string) string {
	start := int(sym.Range.Start.Line)
	end := int(sym.Range.End.Line)
	if start < 0 || end >= len(lines) || start >= end {
		return ""
	}
	seg := lines[start : end+1]
	header := strings.TrimSpace(seg[0])

	name := sym.Name
	params, ret := parseScalaSignature(header, name)
	if len(params) == 0 && sym.Detail != nil {
		params, ret = parseScalaSignature(*sym.Detail, name)
	}
	if len(params) == 0 {
		if hov, err := EnsureAndHoverWithRoot(Servers["scala"].Command, Servers["scala"].Args, Servers["scala"].LangID, strings.Join(lines, "\n"), sym.SelectionRange.Start, root); err == nil {
			if mc, ok := hov.Contents.(MarkupContent); ok {
				for _, l := range strings.Split(mc.Value, "\n") {
					if strings.Contains(l, name+"(") {
						params, ret = parseScalaSignature(l, name)
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
	for i, p := range params {
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
			write("else if " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
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
			write("while " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if m := reFor.FindStringSubmatch(line); m != nil {
			name := strings.TrimSpace(m[1])
			expr := convertScalaExpr(strings.TrimSpace(m[2]))
			write("for " + name + " in " + expr + " {")
			indent++
			continue
		}
		if m := reIf.FindStringSubmatch(line); m != nil {
			write("if " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}

		switch {
		case strings.HasPrefix(line, "println("):
			expr := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
			write("print(" + convertScalaExpr(expr) + ")")
		case strings.HasPrefix(line, "return "):
			expr := strings.TrimSpace(strings.TrimPrefix(line, "return "))
			write("return " + convertScalaExpr(expr))
		case strings.HasPrefix(line, "var "):
			write("var " + strings.TrimPrefix(line, "var "))
		case strings.HasPrefix(line, "val "):
			if idx := strings.Index(line, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(line[4:idx]))[0]
				expr := strings.TrimSpace(line[idx+1:])
				write("let " + n + " = " + convertScalaExpr(expr))
			}
		case strings.Contains(line, "="):
			parts := strings.SplitN(line, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if left != "" && right != "" {
				write(left + " = " + convertScalaExpr(right))
			}
		}
	}
	b.WriteString("}")
	return b.String()
}

func convertScalaExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasSuffix(expr, ".toString()") {
		expr = "str(" + strings.TrimSuffix(expr, ".toString()") + ")"
	}
	prefix := "scala.collection.mutable.ArrayBuffer("
	if strings.HasPrefix(expr, prefix) && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, prefix), ")")
		expr = "[" + inner + "]"
	}
	if strings.HasSuffix(expr, ".length") {
		expr = "len(" + strings.TrimSuffix(expr, ".length") + ")"
	}
	if strings.HasPrefix(expr, "_indexList(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "_indexList("), ")")
		parts := splitArgsScala(inner)
		if len(parts) == 2 {
			return convertScalaExpr(parts[0]) + "[" + convertScalaExpr(parts[1]) + "]"
		}
	}
	if strings.HasPrefix(expr, "_sliceString(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "_sliceString("), ")")
		parts := splitArgsScala(inner)
		if len(parts) == 3 {
			return convertScalaExpr(parts[0]) + "[" + convertScalaExpr(parts[1]) + ": " + convertScalaExpr(parts[2]) + "]"
		}
	}
	return expr
}

// parseScalaSignature extracts parameter names and return type from a Scala
// function signature string. The provided name is used to locate the opening
// parenthesis when necessary.
func parseScalaSignature(sig, name string) ([]string, string) {
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
	close := strings.Index(sig[open:], ")")
	if close == -1 {
		return nil, ""
	}
	close += open
	paramsPart := sig[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
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
			rest = rest[:eq]
		}
		ret = strings.TrimSpace(rest)
	}
	return params, ret
}

func splitArgsScala(s string) []string {
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

// convertScalaFallback parses Scala source using the scalaast CLI and converts
// the resulting AST to Mochi.
func convertScalaFallback(src string) ([]byte, error) {
	funcs, err := runScalaParse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, fn := range funcs {
		code := convertScalaFromAST(fn)
		if code == "" {
			continue
		}
		out.WriteString(code)
		out.WriteByte('\n')
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(strings.TrimSuffix(out.String(), "\n")), nil
}

// convertScalaFromAST converts a parsed scalaFunc into Mochi code.
func convertScalaFromAST(fn scalaFunc) string {
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
		b.WriteString(") {")
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

	for _, l := range fn.Body {
		line := strings.TrimSpace(l)
		if line == "" {
			continue
		}
		if line == "}" {
			indent--
			write("}")
			continue
		}
		if m := reElseIf.FindStringSubmatch(line); m != nil {
			indent--
			write("else if " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
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
			write("while " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}
		if m := reFor.FindStringSubmatch(line); m != nil {
			name := strings.TrimSpace(m[1])
			expr := convertScalaExpr(strings.TrimSpace(m[2]))
			write("for " + name + " in " + expr + " {")
			indent++
			continue
		}
		if m := reIf.FindStringSubmatch(line); m != nil {
			write("if " + convertScalaExpr(strings.TrimSpace(m[1])) + " {")
			indent++
			continue
		}

		switch {
		case strings.HasPrefix(line, "println("):
			expr := strings.TrimSuffix(strings.TrimPrefix(line, "println("), ")")
			write("print(" + convertScalaExpr(expr) + ")")
		case strings.HasPrefix(line, "return "):
			expr := strings.TrimSpace(strings.TrimPrefix(line, "return "))
			write("return " + convertScalaExpr(expr))
		case strings.HasPrefix(line, "var "):
			write("var " + strings.TrimPrefix(line, "var "))
		case strings.HasPrefix(line, "val "):
			if idx := strings.Index(line, "="); idx != -1 {
				n := strings.Fields(strings.TrimSpace(line[4:idx]))[0]
				expr := strings.TrimSpace(line[idx+1:])
				write("let " + n + " = " + convertScalaExpr(expr))
			}
		case strings.Contains(line, "="):
			parts := strings.SplitN(line, "=", 2)
			left := strings.TrimSpace(parts[0])
			right := strings.TrimSpace(parts[1])
			if left != "" && right != "" {
				write(left + " = " + convertScalaExpr(right))
			}
		}
	}
	if !topLevel {
		b.WriteString("}")
	}
	return b.String()
}
