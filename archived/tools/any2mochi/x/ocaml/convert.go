//go:build archive

package ocaml

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	parent "mochi/archived/tools/any2mochi"
)

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

func diagnostics(src string, diags []parent.Diagnostic) string {
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

// Convert converts OCaml source code to Mochi. It prefers using the
// language server when available and falls back to a small parser
// implemented with tree-sitter otherwise.
func Convert(src string) ([]byte, error) {
	fallback := func() ([]byte, error) {
		prog, err := parse(src)
		if err == nil {
			code := formatProgram(prog)
			if len(code) > 0 {
				return code, nil
			}
		}
		var b strings.Builder
		for _, l := range fallbackOcaml(strings.Split(src, "\n")) {
			b.WriteString(l)
			b.WriteByte('\n')
		}
		if b.Len() == 0 {
			if err != nil {
				return nil, err
			}
			return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
		}
		return []byte(b.String()), nil
	}

	if !parent.UseLSP {
		return fallback()
	}

	ls := parent.Servers["ocaml"]
	if ls.Command == "" {
		return fallback()
	}

	syms, diags, err := parent.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return fallback()
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}
	var out strings.Builder
	lines := strings.Split(src, "\n")
	writeOcamlSymbols(&out, nil, syms, lines, src, ls)
	if out.Len() == 0 {
		// fall back to a very small regex based parser for simple scripts
		for _, l := range fallbackOcaml(lines) {
			out.WriteString(l)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the OCaml file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func writeOcamlSymbols(out *strings.Builder, prefix []string, syms []parent.DocumentSymbol, lines []string, src string, ls parent.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case parent.SymbolKindFunction, parent.SymbolKindMethod:
			names := parseOcamlParamNames(lines, s)
			types, ret := ocamlHoverSignature(src, s, ls)
			body := parseOcamlBody(lines, s)
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, n := range names {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(n)
				if i < len(types) && types[i] != "" {
					out.WriteString(": ")
					out.WriteString(types[i])
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "unit" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, b := range body {
					out.WriteString("  ")
					out.WriteString(b)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case parent.SymbolKindVariable, parent.SymbolKindConstant:
			if len(prefix) == 0 {
				typ := ocamlHoverType(src, s, ls)
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				if typ != "" && typ != "unit" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		case parent.SymbolKindStruct, parent.SymbolKindClass, parent.SymbolKindInterface, parent.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []parent.DocumentSymbol{}
			for _, c := range s.Children {
				if c.Kind == parent.SymbolKindField || c.Kind == parent.SymbolKindProperty {
					fields = append(fields, c)
				}
			}
			if len(fields) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					if typ := ocamlHoverType(src, f, ls); typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case parent.SymbolKindModule, parent.SymbolKindNamespace:
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != parent.SymbolKindStruct && s.Kind != parent.SymbolKindClass && s.Kind != parent.SymbolKindInterface && s.Kind != parent.SymbolKindEnum && s.Kind != parent.SymbolKindModule && s.Kind != parent.SymbolKindNamespace && s.Kind != parent.SymbolKindVariable && s.Kind != parent.SymbolKindConstant {
			writeOcamlSymbols(out, nameParts, s.Children, lines, src, ls)
		}
	}
}

func parseOcamlParamNames(lines []string, sym parent.DocumentSymbol) []string {
	start := posToOffset(lines, sym.SelectionRange.End)
	src := strings.Join(lines, "\n")
	rest := src[start:]
	eq := strings.Index(rest, "=")
	if eq == -1 {
		eq = len(rest)
	}
	header := strings.TrimSpace(rest[:eq])
	if idx := strings.LastIndex(header, ":"); idx != -1 {
		if idx > strings.LastIndex(header, ")") {
			header = strings.TrimSpace(header[:idx])
		}
	}
	fields := strings.Fields(header)
	var names []string
	for _, f := range fields {
		if strings.HasPrefix(f, "(") {
			f = strings.TrimPrefix(f, "(")
			if i := strings.Index(f, ")"); i != -1 {
				f = f[:i]
			}
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			f = strings.TrimSpace(f)
			if f != "" {
				names = append(names, f)
			}
		} else if strings.HasPrefix(f, ":") {
			break
		} else {
			if i := strings.Index(f, ":"); i != -1 {
				f = f[:i]
			}
			names = append(names, f)
		}
	}
	return names
}

func ocamlHoverSignature(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) ([]string, string) {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	mc, ok := hov.Contents.(parent.MarkupContent)
	if !ok {
		return nil, ""
	}
	sig := strings.TrimSpace(mc.Value)
	if strings.HasPrefix(sig, "val") {
		if idx := strings.Index(sig, ":"); idx != -1 {
			sig = strings.TrimSpace(sig[idx+1:])
		}
	}
	parts := strings.Split(sig, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	if len(parts) == 0 {
		return nil, ""
	}
	ret := mapOcamlType(parts[len(parts)-1])
	paramTypes := make([]string, 0, len(parts)-1)
	for i := 0; i < len(parts)-1; i++ {
		paramTypes = append(paramTypes, mapOcamlType(parts[i]))
	}
	return paramTypes, ret
}

func ocamlHoverType(src string, sym parent.DocumentSymbol, ls parent.LanguageServer) string {
	hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(parent.MarkupContent); ok {
		return mapOcamlType(strings.TrimSpace(mc.Value))
	}
	return ""
}

func mapOcamlType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool":
		return "bool"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-5]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapOcamlType(strings.TrimSpace(t[:len(t)-6]))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " option") {
		return mapOcamlType(strings.TrimSpace(t[:len(t)-7]))
	}
	return t
}

func posToOffset(lines []string, pos parent.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		off += len(lines[i]) + 1
	}
	off += int(pos.Character)
	return off
}

func parseOcamlBody(lines []string, sym parent.DocumentSymbol) []string {
	start := posToOffset(lines, sym.SelectionRange.End)
	end := posToOffset(lines, sym.Range.End)
	src := strings.Join(lines, "\n")
	if start >= len(src) {
		return nil
	}
	if end > len(src) {
		end = len(src)
	}
	body := src[start:end]
	if idx := strings.Index(body, "="); idx != -1 {
		body = body[idx+1:]
	}
	body = strings.TrimSpace(body)
	body = strings.TrimSuffix(body, ";;")
	body = strings.ReplaceAll(body, "print_endline", "print")
	body = strings.ReplaceAll(body, "print_string", "print")
	body = strings.ReplaceAll(body, "print_int", "print")
	body = strings.ReplaceAll(body, "print_float", "print")
	body = strings.ReplaceAll(body, "string_of_int", "str")
	body = strings.ReplaceAll(body, "string_of_float", "str")
	body = strings.ReplaceAll(body, "string_of_bool", "str")
	body = strings.ReplaceAll(body, "List.length", "len")
	reNth := regexp.MustCompile(`List\.nth\s+([^\s]+)\s+([^\s]+)`)
	body = reNth.ReplaceAllString(body, "$1[$2]")
	body = strings.ReplaceAll(body, ":=", "=")
	body = strings.ReplaceAll(body, "!", "")
	body = strings.ReplaceAll(body, "^", "+")
	body = strings.ReplaceAll(body, ";", "")
	var out []string
	reFor := regexp.MustCompile(`^for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^ ]+)\s+to\s+([^ ]+)\s+do`)
	reWhile := regexp.MustCompile(`^while\s+(.*)\s+do`)
	for _, l := range strings.Split(body, "\n") {
		l = strings.TrimSpace(l)
		if m := reFor.FindStringSubmatch(l); m != nil {
			end := strings.TrimSpace(m[3])
			end = strings.TrimSuffix(strings.TrimSpace(end), "- 1")
			end = strings.TrimSuffix(end, "-1")
			l = fmt.Sprintf("for %s in %s..%s {", m[1], strings.TrimSpace(m[2]), end)
		} else if reWhile.MatchString(l) {
			m := reWhile.FindStringSubmatch(l)
			l = fmt.Sprintf("while %s {", strings.TrimSpace(m[1]))
		} else if l == "done" || l == "done;" {
			l = "}"
		}
		if l != "" {
			out = append(out, l)
		}
	}
	return out
}

func fallbackOcaml(lines []string) []string {
	var out []string
	reLet := regexp.MustCompile(`^let\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*)`)
	reRef := regexp.MustCompile(`^let\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*ref\s+(.*)`)
	reAssign := regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*:=\s*(.*)`)
	reWhile := regexp.MustCompile(`^while\s+(.*)\s+do$`)
	reFor := regexp.MustCompile(`^for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^ ]+)\s+to\s+([^ ]+)\s+do$`)
	reDone := regexp.MustCompile(`^done;*$`)
	reNth := regexp.MustCompile(`List\.nth\s+([^\s]+)\s+([^\s]+)`)
	for _, line := range lines {
		t := strings.TrimSpace(line)
		if m := reDone.FindStringSubmatch(t); m != nil {
			out = append(out, "}")
			continue
		}
		if m := reWhile.FindStringSubmatch(t); m != nil {
			cond := cleanExpr(m[1], reNth)
			out = append(out, "while "+cond+" {")
			continue
		}
		if m := reFor.FindStringSubmatch(t); m != nil {
			end := strings.TrimSuffix(strings.TrimSpace(m[3]), "- 1")
			end = strings.TrimSuffix(end, "-1")
			out = append(out, fmt.Sprintf("for %s in %s..%s {", m[1], cleanExpr(m[2], reNth), cleanExpr(end, reNth)))
			continue
		}
		if m := reRef.FindStringSubmatch(t); m != nil {
			expr := cleanExpr(strings.TrimSuffix(m[2], ";;"), reNth)
			out = append(out, fmt.Sprintf("var %s = %s", m[1], strings.TrimSpace(expr)))
			continue
		}
		if m := reLet.FindStringSubmatch(t); m != nil {
			expr := cleanExpr(strings.TrimSuffix(m[2], ";;"), reNth)
			out = append(out, fmt.Sprintf("let %s = %s", m[1], strings.TrimSpace(expr)))
			continue
		}
		if m := reAssign.FindStringSubmatch(t); m != nil {
			expr := cleanExpr(strings.TrimSuffix(m[2], ";;"), reNth)
			out = append(out, fmt.Sprintf("%s = %s", m[1], strings.TrimSpace(expr)))
			continue
		}
		if strings.HasPrefix(t, "print_endline") || strings.HasPrefix(t, "print_string") || strings.HasPrefix(t, "print_int") || strings.HasPrefix(t, "print_float") {
			t = strings.TrimPrefix(t, "print_endline")
			t = strings.TrimPrefix(t, "print_string")
			t = strings.TrimPrefix(t, "print_int")
			t = strings.TrimPrefix(t, "print_float")
			t = cleanExpr(strings.TrimSpace(strings.TrimSuffix(t, ";;")), reNth)
			if strings.HasPrefix(t, "str(") && strings.HasSuffix(t, ")") {
				t = strings.TrimSuffix(strings.TrimPrefix(t, "str("), ")")
			}
			out = append(out, "print("+t+")")
		}
	}
	return out
}

func cleanExpr(expr string, reNth *regexp.Regexp) string {
	expr = strings.ReplaceAll(expr, ";", ",")
	expr = strings.ReplaceAll(expr, "string_of_int", "")
	expr = strings.ReplaceAll(expr, "string_of_float", "")
	expr = strings.ReplaceAll(expr, "string_of_bool", "")
	expr = strings.ReplaceAll(expr, "List.length", "len")
	expr = reNth.ReplaceAllString(expr, "$1[$2]")
	// Dereference operators in the generated OCaml code correspond to plain
	// variable reads in Mochi. Drop the leading '!' so that expressions like
	// `!x` become just `x` and `!(xs)` turns into `(xs)`.
	expr = strings.ReplaceAll(expr, "!(", "(")
	reDeref := regexp.MustCompile(`!([a-zA-Z_][a-zA-Z0-9_]*)`)
	expr = reDeref.ReplaceAllString(expr, "$1")
	expr = strings.ReplaceAll(expr, "not ", "!")
	expr = strings.ReplaceAll(expr, "^", "+")
	return strings.TrimSpace(expr)
}
