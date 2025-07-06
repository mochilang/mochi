package scheme

import (
	any2mochi "mochi/tools/any2mochi"

	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// Convert parses Scheme source code and emits Mochi stubs using the language server when available.
func Convert(src string) ([]byte, error) {
	ls := any2mochi.Servers["scheme"]
	var syms []any2mochi.DocumentSymbol
	var diags []any2mochi.Diagnostic
	if _, lookErr := exec.LookPath(ls.Command); lookErr == nil {
		syms, diags, _ = any2mochi.ParseText(ls.Command, ls.Args, ls.LangID, src)
	}
	var out strings.Builder
	if syms != nil {
		writeSymbols(&out, nil, syms, src, ls)
	}
	if out.Len() == 0 {
		if len(diags) > 0 {
			return nil, fmt.Errorf("%s", diagnostics(src, diags))
		}
		// fall back to the simple CLI-based parser
		if simple, err := convertSimple(src); err == nil && len(simple) > 0 {
			return simple, nil
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the Scheme file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	n := len(lines)
	if n > 5 {
		n = 5
	}
	out := make([]string, n)
	for i := 0; i < n; i++ {
		prefix := "   "
		if i == 0 {
			prefix = "-> "
		}
		out[i] = fmt.Sprintf("%s%3d| %s", prefix, i+1, lines[i])
	}
	return strings.Join(out, "\n")
}

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		start := int(d.Range.Start.Line)
		msg := d.Message
		out.WriteString(fmt.Sprintf("line %d: %s\n", start+1, msg))
		for i := start - 1; i <= start+1; i++ {
			if i < 0 || i >= len(lines) {
				continue
			}
			prefix := "  "
			if i == start {
				prefix = "->"
			}
			out.WriteString(fmt.Sprintf("%s %3d| %s\n", prefix, i+1, strings.TrimSpace(lines[i])))
		}
	}
	return strings.TrimSpace(out.String())
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction:
			params, ret := parseSignature(s.Detail)
			if len(params) == 0 {
				params = extractParams(s)
			}
			if len(params) == 0 || ret == "" {
				if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseHoverSignature(hov)
					if len(params) == 0 {
						params = p
					}
					if ret == "" {
						ret = r
					}
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range body {
					out.WriteString("  ")
					out.WriteString(ln)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []any2mochi.DocumentSymbol{}
			rest := []any2mochi.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case any2mochi.SymbolKindField:
					fields = append(fields, c)
				default:
					rest = append(rest, c)
				}
			}
			if len(fields) == 0 && len(rest) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					out.WriteString("  ")
					out.WriteString(f.Name)
					typ := parseVarType(f.Detail)
					if typ == "" {
						if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, f.SelectionRange.Start); err == nil {
							typ = parseHoverVarType(hov)
						}
					}
					if typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
				if len(rest) > 0 {
					writeSymbols(out, nameParts, rest, src, ls)
				}
			}
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := parseVarType(s.Detail)
				if typ == "" {
					if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
						typ = parseHoverVarType(hov)
					}
				}
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		if len(s.Children) > 0 {
			writeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func parseParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	return parseParamString(*detail)
}

func parseSignature(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	params := parseParamString(d)
	ret := ""
	if idx := strings.LastIndex(d, ")"); idx != -1 && idx+1 < len(d) {
		rest := strings.TrimSpace(d[idx+1:])
		if strings.HasPrefix(rest, "->") {
			ret = strings.TrimSpace(rest[2:])
		} else if strings.HasPrefix(rest, ":") {
			ret = strings.TrimSpace(rest[1:])
		}
	}
	return params, ret
}

func extractParams(sym any2mochi.DocumentSymbol) []string {
	var params []string
	start := sym.Range.Start.Line
	for _, c := range sym.Children {
		if c.Kind == any2mochi.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func parseHoverParams(h any2mochi.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "(") && strings.Contains(line, ")") {
			if p := parseParamString(line); len(p) > 0 {
				return p
			}
		}
	}
	return nil
}

func parseHoverSignature(h any2mochi.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if !strings.Contains(line, "(") || !strings.Contains(line, ")") {
			continue
		}
		params := parseParamString(line)
		if params == nil {
			continue
		}
		ret := ""
		if idx := strings.LastIndex(line, ")"); idx != -1 && idx+1 < len(line) {
			rest := strings.TrimSpace(line[idx+1:])
			if strings.HasPrefix(rest, "->") {
				ret = strings.TrimSpace(rest[2:])
			} else if strings.HasPrefix(rest, ":") {
				ret = strings.TrimSpace(rest[1:])
			}
		}
		return params, ret
	}
	return nil, ""
}

func parseParamString(line string) []string {
	content := firstParenContent(line)
	if content == "" {
		return nil
	}
	fields := strings.Fields(content)
	params := make([]string, 0, len(fields))
	for _, f := range fields {
		f = strings.TrimLeft(f, "#:")
		if f != "" {
			params = append(params, f)
		}
	}
	return params
}

func firstParenContent(s string) string {
	start := strings.Index(s, "(")
	if start == -1 {
		return ""
	}
	depth := 0
	for i := start; i < len(s); i++ {
		switch s[i] {
		case '(':
			if depth == 0 {
				start = i
			}
			depth++
		case ')':
			depth--
			if depth == 0 {
				return s[start+1 : i]
			}
		}
	}
	return ""
}

func parseVarType(detail *string) string {
	if detail == nil {
		return ""
	}
	d := strings.TrimSpace(*detail)
	if idx := strings.Index(d, ":"); idx != -1 && idx+1 < len(d) {
		return strings.TrimSpace(d[idx+1:])
	}
	return ""
}

func parseHoverVarType(h any2mochi.Hover) string {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, ":") && !strings.Contains(line, "(") {
			if idx := strings.Index(line, ":"); idx != -1 {
				return strings.TrimSpace(line[idx+1:])
			}
		}
	}
	return ""
}

func parseFunctionBody(src string, sym any2mochi.DocumentSymbol) []string {
	code := rangeText(src, sym.Range)
	// find the first closing parenthesis of the parameter list
	idx := strings.Index(code, "(define")
	if idx == -1 {
		return nil
	}
	rest := code[idx+len("(define"):]
	// skip name and params
	if p := strings.Index(rest, ")"); p != -1 {
		rest = strings.TrimSpace(rest[p+1:])
	}
	if strings.HasPrefix(rest, "(") && strings.HasSuffix(strings.TrimSpace(rest), ")") {
		rest = strings.TrimSuffix(strings.TrimPrefix(strings.TrimSpace(rest), "("), ")")
	}
	var out []string
	lines := strings.Split(rest, "\n")
	for i, line := range lines {
		l := strings.TrimSpace(line)
		if l == "" || strings.HasPrefix(l, ";") {
			continue
		}
		switch {
		case strings.HasPrefix(l, "(display"):
			arg := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "(display"), ")"))
			out = append(out, "print("+convertExpr(arg)+")")
		case strings.HasPrefix(l, "(set!"):
			inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "(set!"), ")"))
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				out = append(out, parts[0]+" = "+convertExpr(strings.Join(parts[1:], " ")))
			}
		case strings.HasPrefix(l, "(define"):
			inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "(define"), ")"))
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				out = append(out, "let "+parts[0]+" = "+convertExpr(strings.Join(parts[1:], " ")))
			}
		default:
			expr := convertExpr(l)
			if i == len(lines)-1 {
				out = append(out, "return "+expr)
			} else {
				out = append(out, expr)
			}
		}
	}
	return out
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "(") && strings.HasSuffix(expr, ")") {
		inner := strings.TrimSpace(expr[1 : len(expr)-1])
		parts := strings.Fields(inner)
		if len(parts) == 0 {
			return inner
		}
		switch parts[0] {
		case "+", "-", "*", "/":
			if len(parts) == 3 {
				return parts[1] + " " + parts[0] + " " + parts[2]
			}
		case "display":
			return "print(" + strings.Join(parts[1:], " ") + ")"
		case "list":
			if len(parts) > 1 {
				return "[" + strings.Join(parts[1:], ", ") + "]"
			}
		}
		return inner
	}
	if expr == "'()" {
		return "[]"
	}
	return expr
}

func rangeText(src string, r any2mochi.Range) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line) && i < len(lines); i++ {
		line := lines[i]
		if i == int(r.Start.Line) && int(r.Start.Character) < len(line) {
			line = line[int(r.Start.Character):]
		}
		if i == int(r.End.Line) && int(r.End.Character) <= len(line) {
			line = line[:int(r.End.Character)]
		}
		out.WriteString(line)
		if i != int(r.End.Line) {
			out.WriteByte('\n')
		}
	}
	return out.String()
}
