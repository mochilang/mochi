package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertScheme converts scheme source code to Mochi using the language server.
func ConvertScheme(src string) ([]byte, error) {
	ls := Servers["scheme"]
	var syms []protocol.DocumentSymbol
	var diags []protocol.Diagnostic
	if _, lookErr := exec.LookPath(ls.Command); lookErr == nil {
		syms, diags, _ = ParseText(ls.Command, ls.Args, ls.LangID, src)
	}
	var out strings.Builder
	if syms != nil {
		writeSchemeSymbols(&out, nil, syms, src, ls)
	}
	if out.Len() == 0 {
		// basic regex fallback for simple defines
		lines := strings.Split(src, "\n")
		for i, line := range lines {
			line = strings.TrimSpace(line)
			if strings.HasPrefix(line, "(define ") && strings.Contains(line, "(") {
				nameStart := strings.Index(line, "(") + 1
				nameEnd := strings.Index(line[nameStart:], " ")
				if nameEnd != -1 {
					name := line[nameStart : nameStart+nameEnd]
					ds := protocol.DocumentSymbol{Kind: protocol.SymbolKindFunction, Name: name,
						Range: protocol.Range{Start: protocol.Position{Line: uint32(i), Character: 0}, End: protocol.Position{Line: uint32(i), Character: 0}}}
					body := parseSchemeFunctionBody(src, ds)
					out.WriteString("fun ")
					out.WriteString(name)
					out.WriteString("()")
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
				}
			}
		}
	}
	if out.Len() == 0 {
		if len(diags) > 0 {
			return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertSchemeFile reads the scheme file and converts it to Mochi.
func ConvertSchemeFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScheme(string(data))
}

func writeSchemeSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction:
			params, ret := parseSchemeSignature(s.Detail)
			if len(params) == 0 {
				params = extractSchemeParams(s)
			}
			if len(params) == 0 || ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseSchemeHoverSignature(hov)
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
			body := parseSchemeFunctionBody(src, s)
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
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			rest := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField:
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
					typ := parseSchemeVarType(f.Detail)
					if typ == "" {
						if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, f.SelectionRange.Start); err == nil {
							typ = parseSchemeHoverVarType(hov)
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
					writeSchemeSymbols(out, nameParts, rest, src, ls)
				}
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				typ := parseSchemeVarType(s.Detail)
				if typ == "" {
					if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
						typ = parseSchemeHoverVarType(hov)
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
			writeSchemeSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func parseSchemeParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	return parseSchemeParamString(*detail)
}

func parseSchemeSignature(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	params := parseSchemeParamString(d)
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

func extractSchemeParams(sym protocol.DocumentSymbol) []string {
	var params []string
	start := sym.Range.Start.Line
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			params = append(params, c.Name)
		}
	}
	return params
}

func parseSchemeHoverParams(h protocol.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case protocol.MarkupContent:
		text = c.Value
	case protocol.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m protocol.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []protocol.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m protocol.MarkedStringStruct
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
			if p := parseSchemeParamString(line); len(p) > 0 {
				return p
			}
		}
	}
	return nil
}

func parseSchemeHoverSignature(h protocol.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case protocol.MarkupContent:
		text = c.Value
	case protocol.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m protocol.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []protocol.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m protocol.MarkedStringStruct
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
		params := parseSchemeParamString(line)
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

func parseSchemeParamString(line string) []string {
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

func parseSchemeVarType(detail *string) string {
	if detail == nil {
		return ""
	}
	d := strings.TrimSpace(*detail)
	if idx := strings.Index(d, ":"); idx != -1 && idx+1 < len(d) {
		return strings.TrimSpace(d[idx+1:])
	}
	return ""
}

func parseSchemeHoverVarType(h protocol.Hover) string {
	var text string
	switch c := h.Contents.(type) {
	case protocol.MarkupContent:
		text = c.Value
	case protocol.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m protocol.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []protocol.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m protocol.MarkedStringStruct
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

func parseSchemeFunctionBody(src string, sym protocol.DocumentSymbol) []string {
	code := schemeRange(src, sym.Range)
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
			out = append(out, "print("+convertSchemeExpr(arg)+")")
		case strings.HasPrefix(l, "(set!"):
			inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "(set!"), ")"))
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				out = append(out, parts[0]+" = "+convertSchemeExpr(strings.Join(parts[1:], " ")))
			}
		case strings.HasPrefix(l, "(define"):
			inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "(define"), ")"))
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				out = append(out, "let "+parts[0]+" = "+convertSchemeExpr(strings.Join(parts[1:], " ")))
			}
		default:
			expr := convertSchemeExpr(l)
			if i == len(lines)-1 {
				out = append(out, "return "+expr)
			} else {
				out = append(out, expr)
			}
		}
	}
	return out
}

func convertSchemeExpr(expr string) string {
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
		}
		return inner
	}
	return expr
}

func schemeRange(src string, r protocol.Range) string {
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
