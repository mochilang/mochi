package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertScheme converts scheme source code to Mochi using the language server.
func ConvertScheme(src string) ([]byte, error) {
	ls := Servers["scheme"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeSchemeSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
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
			out.WriteString(" {}\n")
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
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
