package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertRkt converts Racket source code using the language server. It extracts
// top level definitions from the LSP symbols.  Parameter and return type
// information is obtained from the symbol detail string or from hover text
// provided by the server.  Only information returned by the language server is
// used -- no regex parsing or fallbacks.
func ConvertRkt(src string) ([]byte, error) {
	ls := Servers["rkt"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction:
			params, ret := parseRktSignature(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					params, ret = parseRktHoverSignature(hov)
				}
			}
			out.WriteString("fun ")
			out.WriteString(s.Name)
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" && ret != "Void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindStruct, protocol.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(s.Name)
			if len(s.Children) == 0 {
				out.WriteString(" {}\n")
				continue
			}
			out.WriteString(" {\n")
			for _, f := range s.Children {
				if f.Kind != protocol.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(f.Name)
				if typ := parseRktVarType(f.Detail); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(s.Children) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if typ := parseRktVarType(s.Detail); typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertRktFile reads the rkt file and converts it to Mochi.
func ConvertRktFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRkt(string(data))
}

func parseRktParams(detail *string) []string {
	if detail == nil {
		return nil
	}
	d := *detail
	open := strings.Index(d, "(")
	close := strings.Index(d, ")")
	if open == -1 || close == -1 || close <= open {
		return nil
	}
	list := strings.TrimSpace(d[open+1 : close])
	if list == "" {
		return nil
	}
	return strings.Fields(list)
}

func parseRktHoverParams(h protocol.Hover) []string {
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
		open := strings.Index(line, "(")
		close := strings.Index(line, ")")
		if open == -1 || close == -1 || close <= open {
			continue
		}
		list := strings.TrimSpace(line[open+1 : close])
		if list == "" {
			continue
		}
		return strings.Fields(list)
	}
	return nil
}

func parseRktSignature(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	open := strings.Index(d, "(")
	close := strings.Index(d, ")")
	if open == -1 || close == -1 || close <= open {
		return nil, ""
	}
	params := strings.Fields(strings.TrimSpace(d[open+1 : close]))
	rest := strings.TrimSpace(d[close+1:])
	ret := ""
	if strings.HasPrefix(rest, "->") {
		ret = strings.TrimSpace(rest[2:])
	}
	return params, ret
}

func parseRktHoverSignature(h protocol.Hover) ([]string, string) {
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
			open := strings.Index(line, "(")
			close := strings.Index(line, ")")
			if open != -1 && close != -1 && close > open {
				params := strings.Fields(strings.TrimSpace(line[open+1 : close]))
				rest := strings.TrimSpace(line[close+1:])
				ret := ""
				if strings.HasPrefix(rest, "->") {
					ret = strings.TrimSpace(rest[2:])
				}
				if len(params) > 0 || ret != "" {
					return params, ret
				}
			}
		}
	}
	return nil, ""
}

func parseRktVarType(detail *string) string {
	if detail == nil {
		return ""
	}
	if idx := strings.Index(*detail, ":"); idx != -1 {
		return strings.TrimSpace((*detail)[idx+1:])
	}
	return ""
}
