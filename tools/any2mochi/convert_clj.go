package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertClj converts Clojure source code to Mochi using the language server.
func ConvertClj(src string) ([]byte, error) {
	ls := Servers["clj"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeCljSymbols(&out, nil, syms, ls, src)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeCljSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, ls LanguageServer, src string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := parseCljParams(s.Detail)
			if len(params) == 0 {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseCljHoverSignature(hov)
					if len(p) > 0 {
						params = p
					}
					if r != "" {
						ret = r
					}
				}
			}
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
		case protocol.SymbolKindClass, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("\n")
		}
		if len(s.Children) > 0 {
			writeCljSymbols(out, nameParts, s.Children, ls, src)
		}
	}
}

func parseCljHoverParams(h protocol.Hover) []string {
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
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		if list == "" {
			continue
		}
		fields := strings.Fields(list)
		params := make([]string, 0, len(fields))
		for _, f := range fields {
			if strings.HasPrefix(f, "&") {
				f = strings.TrimPrefix(f, "&")
			}
			if i := strings.IndexAny(f, ":"); i != -1 {
				f = f[:i]
			}
			params = append(params, f)
		}
		if len(params) > 0 {
			return params
		}
	}
	return nil
}

func parseCljHoverSignature(h protocol.Hover) ([]string, string) {
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
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		params := []string{}
		if list != "" {
			fields := strings.Fields(list)
			for _, f := range fields {
				if strings.HasPrefix(f, "&") {
					f = strings.TrimPrefix(f, "&")
				}
				if i := strings.IndexAny(f, ":"); i != -1 {
					f = f[:i]
				}
				params = append(params, f)
			}
		}
		ret := strings.TrimSpace(line[cidx+1:])
		if strings.HasPrefix(ret, "->") {
			ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
		}
		return params, ret
	}
	return nil, ""
}

func parseCljParams(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := *detail
	start := strings.Index(d, "[")
	end := strings.Index(d, "]")
	if start == -1 || end == -1 || end <= start {
		return nil, ""
	}
	list := strings.TrimSpace(d[start+1 : end])
	if list == "" {
		return nil, ""
	}
	fields := strings.Fields(list)
	params := make([]string, 0, len(fields))
	for _, f := range fields {
		if strings.HasPrefix(f, "&") {
			f = strings.TrimPrefix(f, "&")
		}
		params = append(params, f)
	}
	ret := strings.TrimSpace(d[end+1:])
	if strings.HasPrefix(ret, "->") {
		ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
	}
	return params, ret
}

// ConvertCljFile reads the Clojure file and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}
