package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertRkt converts rkt source code using the language server. It extracts
// function definitions from the LSP symbols and obtains parameter information
// from the symbol detail or hover text provided by the server. This keeps the
// converter lightweight while relying solely on LSP output.
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
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		params := parseRktParams(s.Detail)
		if len(params) == 0 {
			if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
				params = parseRktHoverParams(hov)
			}
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteByte('(')
		if len(params) > 0 {
			out.WriteString(strings.Join(params, ", "))
		}
		out.WriteString(") {}\n")
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
