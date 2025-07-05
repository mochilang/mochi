package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertTypeScript converts TypeScript source code to a minimal Mochi representation using the language server.
func ConvertTypeScript(src string) ([]byte, error) {
	ls := Servers["typescript"]
	syms, hovers, diags, err := ParseTextWithHover(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms, hovers)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertTypeScriptFile reads the TS file and converts it to Mochi.
func ConvertTypeScriptFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertTypeScript(string(data))
}

// ConvertTypeScriptWithJSON converts the source and also returns the parsed
// symbols encoded as JSON.
func ConvertTypeScriptWithJSON(src string) ([]byte, []byte, error) {
	ls := Servers["typescript"]
	syms, hovers, diags, err := ParseTextWithHover(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, nil, err
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms, hovers)
	if out.Len() == 0 {
		return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return []byte(out.String()), js, nil
}

// ConvertTypeScriptFileWithJSON reads the TS file and converts it to Mochi
// while also returning the parsed symbols as JSON.
func ConvertTypeScriptFileWithJSON(path string) ([]byte, []byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	return ConvertTypeScriptWithJSON(string(data))
}

func writeTSSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, hovers map[protocol.Range]protocol.Hover) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField, protocol.SymbolKindProperty:
			if s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				out.WriteString(": any\n")
			}
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, _ := parseTSHover(hoverString(hovers[s.Range]))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
				out.WriteString(": any")
			}
			out.WriteByte(')')
			out.WriteString(" {}\n")
		}
		if len(s.Children) > 0 {
			writeTSSymbols(out, nameParts, s.Children, hovers)
		}
	}
}

func hoverString(h protocol.Hover) string {
	if mc, ok := h.Contents.(protocol.MarkupContent); ok {
		return mc.Value
	}
	return ""
}

func parseTSHover(v string) ([]string, string) {
	v = strings.TrimSpace(v)
	if strings.HasPrefix(v, "```") {
		lines := strings.Split(v, "\n")
		if len(lines) >= 2 {
			v = strings.TrimSpace(lines[1])
		}
	}
	open := strings.Index(v, "(")
	close := strings.Index(v, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := v[open+1 : close]
	var params []string
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		if colon := strings.Index(p, ":"); colon != -1 {
			p = p[:colon]
		}
		if eq := strings.Index(p, "?"); eq != -1 {
			p = p[:eq]
		}
		params = append(params, strings.TrimSpace(p))
	}
	ret := ""
	after := v[close+1:]
	if i := strings.Index(after, "=>"); i != -1 {
		ret = strings.TrimSpace(after[i+2:])
	} else if i := strings.Index(after, ":"); i != -1 {
		ret = strings.TrimSpace(after[i+1:])
	}
	return params, ret
}
