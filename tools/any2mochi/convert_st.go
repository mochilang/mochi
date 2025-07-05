package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertSt converts st source code to Mochi using the language server.
func ConvertSt(src string) ([]byte, error) {
	ls := Servers["st"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}

	var out strings.Builder
	appendStSymbols(&out, syms, src, ls)

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func appendStSymbols(out *strings.Builder, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children, src, ls)
			}
		case protocol.SymbolKindMethod, protocol.SymbolKindConstructor, protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(cleanStName(s.Name))
			params := extractStParams(s)
			if len(params) == 0 {
				params = getStHoverParams(src, s.SelectionRange.Start, ls)
			}
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {}\n")
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children, src, ls)
			}
		default:
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children, src, ls)
			}
		}
	}
}

func cleanStName(name string) string {
	if i := strings.Index(name, ":"); i != -1 {
		return name[:i]
	}
	return name
}

func extractStParams(sym protocol.DocumentSymbol) []string {
	start := sym.Range.Start.Line
	var params []string
	for _, c := range sym.Children {
		if c.Kind == protocol.SymbolKindVariable && c.Range.Start.Line == start {
			if c.Name != "" {
				params = append(params, c.Name)
			}
		}
	}
	return params
}

func getStHoverParams(src string, pos protocol.Position, ls LanguageServer) []string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil
	}
	text := hoverString(hov)
	text = strings.ReplaceAll(text, "\n", " ")
	parts := strings.Split(text, ":")
	var params []string
	for i := 1; i < len(parts); i++ {
		sec := strings.TrimSpace(parts[i])
		if sec == "" {
			continue
		}
		fields := strings.Fields(sec)
		if len(fields) > 0 {
			p := strings.Trim(fields[0], "()|")
			if p != "" {
				params = append(params, p)
			}
		}
	}
	return params
}

// ConvertStFile reads the st file and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}
