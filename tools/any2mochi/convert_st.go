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
	appendStSymbols(&out, syms)

	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func appendStSymbols(out *strings.Builder, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children)
			}
		case protocol.SymbolKindMethod, protocol.SymbolKindConstructor, protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(cleanStName(s.Name))
			out.WriteString("() {}\n")
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children)
			}
		default:
			if len(s.Children) > 0 {
				appendStSymbols(out, s.Children)
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

// ConvertStFile reads the st file and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}
