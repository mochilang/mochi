package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertSwift converts swift source code to Mochi using the language server.
func ConvertSwift(src string) ([]byte, error) {
	ls := Servers["swift"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeSwiftSymbols(&out, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func writeSwiftSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("() {}\n")
		}
		if len(s.Children) > 0 {
			writeSwiftSymbols(out, nameParts, s.Children)
		}
	}
}

// ConvertSwiftFile reads the swift file and converts it to Mochi.
func ConvertSwiftFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSwift(string(data))
}
