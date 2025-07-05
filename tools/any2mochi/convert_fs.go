package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertFs converts F# source code to Mochi using a simple Go translator.
// The F# code is first parsed via its language server to surface any
// diagnostics before translation.
func ConvertFs(src string) ([]byte, error) {
	ls := Servers["fs"]
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeFsSymbols(&out, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFsFile reads the fs file and converts it to Mochi.
func ConvertFsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFs(string(data))
}

type fsParam struct{}

func writeFsSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("() {}\n")
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children)
			}
		case protocol.SymbolKindClass, protocol.SymbolKindStruct, protocol.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children)
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant, protocol.SymbolKindField:
			if len(prefix) == 0 {
				out.WriteString("let ")
				out.WriteString(strings.Join(nameParts, "."))
				out.WriteByte('\n')
			}
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children)
			}
		default:
			if len(s.Children) > 0 {
				writeFsSymbols(out, nameParts, s.Children)
			}
		}
	}
}
