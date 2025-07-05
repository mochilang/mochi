package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertFortran converts Fortran source code to a minimal Mochi representation
// using the fortls language server.
func ConvertFortran(src string) ([]byte, error) {
	return convertFortran(src, "")
}

func convertFortran(src, root string) ([]byte, error) {
	ls := Servers["fortran"]
	syms, diags, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeFtSymbols(&out, nil, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFortranFile reads the Fortran file and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertFortran(string(data), filepath.Dir(path))
}

func writeFtSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindModule, protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("() {}\n")
		}
		if len(s.Children) > 0 {
			writeFtSymbols(out, nameParts, s.Children)
		}
	}
}
