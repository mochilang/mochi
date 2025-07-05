package any2mochi

import (
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPhp converts php source code to Mochi using the language server.
func ConvertPhp(src string) ([]byte, error) {
	ls := Servers["php"]
	syms, _, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	// Many PHP language servers emit diagnostics for standard library
	// functions unless a full project is loaded. Ignore diagnostics so we
	// can still extract symbols from standalone files.
	var out strings.Builder
	appendPhpSymbols(&out, syms)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func appendPhpSymbols(out *strings.Builder, syms []protocol.DocumentSymbol) {
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(s.Name)
			out.WriteString("() {}\n")
			if len(s.Children) > 0 {
				appendPhpSymbols(out, s.Children)
			}
		case protocol.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {\n")
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty:
					name := strings.TrimPrefix(c.Name, "$")
					fmt.Fprintf(out, "  %s: any\n", name)
				}
			}
			out.WriteString("}\n")
			appendPhpSymbols(out, s.Children)
		default:
			if len(s.Children) > 0 {
				appendPhpSymbols(out, s.Children)
			}
		}
	}
}

// ConvertPhpFile reads the php file and converts it to Mochi.
func ConvertPhpFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPhp(string(data))
}
