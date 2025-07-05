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
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms)
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
	syms, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, nil, err
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTSSymbols(&out, nil, syms)
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

func writeTSSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol) {
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
			out.WriteString("() {}\n")
		}
		if len(s.Children) > 0 {
			writeTSSymbols(out, nameParts, s.Children)
		}
	}
}
