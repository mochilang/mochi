package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertTypeScript converts TypeScript source code to Mochi by
// querying the language server for document symbols and formatting
// them into simple Mochi declarations.
func ConvertTypeScript(src string) ([]byte, error) {
	ls := Servers["typescript"]
	syms, diags, err := ParseAndEnsure(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, fmt.Errorf("convert failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	code := formatTSSymbols(syms)
	if code == "" {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(code), nil
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
	syms, diags, err := ParseAndEnsure(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, nil, fmt.Errorf("convert failure: %w\n\nsource snippet:\n%s", err, numberedSnippet(src))
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	code := formatTSSymbols(syms)
	if code == "" {
		return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return []byte(code), js, nil
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

func formatTSSymbols(syms []protocol.DocumentSymbol) string {
	var out strings.Builder
	for _, s := range syms {
		switch s.Kind {
		case protocol.SymbolKindFunction:
			out.WriteString("fun ")
			out.WriteString(s.Name)
			out.WriteString("() {}\n")
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			out.WriteString("let ")
			out.WriteString(s.Name)
			out.WriteByte('\n')
		case protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(s.Name)
			out.WriteString(" {}\n")
		}
	}
	return out.String()
}
