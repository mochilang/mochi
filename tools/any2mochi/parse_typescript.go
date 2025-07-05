package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ParseTypeScript parses the given source using the TypeScript language server.
func ParseTypeScript(src string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	ls := Servers["typescript"]
	return ParseAndEnsure(ls.Command, ls.Args, ls.LangID, src)
}

// ParseTypeScriptFile reads and parses the file at path using the TypeScript language server.
func ParseTypeScriptFile(path string) ([]protocol.DocumentSymbol, []protocol.Diagnostic, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	return ParseTypeScript(string(data))
}

// ParseTypeScriptFileJSON parses the file and returns the document symbols encoded as pretty JSON.
func ParseTypeScriptFileJSON(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	syms, diags, err := ParseTypeScript(string(data))
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(string(data), diags))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return js, nil
}
