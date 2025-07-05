package any2mochi

import (
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertTypeScript converts TypeScript source code to a minimal Mochi representation using the language server.
func ConvertTypeScript(src string) ([]byte, error) {
	syms, err := ParseText("typescript-language-server", []string{"--stdio"}, "typescript", src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, s := range syms {
		if s.Kind != protocol.SymbolKindFunction {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(s.Name)
		out.WriteString("() {}\n")
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
