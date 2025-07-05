package any2mochi

import (
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertGo converts Go source code to a minimal Mochi representation using
// symbols reported by gopls.
func ConvertGo(src string) ([]byte, error) {
	syms, err := ParseText("gopls", nil, "go", src)
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

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}
