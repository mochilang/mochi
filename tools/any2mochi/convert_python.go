package any2mochi

import (
	"os"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPython converts Python source code to a minimal Mochi representation using pyright.
func ConvertPython(src string) ([]byte, error) {
	syms, err := ParseText("pyright-langserver", []string{"--stdio"}, "python", src)
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

// ConvertPythonFile reads the Python file at path and converts it to Mochi.
func ConvertPythonFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPython(string(data))
}
