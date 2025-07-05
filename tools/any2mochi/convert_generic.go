package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertWithServer converts source code using the provided language server configuration.
func ConvertWithServer(cmd string, args []string, langID, src string) ([]byte, error) {
	syms, err := ParseAndEnsure(cmd, args, langID, src)
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

// ConvertSource converts source written in the specified language to Mochi using
// the default server from Servers.
func ConvertSource(lang, src string) ([]byte, error) {
	ls, ok := Servers[lang]
	if !ok {
		return nil, fmt.Errorf("no language server for %s", lang)
	}
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertFile attempts to detect the language from the file extension and
// converts the file contents using ConvertSource.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	lang := DetectLanguage(path, string(data))
	if lang == "" {
		return nil, fmt.Errorf("unable to detect language for %s", path)
	}
	return ConvertSource(lang, string(data))
}

// DetectLanguage returns a language key based on the filename extension or
// simple textual heuristics if the extension is unknown.
func DetectLanguage(name, src string) string {
	ext := strings.ToLower(filepath.Ext(name))
	switch ext {
	case ".go":
		return "go"
	case ".py":
		return "python"
	case ".ts":
		return "typescript"
	case ".rs":
		return "rust"
	case ".c":
		return "c"
	case ".cpp", ".cc", ".cxx", ".hpp", ".h":
		return "cpp"
	case ".cs":
		return "cs"
	case ".java":
		return "java"
	case ".kt":
		return "kt"
	case ".lua":
		return "lua"
	case ".dart":
		return "dart"
	case ".php":
		return "php"
	case ".swift":
		return "swift"
	case ".rb":
		return "rb"
	case ".scala":
		return "scala"
	case ".mlir":
		return "mlir"
	case ".ex":
		return "ex"
	case ".erl":
		return "erlang"
	case ".fs":
		return "fs"
	case ".hs":
		return "hs"
	case ".ml", ".mli":
		return "ocaml"
	case ".pas":
		return "pas"
	case ".pl":
		return "pl"
	case ".rkt":
		return "rkt"
	case ".zig":
		return "zig"
	}
	if strings.Contains(src, "package ") && strings.Contains(src, "func ") {
		return "go"
	}
	if strings.Contains(src, "fn main") {
		return "rust"
	}
	if strings.Contains(src, "def ") {
		return "python"
	}
	return ""
}

// ParseAndEnsure runs ParseText after ensuring the language server is installed.
func ParseAndEnsure(cmd string, args []string, langID, src string) ([]protocol.DocumentSymbol, error) {
	if err := EnsureServer(cmd); err != nil {
		return nil, err
	}
	return ParseText(cmd, args, langID, src)
}
