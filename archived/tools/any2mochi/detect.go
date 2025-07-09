//go:build slow

package any2mochi

import (
	"path/filepath"
	"strings"
)

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
	case ".f90", ".f95", ".f", ".for":
		return "fortran"
	case ".pro", ".prolog", ".plg":
		return "prolog"
	case ".pl":
		return "pl"
	case ".rkt":
		return "rkt"
	case ".zig":
		return "zig"
	case ".clj":
		return "clj"
	case ".cob":
		return "cobol"
	case ".s":
		return "asm"
	case ".scm", ".ss":
		return "scheme"
	case ".st":
		return "st"
	case ".jvm":
		return "jvm"
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
