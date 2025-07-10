//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	cpp "mochi/compiler/x/cpp"
	"mochi/parser"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	if p := os.Getenv("PATTERN"); p != "" {
		pattern = p
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "cpp")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		cppPath := filepath.Join(outDir, name+".cpp")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(cppPath)
			continue
		}
		code, err := cpp.New().Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(cppPath)
			continue
		}
		os.WriteFile(cppPath, code, 0644)
		os.Remove(errPath)
	}
}
