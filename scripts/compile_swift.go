//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	swift "mochi/compiler/x/swift"
	"mochi/parser"
	"mochi/types"
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
	outDir := filepath.Join("tests", "machine", "x", "swift")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		swiftPath := filepath.Join(outDir, name+".swift")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(swiftPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(swiftPath)
			continue
		}
		code, err := swift.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(swiftPath)
			continue
		}
		os.WriteFile(swiftPath, code, 0644)
		os.Remove(errPath)
	}
}
