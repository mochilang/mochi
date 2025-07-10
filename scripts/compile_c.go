//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	ccode "mochi/compiler/x/c"
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
	outDir := filepath.Join("tests", "machine", "x", "c")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		cPath := filepath.Join(outDir, name+".c")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(cPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(cPath)
			continue
		}
		code, err := ccode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(cPath)
			continue
		}
		os.WriteFile(cPath, code, 0644)
		os.Remove(errPath)
	}
}
