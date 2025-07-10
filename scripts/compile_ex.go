//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "ex")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		exsPath := filepath.Join(outDir, name+".exs")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(exsPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(exsPath)
			continue
		}
		code, err := excode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(exsPath)
			continue
		}
		os.WriteFile(exsPath, code, 0644)
		os.Remove(errPath)
	}
}
