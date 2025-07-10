//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	kt "mochi/compiler/x/kotlin"
	"mochi/parser"
	"mochi/types"
)

func main() {
	pattern := filepath.Join("tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		panic(err)
	}
	outDir := filepath.Join("tests", "machine", "x", "kotlin")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		ktPath := filepath.Join(outDir, name+".kt")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(ktPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(ktPath)
			continue
		}
		code, err := kt.New(env, src).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(ktPath)
			continue
		}
		os.WriteFile(ktPath, code, 0644)
		os.Remove(errPath)
	}
}
