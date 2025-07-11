//go:build ignore

package main

import (
	"os"
	"path/filepath"
	"strings"

	cscode "mochi/compiler/x/cs"
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
	outDir := filepath.Join("tests", "machine", "x", "cs")
	_ = os.MkdirAll(outDir, 0755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		errPath := filepath.Join(outDir, name+".error")
		csPath := filepath.Join(outDir, name+".cs")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(csPath)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
			os.Remove(csPath)
			continue
		}
		comp := cscode.New(env)
		if ns := os.Getenv("CS_NAMESPACE"); ns != "" {
			comp.Namespace = ns
		}
		code, err := comp.Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0644)
			os.Remove(csPath)
			continue
		}
		os.WriteFile(csPath, code, 0644)
		os.Remove(errPath)
	}
}
