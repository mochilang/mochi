//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	rb "mochi/transpiler/x/rb"
	"mochi/types"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("usage: genrb <src> <outdir>")
		os.Exit(1)
	}
	src := os.Args[1]
	outDir := os.Args[2]
	prog, err := parser.Parse(src)
	if err != nil {
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		panic(errs[0])
	}
	ast, err := rb.Transpile(prog, env)
	if err != nil {
		panic(err)
	}
	var buf bytes.Buffer
	if err := rb.Emit(&buf, ast); err != nil {
		panic(err)
	}
	base := filepath.Base(src)
	name := base[:len(base)-len(filepath.Ext(base))]
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		panic(err)
	}
	rbFile := filepath.Join(outDir, name+".rb")
	if err := os.WriteFile(rbFile, buf.Bytes(), 0o644); err != nil {
		panic(err)
	}
	if outData, err := os.ReadFile(filepath.Join(filepath.Dir(src), name+".out")); err == nil {
		os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(outData), 0o644)
	}
}
