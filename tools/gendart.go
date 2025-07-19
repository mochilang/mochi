package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	dart "mochi/transpiler/x/dart"
	"mochi/types"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("usage: gendart <src> <outdir>")
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
	ast, err := dart.Transpile(prog, env)
	if err != nil {
		panic(err)
	}
	var buf bytes.Buffer
	if err := dart.Emit(&buf, ast); err != nil {
		panic(err)
	}
	base := filepath.Base(src)
	name := base[:len(base)-len(filepath.Ext(base))]
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		panic(err)
	}
	dartFile := filepath.Join(outDir, name+".dart")
	if err := os.WriteFile(dartFile, buf.Bytes(), 0o644); err != nil {
		panic(err)
	}
	if outData, err := os.ReadFile(filepath.Join(filepath.Dir(src), name+".out")); err == nil {
		os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(outData), 0o644)
	}
}
