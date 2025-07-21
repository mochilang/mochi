package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		panic("source file required")
	}
	src := os.Args[1]
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	outDir := filepath.Join("tests", "transpiler", "x", "fs")
	os.MkdirAll(outDir, 0o755)
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	codePath := filepath.Join(outDir, base+".fs")

	prog, err := parser.Parse(src)
	if err != nil {
		os.WriteFile(errPath, []byte(err.Error()), 0o644)
		panic(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		panic(errs[0])
	}
	ast, err := fstrans.Transpile(prog, env)
	if err != nil {
		os.WriteFile(errPath, []byte(err.Error()), 0o644)
		panic(err)
	}
	code := fstrans.Emit(ast)
	os.WriteFile(codePath, code, 0o644)
	exe := filepath.Join(outDir, base+".exe")
	cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exe, codePath)
	if out, err := cmd.CombinedOutput(); err != nil {
		os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		panic(err)
	}
	run := exec.Command("mono", exe)
	out, err := run.CombinedOutput()
	if err != nil {
		os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		panic(err)
	}
	outBytes := bytes.TrimSpace(out)
	os.WriteFile(outPath, outBytes, 0o644)
	os.Remove(errPath)
}
