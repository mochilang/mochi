//go:build slow

package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	rb "mochi/transpiler/x/rb"
	"mochi/types"
)

func main() {
	if len(os.Args) < 2 {
		panic("usage: transpile_rb <files>")
	}
	root, _ := os.Getwd()
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rb")
	os.MkdirAll(outDir, 0o755)
	for _, src := range os.Args[1:] {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
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
		rbFile := filepath.Join(outDir, name+".rb")
		os.WriteFile(rbFile, buf.Bytes(), 0o644)
		cmd := exec.Command("ruby", rbFile)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		out, err := cmd.CombinedOutput()
		if err != nil {
			os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			continue
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		trimmed := bytes.TrimSpace(out)
		os.WriteFile(filepath.Join(outDir, name+".out"), trimmed, 0o644)
	}
}
