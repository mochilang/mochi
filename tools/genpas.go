//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/parser"
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

func main() {
	if len(os.Args) != 3 {
		fmt.Println("usage: genpas <src> <outdir>")
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
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		panic(err)
	}
	code := ast.Emit()
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		panic(err)
	}
	base := filepath.Base(src)
	name := base[:len(base)-len(filepath.Ext(base))]
	pasPath := filepath.Join(outDir, name+".pas")
	if err := os.WriteFile(pasPath, code, 0o644); err != nil {
		panic(err)
	}
	// attempt to compile and run if fpc is available
	if fpc, err := exec.LookPath("fpc"); err == nil {
		exe := filepath.Join(outDir, name)
		out, err := exec.Command(fpc, pasPath, "-o"+exe).CombinedOutput()
		if err != nil {
			os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		} else {
			runOut, runErr := exec.Command(exe).CombinedOutput()
			if runErr != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), runOut, 0o644)
			} else {
				os.Remove(filepath.Join(outDir, name+".error"))
				os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(runOut), 0o644)
			}
		}
	}
	if out, err := os.ReadFile(filepath.Join(filepath.Dir(src), name+".out")); err == nil {
		os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0o644)
	}
}
