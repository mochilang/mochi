package main

import (
	"bytes"
	"mochi/parser"
	pl "mochi/transpiler/x/pl"
	"mochi/types"
	"os"
	"os/exec"
	"path/filepath"
)

func main() {
	if len(os.Args) != 3 {
		panic("usage: gen_pl <src> <outdir>")
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		panic(err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		panic(err)
	}
	base := filepath.Base(src)
	name := base[:len(base)-len(".mochi")]
	plFile := filepath.Join(outDir, name+".pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		panic(err)
	}
	// Run swipl
	out, err := exec.Command("swipl", "-q", "-f", plFile).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
	} else {
		os.Remove(filepath.Join(outDir, name+".error"))
	}
	os.WriteFile(filepath.Join(outDir, name+".out"), bytes.TrimSpace(out), 0o644)
}
