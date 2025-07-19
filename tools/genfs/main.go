package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func main() {
	flag.Parse()
	if flag.NArg() == 0 {
		fmt.Println("usage: genfs FILE...")
		os.Exit(1)
	}
	for _, src := range flag.Args() {
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintf(os.Stderr, "parse %s: %v\n", src, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintf(os.Stderr, "type error %s: %v\n", src, errs[0])
			continue
		}
		ast, err := fstrans.Transpile(prog, env)
		if err != nil {
			fmt.Fprintf(os.Stderr, "transpile %s: %v\n", src, err)
			continue
		}
		code := fstrans.Emit(ast)
		base := filepath.Base(src)
		name := base[:len(base)-len(filepath.Ext(base))]
		outDir := filepath.Join("tests", "transpiler", "x", "fs")
		if err := os.MkdirAll(outDir, 0755); err != nil {
			fmt.Fprintf(os.Stderr, "mkdir: %v\n", err)
			continue
		}
		fsFile := filepath.Join(outDir, name+".fs")
		if err := ioutil.WriteFile(fsFile, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "write fs: %v\n", err)
			continue
		}
		exe := filepath.Join(outDir, name+".exe")
		cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exe, fsFile)
		if out, err := cmd.CombinedOutput(); err != nil {
			ioutil.WriteFile(filepath.Join(outDir, name+".error"), out, 0644)
			fmt.Fprintf(os.Stderr, "compile %s: %v\n", src, err)
			continue
		}
		run := exec.Command("mono", exe)
		out, err := run.CombinedOutput()
		if err != nil {
			ioutil.WriteFile(filepath.Join(outDir, name+".error"), out, 0644)
			fmt.Fprintf(os.Stderr, "run %s: %v\n", src, err)
			continue
		}
		out = bytes.TrimSpace(out)
		ioutil.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
		os.Remove(filepath.Join(outDir, name+".error"))
	}
}
