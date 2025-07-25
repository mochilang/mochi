//go:build archive && slow

package main

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	kt "mochi/transpiler/x/kt"
	"mochi/types"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func runCase(name string) error {
	root := repoRoot()
	src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return fmt.Errorf("type: %v", errs[0])
	}
	ast, err := kt.Transpile(env, prog, false)
	if err != nil {
		return fmt.Errorf("transpile: %v", err)
	}
	code := kt.Emit(ast)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	os.MkdirAll(outDir, 0o755)
	if err := os.WriteFile(filepath.Join(outDir, name+".kt"), code, 0o644); err != nil {
		return err
	}
	if data, err := os.ReadFile(filepath.Join(root, "tests", "vm", "valid", name+".out")); err == nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".out"), data, 0o644)
	}
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: gen_kt_golden <testname>...")
		os.Exit(1)
	}
	for _, name := range os.Args[1:] {
		if err := runCase(name); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
		}
	}
}
