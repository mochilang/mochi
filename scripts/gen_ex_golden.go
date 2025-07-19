//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
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
	ast, err := ex.Transpile(prog, env)
	if err != nil {
		return fmt.Errorf("transpile: %v", err)
	}
	code := ex.Emit(ast)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	codePath := filepath.Join(outDir, name+".exs")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return err
	}
	cmd := exec.Command("elixir", codePath)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.Output()
	if err != nil {
		errPath := filepath.Join(outDir, name+".error")
		_ = os.WriteFile(errPath, out, 0o644)
		return fmt.Errorf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	trimmed := bytes.TrimSpace(out)
	if !bytes.HasSuffix(trimmed, []byte("\n")) {
		trimmed = append(trimmed, '\n')
	}
	outPath := filepath.Join(outDir, name+".out")
	if err := os.WriteFile(outPath, trimmed, 0o644); err != nil {
		return err
	}
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: gen_ex_golden <testname>...")
		os.Exit(1)
	}
	for _, name := range os.Args[1:] {
		if err := runCase(name); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
		}
	}
}
