//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"

	"mochi/parser"
	rkt "mochi/transpiler/x/rkt"
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
	ast, err := rkt.Transpile(prog, env)
	if err != nil {
		return fmt.Errorf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := rkt.Emit(&buf, ast); err != nil {
		return fmt.Errorf("emit: %v", err)
	}
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rkt")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	codePath := filepath.Join(outDir, name+".rkt")
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		return err
	}
	srcOut := filepath.Join(root, "tests", "vm", "valid", name+".out")
	if data, err := os.ReadFile(srcOut); err == nil {
		trimmed := bytes.TrimSpace(data)
		trimmed = append(trimmed, '\n')
		os.WriteFile(filepath.Join(outDir, name+".out"), trimmed, 0o644)
	}
	os.Remove(filepath.Join(outDir, name+".error"))
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: gen_rkt_golden <testname>...")
		os.Exit(1)
	}
	for _, name := range os.Args[1:] {
		if err := runCase(name); err != nil {
			errPath := filepath.Join(repoRoot(), "tests", "transpiler", "x", "rkt", name+".error")
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
		}
	}
}
