package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"mochi/parser"
	pl "mochi/transpiler/x/pl"
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
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		return fmt.Errorf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		return fmt.Errorf("emit: %v", err)
	}
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	os.MkdirAll(outDir, 0o755)
	plPath := filepath.Join(outDir, name+".pl")
	if err := os.WriteFile(plPath, buf.Bytes(), 0o644); err != nil {
		return err
	}
	cmd := exec.Command("swipl", "-q", "-f", plPath)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		return fmt.Errorf("run: %v", err)
	}
	os.Remove(filepath.Join(outDir, name+".error"))
	trimmed := bytes.TrimSpace(out)
	if !bytes.HasSuffix(trimmed, []byte("\n")) {
		trimmed = append(trimmed, '\n')
	}
	outPath := filepath.Join(outDir, name+".out")
	return os.WriteFile(outPath, trimmed, 0o644)
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: gen_pl_golden <testname>...")
		os.Exit(1)
	}
	for _, name := range os.Args[1:] {
		if err := runCase(name); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", name, err)
		}
	}
}
