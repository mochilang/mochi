package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	tscode "mochi/archived/ts"
	"mochi/parser"
	mod "mochi/runtime/mod"
	"mochi/types"
)

func findRepoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

func main() {
	root, err := findRepoRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	queries := []string{}
	for i := 1; i <= 99; i++ {
		queries = append(queries, fmt.Sprintf("q%d", i))
	}
	dir := filepath.Join(root, "tests", "dataset", "tpc-ds")
	outDir := filepath.Join(dir, "compiler", "ts")
	os.MkdirAll(outDir, 0755)
	for _, q := range queries {
		src := filepath.Join(dir, q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: parse error: %v\n", src, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintf(os.Stderr, "%s: type error: %v\n", src, errs[0])
			continue
		}
		modRoot, errRoot := mod.FindRoot(filepath.Dir(src))
		if errRoot != nil {
			modRoot = filepath.Dir(src)
		}
		code, err := tscode.New(env, modRoot).Compile(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".ts.out")
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write code: %v\n", q, err)
		}
		tmpDir, _ := os.MkdirTemp("", "mochts")
		file := filepath.Join(tmpDir, "main.ts")
		os.WriteFile(file, code, 0644)
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		out, err := cmd.CombinedOutput()
		lines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
		var first []byte
		if len(lines) > 0 {
			first = lines[0]
		}
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: run error: %v\n", q, err)
		}
		outPath := filepath.Join(outDir, q+".out")
		os.WriteFile(outPath, first, 0644)
	}
}
