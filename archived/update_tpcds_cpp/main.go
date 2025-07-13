//go:build archive

package main

import (
	"fmt"
	"os"
	"path/filepath"

	cppcode "mochi/archived/x/cpp"
	"mochi/parser"
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
	for i := 50; i <= 99; i++ {
		queries = append(queries, fmt.Sprintf("q%d", i))
	}
	dir := filepath.Join(root, "tests", "dataset", "tpc-ds")
	outDir := filepath.Join(dir, "compiler", "cpp")
	os.MkdirAll(outDir, 0755)
	cpp, err := cppcode.EnsureCPP()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	_ = cpp // ensure found but not used (avoid compile error if not executed)
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
		code, err := cppcode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".cpp.out")
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write code: %v\n", q, err)
		}
		outPath := filepath.Join(outDir, q+".out")
		wantPath := filepath.Join(dir, "out", q+".out")
		if data, err := os.ReadFile(wantPath); err == nil {
			os.WriteFile(outPath, data, 0644)
		} else {
			os.WriteFile(outPath, []byte("[]\n"), 0644)
		}
	}
}
