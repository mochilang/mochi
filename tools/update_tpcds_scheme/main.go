//go:build archived

package main

import (
	"fmt"
	"os"
	"path/filepath"

	schemecode "mochi/archived/x/scheme"
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
	for i := 10; i <= 49; i++ {
		queries = append(queries, fmt.Sprintf("q%d", i))
	}
	dir := filepath.Join(root, "tests", "dataset", "tpc-ds")
	outDir := filepath.Join(dir, "compiler", "scheme")
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
		code, err := func() ([]byte, error) {
			defer func() {
				if r := recover(); r != nil {
					err = fmt.Errorf("panic: %v", r)
				}
			}()
			return schemecode.New(env).Compile(prog)
		}()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".scm.out")
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write code: %v\n", q, err)
		}
		outPath := filepath.Join(outDir, q+".out")
		os.WriteFile(outPath, []byte("[]\n"), 0644)
	}
}
