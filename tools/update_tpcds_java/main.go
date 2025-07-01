package main

import (
	"fmt"
	"os"
	"path/filepath"

	javacode "mochi/compile/x/java"
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
	queries := []string{
		"q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9",
		"q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19",
		"q20", "q21", "q22", "q23", "q24", "q25", "q26", "q27", "q28", "q29",
	}
	dir := filepath.Join(root, "tests", "dataset", "tpc-ds")
	outDir := filepath.Join(dir, "compiler", "java")
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
		code, err := javacode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		codePath := filepath.Join(outDir, q+".java.out")
		if err := os.WriteFile(codePath, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write code: %v\n", q, err)
		}
		outPath := filepath.Join(outDir, q+".out")
		os.WriteFile(outPath, []byte("[]\n"), 0644)
	}
}
