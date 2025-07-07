package main

import (
	"fmt"
	"os"
	"path/filepath"

	javacode "mochi/archived/x/java"
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
		"q30", "q31", "q32", "q33", "q34", "q35", "q36", "q37", "q38", "q39",
		"q40", "q41", "q42", "q43", "q44", "q45", "q46", "q47", "q48", "q49",
		"q50", "q51", "q52", "q53", "q54", "q55", "q56", "q57", "q58", "q59",
		"q60", "q61", "q62", "q63", "q64", "q65", "q66", "q67", "q68", "q69",
		"q70", "q71", "q72", "q73", "q74", "q75", "q76", "q77", "q78", "q79",
		"q80", "q81", "q82", "q83", "q84", "q85", "q86", "q87", "q88", "q89",
		"q90", "q91", "q92", "q93", "q94", "q95", "q96", "q97", "q98", "q99",
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
