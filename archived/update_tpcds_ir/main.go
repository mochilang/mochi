package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/runtime/vm"
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
	dir := filepath.Join(root, "tests/dataset/tpc-ds")
	pattern := filepath.Join(dir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	sort.Strings(files)
	if len(os.Args) > 1 {
		var filtered []string
		for _, f := range files {
			base := strings.TrimSuffix(filepath.Base(f), ".mochi")
			for _, a := range os.Args[1:] {
				if base == a {
					filtered = append(filtered, f)
				}
			}
		}
		files = filtered
	}
	if len(files) == 0 {
		fmt.Fprintln(os.Stderr, "no files")
		os.Exit(1)
	}
	outDir := filepath.Join(dir, "out")
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		irPath := filepath.Join(outDir, base+".ir.out")
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
		p, err := vm.Compile(prog, env)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		srcData, err := os.ReadFile(src)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: read src: %v\n", src, err)
			continue
		}
		ir := []byte(strings.TrimSpace(p.Disassemble(string(srcData))))
		if err := os.WriteFile(irPath, ir, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write ir: %v\n", src, err)
		}
	}
}
