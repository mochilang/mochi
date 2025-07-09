//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/parser"
	mod "mochi/runtime/mod"
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
	if len(files) == 0 {
		fmt.Fprintln(os.Stderr, "no source files")
		os.Exit(1)
	}

	if len(os.Args) > 1 {
		var filtered []string
		for _, f := range files {
			base := strings.TrimSuffix(filepath.Base(f), ".mochi")
			for _, arg := range os.Args[1:] {
				if base == arg {
					filtered = append(filtered, f)
				}
			}
		}
		if len(filtered) == 0 {
			fmt.Fprintln(os.Stderr, "no matching queries")
			os.Exit(1)
		}
		files = filtered
	}

	outDir := filepath.Join(dir, "out")
	os.MkdirAll(outDir, 0755)

	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		outPath := filepath.Join(outDir, base+".out")
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
		modRoot, errRoot := mod.FindRoot(filepath.Dir(src))
		if errRoot != nil {
			modRoot = filepath.Dir(src)
		}
		os.Setenv("MOCHI_ROOT", modRoot)
		p, err := vm.Compile(prog, env)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}

		var buf bytes.Buffer
		m := vm.New(p, &buf)
		runErr := m.Run()
		outBytes := []byte(strings.TrimSpace(buf.String()) + "\n")
		if err := os.WriteFile(outPath, outBytes, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write out: %v\n", src, err)
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

		if runErr != nil {
			fmt.Fprintf(os.Stderr, "%s: run error: %v\n", src, runErr)
		}
	}
}
