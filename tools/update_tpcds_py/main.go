package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	pycode "mochi/compile/py"
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
	dir := filepath.Join(root, "tests/dataset/tpc-ds")
	pattern := filepath.Join(dir, "q[1-9].mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	sort.Strings(files)
	if len(files) == 0 {
		fmt.Fprintln(os.Stderr, "no files")
		os.Exit(1)
	}
	outDir := filepath.Join(dir, "compiler", "py")
	os.MkdirAll(outDir, 0755)
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".py.out")
		outPath := filepath.Join(outDir, base+".out")
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
		code, err := pycode.New(env).Compile(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: compile error: %v\n", src, err)
			continue
		}
		if err := os.WriteFile(codePath, bytes.TrimSpace(code), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write code: %v\n", src, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), base+".py")
		if err := os.WriteFile(tmp, code, 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write tmp: %v\n", src, err)
			continue
		}
		cmd := exec.Command("python3", tmp)
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: run error: %v\n%s\n", src, err, out)
			continue
		}
		if err := os.WriteFile(outPath, bytes.TrimSpace(out), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "%s: write out: %v\n", src, err)
		}
	}
}
