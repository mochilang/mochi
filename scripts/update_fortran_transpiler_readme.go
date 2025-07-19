//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return ""
}

func main() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fortran")
	readme := filepath.Join(root, "transpiler", "x", "fortran", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)

	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				mark = "[x]"
				compiled++
			}
		}
		lines = append(lines, fmt.Sprintf("* %s %s.mochi", mark, name))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Transpiler Progress\n\n")
	buf.WriteString("This checklist tracks Mochi programs from `tests/vm/valid` that successfully transpile using the experimental Fortran backend.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d):\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')

	os.WriteFile(readme, buf.Bytes(), 0644)
}
