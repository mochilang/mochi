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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "st")

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
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Smalltalk Transpiler Output (%d/%d programs)\n\n", compiled, total)
	buf.WriteString("Generated Smalltalk code for programs in `tests/vm/valid`. Each program has a `.st` file and matching `.out` with the runtime output. Failures are stored as `.error`.\n\n")
	buf.WriteString("## Program checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')

	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
