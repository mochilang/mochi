//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	root := "."
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "out", "Dart")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string

	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Rosetta Dart Output\n\n")
	buf.WriteString("This directory holds Dart code generated from the real Mochi programs in `tests/rosetta/x/Mochi`. Each file has the expected output in a matching `.out` file. Compilation or runtime failures are stored in a corresponding `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n\n", compiled, total)
	buf.WriteString("Checklist:\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")

	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
