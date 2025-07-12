//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	root := "." // assume run from repo root
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "scheme")
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
	fmt.Fprintf(&buf, "# Scheme Machine Output (%d/%d compiled and run)\n\n", compiled, total)
	buf.WriteString("This directory contains Scheme code generated from the Mochi programs in `tests/vm/valid`. Each program was executed with chibi-scheme. Successful runs have a `.out` file and failures provide a `.error`.\n\n")
	buf.WriteString("## Program checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n## Remaining tasks\n")
	buf.WriteString("- [ ] Better handling of date comparisons and sorting when running JOB benchmarks\n")
	buf.WriteString("- [ ] More efficient dataset grouping and aggregation\n")
	buf.WriteString("- [ ] Support for concurrent agents and streaming primitives\n")
	if compiled == total {
		buf.WriteString("\nAll programs executed successfully.\n")
	}
	os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0o644)
}
