//go:build archive && slow

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
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "dart")
	readmePath := filepath.Join(root, "transpiler", "x", "dart", "README.md")

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
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Dart Transpiler Output (%d/%d generated and run)\n\n", compiled, total)
	buf.WriteString("Generated Dart code for programs in `tests/vm/valid`. Each program has a `.dart` file and `.out` output. Compilation or runtime failures are captured in a `.error` file.\n\n")
	buf.WriteString("## Program checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	os.WriteFile(readmePath, buf.Bytes(), 0644)
}
