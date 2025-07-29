//go:build slow

package ruby

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadmeForTests regenerates README checklist from golden outputs.
func UpdateReadmeForTests() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "rb")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "rb")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "ruby", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.rb"))
	sort.Strings(files)
	total := len(files)
	converted := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".rb")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			converted++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Ruby Converter\n\n")
	buf.WriteString("This directory contains a minimal converter that translates simple Ruby programs back into Mochi form. It uses Ruby's built in `ripper` library to obtain an s-expression AST which is then converted to Mochi code. The implementation mirrors the Python and TypeScript converters and is only powerful enough for the examples under `tests/transpiler/x/rb`.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", converted, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", ts)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return dir
}
