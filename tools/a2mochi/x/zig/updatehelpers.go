package zig

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadme regenerates README checklist from golden tests.
func UpdateReadme() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "zig")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "zig")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.zig"))
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".zig")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			mark = "[x]"
			done++
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Zig Converter\n\n")
	buf.WriteString("This directory stores helpers and golden files for converting Zig programs\n")
	buf.WriteString("in `tests/transpiler/x/zig` into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n\n", done, total)
	buf.WriteString("Last updated: " + ts + "\n\n")
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "tools", "a2mochi", "x", "zig", "README.md"), buf.Bytes(), 0o644)
}

func repoRootDir() string {
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
