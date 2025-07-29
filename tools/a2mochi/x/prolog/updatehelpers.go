package prolog

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadmeForTests regenerates README checklist from golden files.
func UpdateReadmeForTests() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "prolog")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "prolog", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.pl"))
	sort.Strings(files)
	total := len(files)
	converted := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".pl")
		mark := "[ ]"
		outFile := filepath.Join(outDir, name+".out")
		if _, err := os.Stat(outFile); err == nil {
			if _, err := os.Stat(filepath.Join(outDir, name+".error")); err != nil {
				converted++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi Prolog Converter\n\n")
	buf.WriteString("This directory contains golden outputs for converting Prolog programs under `tests/transpiler/x/pl` into Mochi AST form.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", converted, total)
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n", ts)
	}
	buf.WriteString("\n## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
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
