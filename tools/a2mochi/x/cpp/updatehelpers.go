package cpp

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
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "cpp")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "cpp")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "cpp", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.cpp"))
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".cpp")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".mochi")); err == nil {
			mark = "[x]"
			done++
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# a2mochi C++ Converter\n\n")
	buf.WriteString("This directory contains helpers and golden files for converting C++ programs under `tests/transpiler/x/cpp` back into Mochi AST form using `clang++`.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", done, total)
	fmt.Fprintf(&buf, "Date: %s\n\n", ts)
	buf.WriteString("## Checklist\n")
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
