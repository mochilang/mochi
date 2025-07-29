package fs

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// UpdateReadme regenerates README checklist from golden outputs.
func UpdateReadme() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "fs")
	outDir := filepath.Join(root, "tests", "a2mochi", "x", "fs")
	vmDir := filepath.Join(root, "tests", "vm", "valid")
	readme := filepath.Join(root, "tools", "a2mochi", "x", "fs", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.fs"))
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".fs")
		mark := "[ ]"
		outPath := filepath.Join(outDir, name+".out")
		vmOut := filepath.Join(vmDir, name+".out")
		if got, err := os.ReadFile(outPath); err == nil {
			if want, werr := os.ReadFile(vmOut); werr == nil {
				if bytes.Equal(bytes.TrimSpace(got), bytes.TrimSpace(want)) {
					mark = "[x]"
					done++
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*3600)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# F# AST Conversion\n\n")
	buf.WriteString("This directory contains test helpers and golden files for converting F# programs under `tests/transpiler/x/fs` into Mochi AST form.\n\n")
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
