//go:build slow

package pl

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
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	outDir := filepath.Join(root, "tests", "aster", "x", "pl")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.pl"))
	sort.Strings(files)
	total := len(files)
	done := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".pl")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			mark = "[x]"
			done++
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", len(lines)+1, mark, name+".pl"))
	}
	tz := time.FixedZone("GMT+7", 7*3600)
	var b bytes.Buffer
	b.WriteString("# Prolog AST Printer\n\n")
	b.WriteString("This directory contains utilities for inspecting Prolog programs using SWI-Prolog and printing them back from the AST.\n\n")
	b.WriteString("## Test Files\n\n")
	b.WriteString(strings.Join(lines, "\n"))
	fmt.Fprintf(&b, "\n\nCompleted **%d/%d** examples.\n\n", done, total)
	fmt.Fprintf(&b, "_Last updated: %s_\n", time.Now().In(tz).Format("2006-01-02 15:04 GMT-07"))
	_ = os.WriteFile(filepath.Join(root, "aster", "x", "pl", "README.md"), b.Bytes(), 0o644)
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
