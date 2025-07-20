package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

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
	return ""
}

func updateReadme() error {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rkt")
	readmePath := filepath.Join(root, "transpiler", "x", "rkt", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".rkt")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi Racket Transpiler\n")
	buf.WriteString("This directory contains the experimental Racket transpiler. Golden tests under `tests/transpiler/x/rkt` check the generated code and its runtime output.\n")
	fmt.Fprintf(&buf, "\n## Golden Test Checklist (%d/%d)\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	return os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() error {
	root := repoRoot()
	taskPath := filepath.Join(root, "transpiler", "x", "rkt", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cd", "--date=format:%Y-%m-%d %H:%M:%S %Z").Output()
	ts := strings.TrimSpace(string(out))
	if err != nil {
		ts = ""
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	buf.WriteString("- Added cast expressions and function declarations\n")
	buf.WriteString("- Removed _div helper and improved type-based division\n")
	buf.WriteString("- Updated golden tests and README\n\n")
	if data, err := os.ReadFile(taskPath); err == nil {
		buf.Write(data)
	}
	return os.WriteFile(taskPath, buf.Bytes(), 0o644)
}

func main() {
	if err := updateReadme(); err != nil {
		panic(err)
	}
	if err := updateTasks(); err != nil {
		panic(err)
	}
}
