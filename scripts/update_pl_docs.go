//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	readmePath := filepath.Join(root, "transpiler", "x", "pl", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		plPath := filepath.Join(outDir, name+".pl")
		outPath := filepath.Join(outDir, name+".out")
		if _, err := os.Stat(plPath); err == nil {
			if _, err := os.Stat(outPath); err == nil {
				if _, err := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err) {
					compiled++
					mark = "[x]"
				}
			}
		}
		lines = append(lines, fmt.Sprintf("- %s `%s`", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Prolog Transpiler\n\n")
	buf.WriteString("This directory contains a tiny transpiler that converts a restricted subset of Mochi programs to SWI-Prolog. It is mainly used for experimentation and golden tests.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n*Checklist generated automatically from tests/vm/valid*")
	return os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() error {
	root := repoRoot()
	taskPath := filepath.Join(root, "transpiler", "x", "pl", "TASKS.md")

	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	msgRaw, _ := exec.Command("git", "log", "-1", "--format=%s").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	msg := strings.TrimSpace(string(msgRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		tsStr = t.Format("2006-01-02 15:04 -0700")
	}

	files, _ := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		plPath := filepath.Join(root, "tests", "transpiler", "x", "pl", name+".pl")
		outPath := filepath.Join(root, "tests", "transpiler", "x", "pl", name+".out")
		if _, err := os.Stat(plPath); err == nil {
			if _, err := os.Stat(outPath); err == nil {
				if _, err := os.Stat(filepath.Join(root, "tests", "transpiler", "x", "pl", name+".error")); os.IsNotExist(err) {
					compiled++
				}
			}
		}
	}

	entry := fmt.Sprintf("## Progress (%s)\n- %s\n- %d/%d VM programs transpiled successfully\n\n", tsStr, msg, compiled, total)
	if prev, err := os.ReadFile(taskPath); err == nil {
		entry += string(prev)
	}
	return os.WriteFile(taskPath, []byte(entry), 0o644)
}

func main() {
	if err := updateReadme(); err != nil {
		panic(err)
	}
	if err := updateTasks(); err != nil {
		panic(err)
	}
}
