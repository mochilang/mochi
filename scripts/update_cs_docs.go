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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	readmePath := filepath.Join(root, "transpiler", "x", "cs", "README.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}

	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		tsStr = t.Format("2006-01-02 15:04 -0700")
	}

	var buf bytes.Buffer
	buf.WriteString("# C# Transpiler Output\n\n")
	buf.WriteString("Generated C# code for programs in `tests/vm/valid`. Each program has a `.cs` file produced by the transpiler and a `.out` file containing its runtime output. Compilation or execution errors are captured in a `.error` file placed next to the source.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", tsStr)
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	return os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() error {
	root := repoRoot()
	taskPath := filepath.Join(root, "transpiler", "x", "cs", "TASKS.md")

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
		if _, err := os.Stat(filepath.Join(root, "tests", "transpiler", "x", "cs", name+".out")); err == nil {
			compiled++
		}
	}

	entry := fmt.Sprintf("## Progress (%s)\n- %s (progress %d/%d)\n\n", tsStr, msg, compiled, total)
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
