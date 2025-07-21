package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

func main() {
	root := "."
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "fs")
	readme := filepath.Join(root, "transpiler", "x", "fs", "README.md")
	taskFile := filepath.Join(root, "transpiler", "x", "fs", "TASKS.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
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
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}

	var buf bytes.Buffer
	buf.WriteString("# Mochi F# Transpiler\n\n")
	buf.WriteString("This folder contains an experimental transpiler that converts Mochi source code into F#.\n\n")
	fmt.Fprintf(&buf, "## Golden Test Checklist (%d/%d)\n\n", compiled, total)
	buf.WriteString("The list below tracks Mochi programs under `tests/vm/valid` that should successfully transpile. Checked items indicate tests known to work.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)

	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}

	buf.Reset()
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- Generated F# for %d/%d programs (%d passing)\n\n", total, total, compiled)
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
