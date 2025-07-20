//go:build slow

package kt

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// UpdateReadmeForTests regenerates README checklist.
func UpdateReadmeForTests() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".kt")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Kotlin Transpiler\n\n")
	buf.WriteString("Generated Kotlin sources for golden tests are stored in `tests/transpiler/x/kt`.\n\n")
	buf.WriteString("The transpiler currently supports expression programs with `print`, integer and list literals, mutable variables and built-ins `count`, `sum`, `avg`, `len`, `str`, `append`, `min`, `max`, `substring` and `values`.\n\n")
	fmt.Fprintf(&buf, "Completed golden tests: **%d/%d** (auto-generated)\n\n", compiled, total)
	buf.WriteString("### Golden test checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "transpiler", "x", "kt", "README.md"), buf.Bytes(), 0o644)
}

// UpdateTasksForTests appends a progress entry using git timestamp.
func UpdateTasksForTests() {
	root := repoRootDir()
	taskFile := filepath.Join(root, "transpiler", "x", "kt", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## VM Golden Progress (%s)\n", ts))
	buf.WriteString("- Regenerated Kotlin golden files and README\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
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
