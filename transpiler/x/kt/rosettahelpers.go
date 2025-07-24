//go:build slow

package kt

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

// UpdateRosettaChecklist regenerates ROSETTA.md for Kotlin transpiler tests.
func UpdateRosettaChecklist() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Kotlin")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".kt")); err == nil {
			if _, errE := os.Stat(filepath.Join(outDir, name+".error")); errE != nil {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s `%s` (%d)", i+1, mark, name, i+1))
	}
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
	buf.WriteString("# Kotlin Rosetta Transpiler\n\n")
	buf.WriteString("Generated Kotlin sources for Rosetta Code tests are stored in `tests/rosetta/transpiler/Kotlin`.\n\n")
	if ts != "" {
		buf.WriteString("Last updated: " + ts + "\n\n")
	}
	fmt.Fprintf(&buf, "Completed tasks: **%d/%d**\n\n", compiled, total)
	buf.WriteString("### Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "transpiler", "x", "kt", "ROSETTA.md"), buf.Bytes(), 0o644)
}
