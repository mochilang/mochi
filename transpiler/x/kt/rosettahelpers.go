//go:build slow

package kt

import (
	"bytes"
	"encoding/json"
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
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "| ---: | --- | :---: | ---: | ---: |")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := ""
		dur := ""
		mem := ""
		outPath := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(outPath); err == nil {
			status = "✓"
			compiled++
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				var res struct {
					Dur int64 `json:"duration_us"`
					Mem int64 `json:"memory_bytes"`
				}
				if json.Unmarshal(trimmed[idx:], &res) == nil {
					if res.Dur > 0 {
						dur = humanDur(time.Duration(res.Dur) * time.Microsecond)
					}
					if res.Mem > 0 {
						mem = humanSize(res.Mem)
					}
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
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

func humanDur(d time.Duration) string {
	if d < time.Millisecond {
		return fmt.Sprintf("%dus", d.Microseconds())
	}
	if d < time.Second {
		return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000)
	}
	if d < time.Minute {
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
	return d.String()
}

func humanSize(n int64) string {
	const unit = 1024
	units := []string{"B", "KB", "MB", "GB", "TB"}
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	val := float64(n)
	exp := 0
	for val >= unit && exp < len(units)-1 {
		val /= unit
		exp++
	}
	return fmt.Sprintf("%.1f %s", val, units[exp])
}
