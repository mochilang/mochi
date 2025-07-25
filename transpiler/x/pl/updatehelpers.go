//go:build slow

package pl

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

// UpdateReadmeForTests regenerates README checklist from the golden test outputs.
func UpdateReadmeForTests() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	readme := filepath.Join(root, "transpiler", "x", "pl", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".pl")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s `%s`", mark, name))
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
	buf.WriteString("# Prolog Transpiler\n\n")
	buf.WriteString("This directory contains a tiny transpiler that converts a restricted subset of Mochi programs to SWI-Prolog. It is mainly used for experimentation and golden tests.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n", ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n*Checklist generated automatically from tests/vm/valid*")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

// UpdateTasksForTests appends a progress entry using git timestamp.
func UpdateTasksForTests() {
	root := repoRootDir()
	taskFile := filepath.Join(root, "transpiler", "x", "pl", "TASKS.md")
	compiled, total := countCompiled(root)
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
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- VM valid golden test results updated to %d/%d\n", compiled, total)
	buf.WriteString("- Regenerated README checklist and outputs\n\n")
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

func countCompiled(root string) (int, int) {
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".pl")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

// UpdateRosettaReadme regenerates ROSETTA checklist from transpiler outputs.
func UpdateRosettaReadme() {
	root := repoRootDir()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Prolog")
	readme := filepath.Join(root, "transpiler", "x", "pl", "ROSETTA.md")
	idxPath := filepath.Join(srcDir, "index.txt")
	data, _ := os.ReadFile(idxPath)
	var files []string
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		fields := strings.Fields(line)
		if len(fields) >= 2 {
			files = append(files, strings.TrimSuffix(fields[1], ".mochi"))
		}
	}
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, name := range files {
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".pl")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
		if data, err := os.ReadFile(benchFile); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil {
				if js.Duration > 0 {
					dur = humanDuration(js.Duration)
				}
				if js.Memory > 0 {
					mem = humanSize(js.Memory)
				}
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &js) == nil && js.Duration > 0 {
					dur = humanDuration(js.Duration)
					mem = humanSize(js.Memory)
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
	buf.WriteString("# Rosetta Prolog Transpiler\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Prolog code from programs in `tests/rosetta/x/Mochi` lives in `tests/rosetta/transpiler/Prolog`.\n")
	if ts != "" {
		buf.WriteString("Last updated: " + ts + "\n\n")
	}
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func humanDuration(us int64) string {
	d := time.Duration(us) * time.Microsecond
	return d.String()
}

func humanSize(b int64) string {
	const unit = 1024
	if b < unit {
		return fmt.Sprintf("%d B", b)
	}
	div, exp := int64(unit), 0
	for n := b / unit; n >= unit; n /= unit {
		div *= unit
		exp++
	}
	return fmt.Sprintf("%.1f %cB", float64(b)/float64(div), "KMGTPE"[exp])
}
