package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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

func readIndex(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var names []string
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		f := strings.Fields(line)
		if len(f) == 2 {
			names = append(names, f[1])
		}
	}
	return names, nil
}

func fmtDuration(us int64) string {
	d := time.Duration(us) * time.Microsecond
	return d.String()
}

func fmtSize(b int64) string {
	const KB = 1024
	const MB = 1024 * KB
	if b >= MB {
		return fmt.Sprintf("%.1f MB", float64(b)/float64(MB))
	}
	if b >= KB {
		return fmt.Sprintf("%.1f KB", float64(b)/float64(KB))
	}
	return fmt.Sprintf("%d B", b)
}

func main() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Dart")
	readmePath := filepath.Join(root, "transpiler", "x", "dart", "ROSETTA.md")

	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
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

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		panic(err)
	}
	total := len(names)
	completed := 0
	var lines []string
	for i, fname := range names {
		name := strings.TrimSuffix(fname, ".mochi")
		mark := " "
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
		if _, err := os.Stat(benchFile); err == nil {
			mark = "✓"
			completed++
			data, _ := os.ReadFile(benchFile)
			var bench struct {
				DurationUs  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &bench) == nil {
				if bench.DurationUs > 0 {
					dur = fmtDuration(bench.DurationUs)
				}
				if bench.MemoryBytes > 0 {
					mem = fmtSize(bench.MemoryBytes)
				}
			}
		} else if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				mark = "✓"
				completed++
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, mark, dur, mem))
	}

	var buf bytes.Buffer
	buf.WriteString("# Dart Rosetta Transpiler Output\n\n")
	buf.WriteString("This directory contains Dart code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each program has a `.dart` file and `.out` output. Compilation or runtime failures are captured in a `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled and ran: %d/%d\n", completed, total)
	buf.WriteString("\n## Checklist\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	if ts != "" {
		buf.WriteString(fmt.Sprintf("\n_Last updated: %s_\n", ts))
	}
	if err := os.WriteFile(readmePath, buf.Bytes(), 0o644); err != nil {
		panic(err)
	}
}
