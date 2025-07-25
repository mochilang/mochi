//go:build slow

package vm_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	_ "mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestVM_RosettaTasks(t *testing.T) {
	root := findRepoRoot(t)
	dir := filepath.Join(root, "tests/rosetta/x/Mochi")
	t.Cleanup(updateRosettaChecklist)

	names, err := readIndex(filepath.Join(dir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no Mochi files in %s", dir)
	}

	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(dir, n)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	benchEnv := os.Getenv("MOCHI_BENCHMARK")
	bench := benchEnv == "true" || benchEnv == "1"

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		out := strings.TrimSuffix(src, ".mochi") + ".out"

		t.Run(name, func(t *testing.T) {
			got, err := runMochi(src)
			if err != nil {
				t.Fatalf("run error: %v", err)
			}
			want, _ := os.ReadFile(out)
			if !bench {
				want = bytes.TrimSpace(want)
			}

			got = bytes.TrimSpace(got)
			if bench {
				benchData := got
				if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
					_ = os.WriteFile(out, bytes.TrimSpace(got[:idx]), 0644)
					benchData = got[idx:]
				}
				_ = os.WriteFile(strings.TrimSuffix(src, ".mochi")+".bench", benchData, 0644)
				return
			}

			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if shouldUpdate() || len(want) == 0 {
				if err2 := os.WriteFile(out, append(got, '\n'), 0644); err2 == nil {
					t.Logf("updated: %s", out)
					return
				} else {
					t.Fatalf("write golden: %v", err2)
				}
			}

			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
	}
}

func runMochi(src string) ([]byte, error) {
	benchEnv := os.Getenv("MOCHI_BENCHMARK")
	bench := benchEnv == "true" || benchEnv == "1"
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(src, errs[0])
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	var start time.Time
	var startMem uint64
	if bench {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem = ms.Alloc
		start = time.Now()
	}
	if err := m.Run(); err != nil {
		writeErr(src, err)
		return nil, fmt.Errorf("run error: %w", err)
	}
	if bench {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		dur := time.Since(start)
		mem := int64(ms.Alloc - startMem)
		data := map[string]any{"name": "main", "duration_us": dur.Microseconds(), "memory_bytes": mem}
		js, _ := json.MarshalIndent(data, "", "  ")
		fmt.Fprintln(&out, string(js))
	}
	removeErr(src)
	b := bytes.TrimSpace(out.Bytes())
	if b == nil {
		b = []byte{}
	}
	return b, nil
}

func writeErr(src string, err error) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	_ = os.WriteFile(base+".error", []byte(err.Error()), 0644)
}

func removeErr(src string) {
	base := strings.TrimSuffix(src, filepath.Ext(src))
	os.Remove(base + ".error")
	os.Remove(base + ".mochi.error")
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func updateRosettaChecklist() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	readmePath := filepath.Join(root, "runtime", "vm", "ROSETTA.md")
	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|--------|---------:|-------:|")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
			status = " "
		} else if _, err := os.Stat(filepath.Join(srcDir, name+".out")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(srcDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &r) == nil && r.DurationUS > 0 {
					dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
					mem = formatBytes(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# VM Rosetta Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
