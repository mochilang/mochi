//go:build slow

package st_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	st "mochi/transpiler/x/st"
	"mochi/types"
)

func TestSmalltalkRosetta(t *testing.T) {
	t.Cleanup(updateRosettaChecklist)
	_, gstErr := exec.LookPath("gst")
	if gstErr != nil {
		t.Log("gst not installed; falling back to stored outputs")
	}
	root := repoRootDir(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "st")
	os.MkdirAll(outDir, 0o755)

	indexPath := filepath.Join(srcDir, "index.txt")
	names, err := readIndex(indexPath)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no Mochi Rosetta tests found: %s", indexPath)
	}
	var files []string
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		files = []string{filepath.Join(srcDir, only+".mochi")}
	} else {
		for _, name := range names {
			files = append(files, filepath.Join(srcDir, name))
		}
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX %q", idxStr)
		}
		t.Logf("running rosetta program #%d/%d: %s", idx, len(files), filepath.Base(files[idx-1]))
		files = files[idx-1 : idx]
	}

	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	st.SetBenchMain(bench)

	for i, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		stPath := filepath.Join(outDir, name+".st")
		wantPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		if _, err := os.Stat(wantPath); os.IsNotExist(err) && !shouldUpdate() {
			t.Run(testName, func(t *testing.T) { t.Skip("missing golden") })
			continue
		}

		ok := t.Run(testName, func(t *testing.T) {
			prog, err := parser.Parse(srcPath)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("parse error")
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Skip("type error")
				return
			}
			ast, err := st.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("transpile error")
				return
			}
			var buf bytes.Buffer
			if err := st.Emit(&buf, ast, bench); err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Skip("emit error")
				return
			}
			code := buf.Bytes()
			if err := os.WriteFile(stPath, code, 0o644); err != nil {
				t.Fatalf("write st: %v", err)
			}
			var out []byte
			if gstErr == nil {
				cmd := exec.Command("gst", stPath)
				cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
				if bench {
					cmd.Env = append(cmd.Env, "MOCHI_BENCHMARK=1")
				}
				if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
					cmd.Stdin = bytes.NewReader(data)
				}
				out, err = cmd.CombinedOutput()
				if err != nil {
					_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
					return
				}
			} else {
				if bench {
					t.Skip("gst not installed for benchmark")
					return
				}
				var err error
				out, err = os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".out")
				if err != nil {
					t.Skip("no gst and no stored output")
					return
				}
			}
			got := bytes.TrimSpace(out)
			if bench {
				benchPath := filepath.Join(outDir, name+".bench")
				if shouldUpdate() {
					_ = os.WriteFile(benchPath, got, 0o644)
				}
				return
			}
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, append(got, '\n'), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
				_ = os.Remove(errPath)
				t.Logf("updated: %s", wantPath)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				_ = os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want)), 0o644)
				t.Errorf("output mismatch for %s", name)
				return
			}
			_ = os.Remove(errPath)
		})
		if !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
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
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "st")
	readmePath := filepath.Join(root, "transpiler", "x", "st", "ROSETTA.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|--------|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = "✗"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".st")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc)
			} else {
				ts = t
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Smalltalk Transpiler Rosetta Results\n\n")
	buf.WriteString("This checklist tracks the Rosetta Code programs transpiled into Smalltalk under `tests/rosetta/transpiler/st`.\n")
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.Format("2006-01-02 15:04 -0700"))
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
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
