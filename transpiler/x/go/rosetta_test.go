//go:build slow

package gotranspiler_test

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
	gotrans "mochi/transpiler/x/go"
	"mochi/types"
)

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

func TestGoTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Go")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaChecklist)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}

	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(srcDir, n)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	var passed, failed int
	var firstFail string
	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, name+".go")
			outPath := filepath.Join(outDir, name+".out")
			errPath := filepath.Join(outDir, name+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
			gprog, err := gotrans.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := gotrans.Emit(gprog)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("go", "run", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)

			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			_ = os.WriteFile(outPath, got, 0o644)

			if bench {
				return
			}

			if updating() || len(want) == 0 {
				return
			}
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = name
			}
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

// TestMain is defined in vm_valid_golden_test.go. That TestMain updates
// README and TASKS files after the test run. We hook the rosetta checklist
// update there as well, so we don't need a separate TestMain in this file.

func updateRosettaChecklist() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Go")
	readmePath := filepath.Join(root, "transpiler", "x", "go", "ROSETTA.md")
	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave blank for failure
		} else if _, err := os.Stat(filepath.Join(outDir, name+".go")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
			var r struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(trimmed, '{'); idx >= 0 {
				if json.Unmarshal(trimmed[idx:], &r) == nil && r.Dur > 0 {
					dur = humanDur(time.Duration(r.Dur) * time.Microsecond)
					mem = humanSize(r.Mem)
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
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
	buf.WriteString("# Go Rosetta Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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

// findRepoRoot is declared in vm_valid_golden_test.go and shared across tests.

func updating() bool {
	f := flag.Lookup("update")
	if f == nil {
		return false
	}
	if getter, ok := f.Value.(interface{ Get() any }); ok {
		if v, ok2 := getter.Get().(bool); ok2 {
			return v
		}
	}
	return false
}
