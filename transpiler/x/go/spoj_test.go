//go:build slow

package gotranspiler_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	gotrans "mochi/transpiler/x/go"
	"mochi/types"
)

func TestGoTranspiler_Spoj_Golden(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go toolchain not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "go")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpojChecklist)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})
	if len(files) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}
	if idxStr := os.Getenv("MOCHI_SPOJ_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	} else {
		files = []string{filepath.Join(srcDir, "1.mochi")}
	}

	var passed, failed int
	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(fmt.Sprintf("%03s", base), func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".go")
			inPath := filepath.Join(outDir, base+".in")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")
			benchPath := filepath.Join(outDir, base+".bench")

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
			bench := true
			gprog, err := gotrans.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			gprog.BenchMain = bench
			code := gotrans.Emit(gprog, bench)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("go", "run", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
			if data, err := os.ReadFile(inPath); err == nil {
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
			benchData := got
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				_ = os.WriteFile(outPath, bytes.TrimSpace(got[:idx]), 0o644)
				benchData = got[idx:]
			} else {
				_ = os.WriteFile(outPath, got, 0o644)
				benchData = nil
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if updating() || len(want) == 0 {
				return
			}
			if gotOut, err := os.ReadFile(outPath); err == nil {
				gotOut = bytes.TrimSpace(gotOut)
				if !bytes.Equal(gotOut, want) {
					t.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, want)
				}
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = base
			}
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateSpojChecklist() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "go")
	readmePath := filepath.Join(root, "transpiler", "x", "go", "SPOJ.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Status | Duration | Memory |")
	rows = append(rows, "|------:|--------|---------:|-------:|")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = " "
		} else if _, err := os.Stat(filepath.Join(outDir, base+".go")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s |", base, status, dur, mem))
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
	buf.WriteString("# Go SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
