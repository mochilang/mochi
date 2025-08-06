//go:build slow

package fstrans_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	fstrans "mochi/transpiler/x/fs"
	"mochi/types"
)

func TestFSTranspiler_Algorithms_Golden(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "FS")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateAlgorithmsChecklist)

	names, err := readAlgIndex(filepath.Join(srcDir, "index.txt"))
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
	if idxStr := os.Getenv("MOCHI_ALGORITHMS_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ALGORITHMS_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
		names = []string{names[idx-1]}
	}

	fstrans.SetBenchMain(true)

	var passed, failed int
	var firstFail string
	for i, src := range files {
		rel := strings.TrimSuffix(names[i], ".mochi")
		name := strings.ReplaceAll(rel, string(os.PathSeparator), "_")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, rel+".fs")
			outPath := filepath.Join(outDir, rel+".out")
			errPath := filepath.Join(outDir, rel+".error")
			benchPath := filepath.Join(outDir, rel+".bench")
			exePath := filepath.Join(outDir, rel+".exe")
			os.MkdirAll(filepath.Dir(codePath), 0o755)

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
			ast, err := fstrans.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := fstrans.Emit(ast)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exePath, codePath)
			if out, err := cmd.CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			defer os.Remove(exePath)
			run := exec.Command("mono", exePath)
			run.Env = append(os.Environ(), "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				run.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
			want = bytes.TrimSpace(want)
			out, err := run.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			benchData := got
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				_ = os.WriteFile(outPath, bytes.TrimSpace(got[:idx]), 0o644)
				benchData = bytes.TrimSpace(got[idx:])
			} else {
				_ = os.WriteFile(outPath, got, 0o644)
				benchData = nil
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if want != nil && len(want) > 0 {
				if gotOut, err := os.ReadFile(outPath); err == nil {
					gotOut = bytes.TrimSpace(gotOut)
					if !bytes.Equal(gotOut, want) {
						t.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, want)
					}
				}
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = rel
			}
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateAlgorithmsChecklist() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "FS")
	readmePath := filepath.Join(root, "transpiler", "x", "fs", "ALGORITHMS.md")
	names, _ := readAlgIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".fs")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &r) == nil {
				if r.DurationUS > 0 {
					dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				}
				if r.MemoryBytes > 0 {
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
	buf.WriteString("# F# Algorithms Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func findRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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
	t.Fatal("go.mod not found")
	return ""
}

func readAlgIndex(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var names []string
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		fields := strings.Fields(line)
		if len(fields) == 2 {
			names = append(names, fields[1])
		}
	}
	return names, nil
}

func formatDuration(d time.Duration) string { return d.String() }

func formatBytes(b int64) string {
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
