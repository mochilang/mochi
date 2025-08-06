//go:build slow

package rs_test

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
	"unicode"

	"mochi/parser"
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

func TestRSTranspiler_Algorithms_Golden(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Rust")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateAlgorithmsChecklist)

	names, err := readIndex(srcDir)
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
	var passed, failed int
	var firstFail string
	for i, src := range files {
		rel := strings.TrimSuffix(names[i], ".mochi")
		name := strings.ReplaceAll(rel, string(os.PathSeparator), "_")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, rel+".rs")
			outPath := filepath.Join(outDir, rel+".out")
			errPath := filepath.Join(outDir, rel+".error")
			benchPath := filepath.Join(outDir, rel+".bench")
			if err := os.MkdirAll(filepath.Dir(codePath), 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
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
			gprog, err := rs.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := rs.Emit(gprog)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			tmpDir := t.TempDir()
			bin := filepath.Join(tmpDir, name)
			crate := strings.Map(func(r rune) rune {
				if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
					return r
				}
				return '_'
			}, name)
			if out, err := exec.Command("rustc", codePath, "-O", "--crate-name", crate, "-o", bin).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, out, 0o644)
				t.Fatalf("rustc: %v", err)
			}
			cmd := exec.Command(bin)
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
			want = bytes.TrimSpace(want)
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			benchData := got
			outBytes := got
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				outBytes = bytes.TrimSpace(got[:idx])
				benchData = bytes.TrimSpace(got[idx:])
			} else {
				benchData = nil
			}
			outBytes = bytes.ReplaceAll(outBytes, []byte(", "), []byte(" "))
			_ = os.WriteFile(outPath, outBytes, 0o644)
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if want != nil && len(want) > 0 {
				if !bytes.Equal(outBytes, want) {
					t.Errorf("output mismatch\nGot: %s\nWant: %s", outBytes, want)
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
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Rust")
	md := filepath.Join(root, "transpiler", "x", "rs", "ALGORITHMS.md")
	names, _ := readIndex(srcDir)
	total := len(names)
	compiled := 0
	lines := []string{"| Index | Name | Status | Duration | Memory |", "|------:|------|:-----:|---------:|-------:|"}
	for i, n := range names {
		name := strings.TrimSuffix(n, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = " "
		} else if _, err := os.Stat(filepath.Join(outDir, name+".rs")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &r) == nil && r.DurationUS > 0 {
				dur = humanDuration(r.DurationUS)
				mem = humanSize(r.MemoryBytes)
			}
		} else if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &r) == nil && r.DurationUS > 0 {
					dur = humanDuration(r.DurationUS)
					mem = humanSize(r.MemoryBytes)
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Algorithms Rust Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Rust code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/Rust`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
