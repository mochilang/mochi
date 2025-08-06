//go:build slow

package rkt_test

import (
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
	rkt "mochi/transpiler/x/rkt"
	"mochi/types"
)

func runAlgorithmCase(t *testing.T, nameFile string) {
	t.Helper()
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", nameFile)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Racket", filepath.Dir(nameFile))
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	base := strings.TrimSuffix(filepath.Base(nameFile), ".mochi")
	codePath := filepath.Join(outDir, base+".rkt")
	outPath := filepath.Join(outDir, base+".out")
	benchPath := filepath.Join(outDir, base+".bench")
	errPath := filepath.Join(outDir, base+".error")

	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	bench := true
	ast, err := rkt.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	var buf bytes.Buffer
	if err := rkt.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		t.Fatalf("emit: %v", err)
	}
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("racket", codePath)
	envs := append(os.Environ(), "MOCHI_ROOT="+root, "MOCHI_NOW_SEED=1", "MOCHI_BENCHMARK=1")
	cmd.Env = envs
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)
	benchBytes := []byte{}
	if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
		benchBytes = bytes.TrimSpace(got[idx:])
		got = bytes.TrimSpace(got[:idx])
	}
	if len(benchBytes) > 0 {
		_ = os.WriteFile(benchPath, benchBytes, 0o644)
	}
	_ = os.WriteFile(outPath, got, 0o644)
	if updating() || len(want) == 0 {
		return
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestRktTranspiler_Algorithms_Golden(t *testing.T) {
	t.Cleanup(updateAlgorithmsChecklist)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatal("no algorithm programs found")
	}
	start := 0
	if idxStr := os.Getenv("MOCHI_ALGORITHMS_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ALGORITHMS_INDEX: %s", idxStr)
		}
		start = idx - 1
		names = names[start : start+1]
	}
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		idx := start + i + 1
		if ok := t.Run(fmt.Sprintf("%04d_%s", idx, name), func(t *testing.T) { runAlgorithmCase(t, nameFile) }); !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func updateAlgorithmsChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Racket")
	md := filepath.Join(root, "transpiler", "x", "rkt", "ALGORITHMS.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		rktPath := filepath.Join(outDir, nameFile)
		rktPath = strings.TrimSuffix(rktPath, ".mochi") + ".rkt"
		if _, err := os.Stat(strings.TrimSuffix(rktPath, ".rkt") + ".error"); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(rktPath); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := strings.TrimSuffix(rktPath, ".rkt") + ".bench"
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
		} else if data, err := os.ReadFile(strings.TrimSuffix(rktPath, ".rkt") + ".out"); err == nil {
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
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Algorithms Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Racket code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/Racket`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func readIndex(path string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(string(data), "\n")
	var names []string
	for _, ln := range lines {
		ln = strings.TrimSpace(ln)
		if ln == "" {
			continue
		}
		parts := strings.Fields(ln)
		if len(parts) != 2 {
			continue
		}
		names = append(names, parts[1])
	}
	return names, nil
}

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
