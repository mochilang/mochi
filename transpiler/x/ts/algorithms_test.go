//go:build slow

package tstranspiler_test

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
	meta "mochi/transpiler/meta"
	tstranspiler "mochi/transpiler/x/ts"
	"mochi/types"
)

func runAlgorithmCase(t *testing.T, nameFile string) {
	t.Helper()
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", nameFile)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "TypeScript")
	codePath := filepath.Join(outDir, strings.TrimSuffix(nameFile, ".mochi")+".ts")
	outPath := strings.TrimSuffix(codePath, ".ts") + ".out"
	benchPath := strings.TrimSuffix(codePath, ".ts") + ".bench"
	errPath := strings.TrimSuffix(codePath, ".ts") + ".error"

	if err := os.MkdirAll(filepath.Dir(codePath), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

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
	tsprog, err := tstranspiler.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := tstranspiler.Emit(tsprog)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", codePath)
	envv := append(os.Environ(), "DENO_TLS_CA_STORE=system", "DENO_INSTALLER=skip", "DENO_NO_UPDATE_CHECK=1")
	cmd.Env = envv
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

func TestTSTranspiler_Algorithms_Golden(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
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
	if only := os.Getenv("MOCHI_ALGORITHMS_ONLY"); only != "" {
		names = []string{only}
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
	outDir := filepath.Join(root, "tests", "algorithms", "x", "TypeScript")
	md := filepath.Join(root, "transpiler", "x", "ts", "ALGORITHMS.md")

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
		tsPath := filepath.Join(outDir, nameFile)
		tsPath = strings.TrimSuffix(tsPath, ".mochi") + ".ts"
		if _, err := os.Stat(strings.TrimSuffix(tsPath, ".ts") + ".error"); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(tsPath); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := strings.TrimSuffix(tsPath, ".ts") + ".bench"
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
		} else if data, err := os.ReadFile(strings.TrimSuffix(tsPath, ".ts") + ".out"); err == nil {
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
	buf.WriteString("Generated TypeScript code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/TypeScript`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
