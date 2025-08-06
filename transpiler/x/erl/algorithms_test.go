//go:build slow

package erl_test

import (
	"bufio"
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
	erl "mochi/transpiler/x/erl"
	"mochi/types"
)

func runAlgorithmCase(t *testing.T, nameFile string) {
	t.Helper()
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", nameFile)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Erlang")
	codePath := filepath.Join(outDir, strings.TrimSuffix(nameFile, ".mochi")+".erl")
	outPath := strings.TrimSuffix(codePath, ".erl") + ".out"
	benchPath := strings.TrimSuffix(codePath, ".erl") + ".bench"
	errPath := strings.TrimSuffix(codePath, ".erl") + ".error"

	if err := os.MkdirAll(filepath.Dir(codePath), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	want, err := os.ReadFile(outPath)
	if err == nil {
		want = bytes.TrimSpace(want)
	} else {
		want = nil
	}

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
	ast, err := erl.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("escript", codePath)
	envv := append(os.Environ(), "MOCHI_BENCHMARK=1")
	cmd.Env = envv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), got...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	benchBytes := []byte{}
	output := got
	if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
		output = bytes.TrimSpace(got[:idx])
		benchBytes = bytes.TrimSpace(got[idx:])
	}
	if len(benchBytes) > 0 {
		_ = os.WriteFile(benchPath, benchBytes, 0o644)
	}
	if err := os.WriteFile(outPath, output, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	if len(want) > 0 {
		canon := bytes.ReplaceAll(output, []byte{','}, []byte{' '})
		wantCanon := bytes.ReplaceAll(want, []byte{','}, []byte{' '})
		if !bytes.Equal(canon, wantCanon) {
			t.Fatalf("output mismatch\nGot: %s\nWant: %s", output, want)
		}
	}
}

func TestErlangTranspiler_Algorithms_Golden(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
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
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Erlang")
	mdPath := filepath.Join(root, "transpiler", "x", "erl", "ALGORITHMS.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		erlPath := filepath.Join(outDir, nameFile)
		erlPath = strings.TrimSuffix(erlPath, ".mochi") + ".erl"
		if _, err := os.Stat(strings.TrimSuffix(erlPath, ".erl") + ".error"); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(erlPath); err == nil {
			status = "✓"
			compiled++
		}
		dur := ""
		mem := ""
		benchFile := strings.TrimSuffix(erlPath, ".erl") + ".bench"
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
		} else if data, err := os.ReadFile(strings.TrimSuffix(erlPath, ".erl") + ".out"); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Erlang Algorithms Output (%d/%d compiled and run)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("This directory contains Erlang code generated by the Mochi transpiler for TheAlgorithms programs. Each program in `tests/github/TheAlgorithms/Mochi` is transpiled and executed with `escript`.\n\n")
	buf.WriteString("## Program checklist\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
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
