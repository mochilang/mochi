//go:build slow

package rkt_test

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
	rkt "mochi/transpiler/x/rkt"
	"mochi/types"
)

func runSpojCase(t *testing.T, idx int) {
	t.Helper()
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.mochi", idx))
	outDir := filepath.Join(root, "tests", "spoj", "x", "racket")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	codePath := filepath.Join(outDir, fmt.Sprintf("%d.rkt", idx))
	outPath := filepath.Join(outDir, fmt.Sprintf("%d.out", idx))
	inPath := filepath.Join(outDir, fmt.Sprintf("%d.in", idx))
	benchPath := filepath.Join(outDir, fmt.Sprintf("%d.bench", idx))
	errPath := filepath.Join(outDir, fmt.Sprintf("%d.error", idx))

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
	if data, err := os.ReadFile(inPath); err == nil {
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

func TestRktTranspiler_SPOJ_Golden(t *testing.T) {
	t.Cleanup(updateSPOJChecklist)
	idx := 1
	if v := os.Getenv("MOCHI_SPOJ_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 {
			idx = n
		}
	}
	runSpojCase(t, idx)
}

func updateSPOJChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "racket")
	md := filepath.Join(root, "transpiler", "x", "rkt", "SPOJ.md")

	cases, err := readSpojCases(srcDir)
	if err != nil {
		return
	}
	total := len(cases)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for _, c := range cases {
		status := " "
		rktPath := filepath.Join(outDir, fmt.Sprintf("%d.rkt", c.idx))
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
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", c.idx, c.name, status, dur, mem))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Racket code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/racket`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

type spojCase struct {
	idx  int
	name string
}

func readSpojCases(dir string) ([]spojCase, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}
	var cases []spojCase
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if !strings.HasSuffix(name, ".mochi") {
			continue
		}
		base := strings.TrimSuffix(name, ".mochi")
		idx, err := strconv.Atoi(base)
		if err != nil {
			continue
		}
		mdPath := filepath.Join(dir, base+".md")
		probName := ""
		if data, err := os.ReadFile(mdPath); err == nil {
			lines := strings.Split(string(data), "\n")
			if len(lines) > 0 {
				first := strings.TrimSpace(lines[0])
				first = strings.TrimPrefix(first, "#")
				first = strings.TrimSpace(first)
				if strings.HasPrefix(first, "[") {
					if end := strings.Index(first, "]"); end > 1 {
						probName = first[1:end]
					} else {
						probName = first
					}
				} else {
					probName = first
				}
			}
		}
		cases = append(cases, spojCase{idx: idx, name: probName})
	}
	sort.Slice(cases, func(i, j int) bool { return cases[i].idx < cases[j].idx })
	return cases, nil
}
