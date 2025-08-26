//go:build slow

package erl_test

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
	erl "mochi/transpiler/x/erl"
	"mochi/types"
)

func runSpojCase(t *testing.T, idx int) {
	t.Helper()
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.mochi", idx))
	outDir := filepath.Join(root, "tests", "spoj", "x", "erlang")
	codePath := filepath.Join(outDir, fmt.Sprintf("%d.erl", idx))
	outPath := strings.TrimSuffix(codePath, ".erl") + ".out"
	benchPath := strings.TrimSuffix(codePath, ".erl") + ".bench"
	errPath := strings.TrimSuffix(codePath, ".erl") + ".error"
	inPath := strings.TrimSuffix(codePath, ".erl") + ".in"

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
	if data, err := os.ReadFile(inPath); err == nil {
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
	// remove trailing "break" line if present
	lines := bytes.Split(output, []byte{'\n'})
	if n := len(lines); n > 0 && bytes.Equal(bytes.TrimSpace(lines[n-1]), []byte("break")) {
		lines = lines[:n-1]
		output = bytes.Join(lines, []byte{'\n'})
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

func TestErlangTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	t.Cleanup(updateSpojChecklist)
	indices := []int{1}
	if idxStr := os.Getenv("MOCHI_SPOJ_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", idxStr)
		}
		indices = []int{idx}
	}
	for _, idx := range indices {
		if ok := t.Run(fmt.Sprintf("%04d", idx), func(t *testing.T) { runSpojCase(t, idx) }); !ok {
			t.Fatalf("first failing program: %d", idx)
		}
	}
}

func updateSpojChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "erlang")
	mdPath := filepath.Join(root, "transpiler", "x", "erl", "SPOJ.md")

	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return
	}
	type ent struct {
		idx  int
		name string
	}
	var ents []ent
	for _, e := range entries {
		if strings.HasSuffix(e.Name(), ".md") {
			base := strings.TrimSuffix(e.Name(), ".md")
			i, err := strconv.Atoi(base)
			if err != nil {
				continue
			}
			name := fmt.Sprintf("Problem %d", i)
			if data, err := os.ReadFile(filepath.Join(srcDir, e.Name())); err == nil {
				line := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
				line = strings.TrimPrefix(line, "#")
				line = strings.TrimSpace(line)
				if strings.HasPrefix(line, "[") {
					if end := strings.Index(line, "]"); end >= 0 {
						name = line[1:end]
					} else {
						name = line
					}
				} else if idx := strings.Index(line, "("); idx >= 0 {
					name = strings.TrimSpace(line[:idx])
				} else if line != "" {
					name = line
				}
			}
			ents = append(ents, ent{idx: i, name: name})
		}
	}
	sort.Slice(ents, func(i, j int) bool { return ents[i].idx < ents[j].idx })

	total := len(ents)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, e := range ents {
		codePath := filepath.Join(outDir, fmt.Sprintf("%d.erl", e.idx))
		status := " "
		if _, err := os.Stat(strings.TrimSuffix(codePath, ".erl") + ".error"); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(codePath); err == nil {
			status = "✓"
			compiled++
		}
		dur := ""
		mem := ""
		benchFile := strings.TrimSuffix(codePath, ".erl") + ".bench"
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
		} else if data, err := os.ReadFile(strings.TrimSuffix(codePath, ".erl") + ".out"); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", e.idx, e.name, status, dur, mem))
	}
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Erlang SPOJ Output (%d/%d compiled and run)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("This directory contains Erlang code generated by the Mochi transpiler for SPOJ problems. Each program in `tests/spoj/x/mochi` is transpiled and executed with `escript`.\n\n")
	buf.WriteString("## Program checklist\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
