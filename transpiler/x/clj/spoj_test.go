//go:build slow

package cljt_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

var updateSpojFlag = flag.Bool("update-spoj-clj", false, "update golden files")

func spojUpdateEnabled() bool { return *updateSpojFlag }

func TestClojureTranspiler_Spoj_Golden(t *testing.T) {
	defer updateSpojReadme()
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "clj")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	srcs, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(srcs)
	for _, src := range srcs {
		idx := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, idx+".out")); err != nil && !spojUpdateEnabled() {
			continue
		}
		t.Run(idx, func(t *testing.T) { runSpojCase(t, idx) })
	}
}

func runSpojCase(t *testing.T, idx string) {
	t.Helper()
	root := findRepoRoot(t)
	src := filepath.Join(root, "tests", "spoj", "x", "mochi", idx+".mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "clj")
	codePath := filepath.Join(outDir, idx+".clj")
	outPath := filepath.Join(outDir, idx+".out")
	errPath := filepath.Join(outDir, idx+".error")
	benchPath := filepath.Join(outDir, idx+".bench")
	inPath := filepath.Join(outDir, idx+".in")

	want, err := os.ReadFile(outPath)
	if err != nil {
		if !spojUpdateEnabled() {
			t.Fatalf("read want: %v", err)
		}
	} else {
		want = bytes.TrimSpace(want)
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
	ast, err := cljt.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := cljt.Format(cljt.EmitString(ast))
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	cmd := cljCommand(codePath)
	cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	warn := []byte("WARNING: Implicit use of clojure.main with options is deprecated, use -M\n")
	got = bytes.TrimPrefix(got, warn)
	if err != nil {
		if spojUpdateEnabled() {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return
		}
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)
	idxBrace := bytes.LastIndexByte(got, '{')
	var benchData []byte
	outBytes := got
	if idxBrace >= 0 {
		outBytes = bytes.TrimSpace(got[:idxBrace])
		benchData = bytes.TrimSpace(got[idxBrace:])
	}
	if benchData != nil {
		_ = os.WriteFile(benchPath, benchData, 0o644)
	}
	if spojUpdateEnabled() {
		_ = os.WriteFile(outPath, outBytes, 0o644)
		return
	}
	_ = os.WriteFile(outPath, outBytes, 0o644)
	if want != nil && !bytes.Equal(outBytes, want) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", idx, outBytes, want)
	}
}

func updateSpojReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "clj")
	md := filepath.Join(root, "transpiler", "x", "clj", "SPOJ.md")
	srcs, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return
	}
	sort.Strings(srcs)
	total := len(srcs)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, src := range srcs {
		idx := strings.TrimSuffix(filepath.Base(src), ".mochi")
		name := idx
		if data, err := os.ReadFile(filepath.Join(srcDir, idx+".md")); err == nil {
			scanner := bufio.NewScanner(bytes.NewReader(data))
			if scanner.Scan() {
				line := strings.TrimSpace(scanner.Text())
				line = strings.TrimPrefix(line, "#")
				line = strings.TrimSpace(line)
				if strings.HasPrefix(line, "[") && strings.Contains(line, "]") {
					if end := strings.Index(line, "]"); end > 1 {
						name = line[1:end]
					}
				} else if line != "" {
					name = line
				}
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, idx+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, idx+".clj")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, idx+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				if r.DurationUS > 0 {
					dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				}
				if r.MemoryBytes > 0 {
					mem = formatBytes(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# Clojure SPOJ Transpiler\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Clojure code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/clj`.\n")
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
