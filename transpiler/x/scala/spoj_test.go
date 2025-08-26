//go:build slow

package scalat_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	scalat "mochi/transpiler/x/scala"
	"mochi/types"
)

func TestScalaTranspiler_SPOJ_Golden(t *testing.T) {
	ensureScala(t)
	root := repoRoot()
	src := filepath.Join(root, "tests", "spoj", "x", "mochi", "1.mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "scala")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJChecklist)

	bench := true
	scalat.SetBenchMain(bench)

	codePath := filepath.Join(outDir, "1.scala")
	outPath := filepath.Join(outDir, "1.out")
	errPath := filepath.Join(outDir, "1.error")
	benchPath := filepath.Join(outDir, "1.bench")
	inPath := filepath.Join(outDir, "1.in")

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
	ast, err := scalat.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := scalat.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	tmp := t.TempDir()
	if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command("scala", "-cp", tmp, "Main")
	cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)

	outBytes, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(outBytes)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	benchData := got
	outPart := got
	if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
		outPart = bytes.TrimSpace(got[:idx])
		benchData = got[idx:]
	} else {
		benchData = nil
	}
	_ = os.WriteFile(outPath, outPart, 0o644)
	if benchData != nil {
		_ = os.WriteFile(benchPath, benchData, 0o644)
	}
	if want != nil && len(want) > 0 {
		if !bytes.Equal(outPart, want) {
			t.Errorf("output mismatch\nGot: %s\nWant: %s", outPart, want)
		}
	}
}

func updateSPOJChecklist() {
	root := repoRoot()
	outDir := filepath.Join(root, "tests", "spoj", "x", "scala")
	readmePath := filepath.Join(root, "transpiler", "x", "scala", "SPOJ.md")

	type row struct {
		Index  int
		Name   string
		Status string
		Dur    string
		Mem    string
	}

	idx := 1
	name := "Life, the Universe, and Everything"
	status := " "
	dur := ""
	mem := ""
	if _, err := os.Stat(filepath.Join(outDir, "1.error")); err == nil {
		status = "error"
	} else if _, err := os.Stat(filepath.Join(outDir, "1.scala")); err == nil {
		status = "✓"
		if data, err := os.ReadFile(filepath.Join(outDir, "1.bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &r) == nil && r.DurationUS > 0 {
				dur = humanDuration(r.DurationUS)
				mem = humanSize(r.MemoryBytes)
			}
		}
	}
	rows := []row{{Index: idx, Name: name, Status: status, Dur: dur, Mem: mem}}

	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for _, r := range rows {
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", r.Index, r.Name, r.Status, r.Dur, r.Mem))
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
	buf.WriteString("# Scala SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", func() int {
		if status == "✓" {
			return 1
		}
		return 0
	}(), 1)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
