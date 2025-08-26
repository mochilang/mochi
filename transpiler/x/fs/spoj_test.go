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

func TestFSTranspiler_Spoj_Golden(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
	if _, err := exec.LookPath("mono"); err != nil {
		t.Skip("mono not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "human", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "human", "x", "fs")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpojReadme)

	idx := 1
	if v := os.Getenv("MOCHI_SPOJ_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 {
			idx = n
		}
	}

	src := filepath.Join(srcDir, fmt.Sprintf("%d.mochi", idx))
	inPath := filepath.Join(outDir, fmt.Sprintf("%d.in", idx))
	outPath := filepath.Join(outDir, fmt.Sprintf("%d.out", idx))
	errPath := filepath.Join(outDir, fmt.Sprintf("%d.error", idx))
	benchPath := filepath.Join(outDir, fmt.Sprintf("%d.bench", idx))
	codePath := filepath.Join(outDir, fmt.Sprintf("%d.fs", idx))
	exePath := filepath.Join(outDir, fmt.Sprintf("%d.exe", idx))

	if _, err := os.Stat(inPath); os.IsNotExist(err) {
		if data, err2 := os.ReadFile(filepath.Join(srcDir, fmt.Sprintf("%d.in", idx))); err2 == nil {
			_ = os.WriteFile(inPath, data, 0o644)
		}
	}
	if _, err := os.Stat(outPath); os.IsNotExist(err) {
		if data, err2 := os.ReadFile(filepath.Join(srcDir, fmt.Sprintf("%d.out", idx))); err2 == nil {
			_ = os.WriteFile(outPath, data, 0o644)
		}
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
	fstrans.SetBenchMain(true)
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
	if data, err := os.ReadFile(inPath); err == nil {
		run.Stdin = bytes.NewReader(data)
	}
	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)
	out, err := run.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)
	benchData := got
	if idx := bytes.LastIndexByte(benchData, '{'); idx >= 0 {
		_ = os.WriteFile(benchPath, benchData[idx:], 0o644)
		got = bytes.TrimSpace(benchData[:idx])
	} else {
		_ = os.WriteFile(benchPath, benchData, 0o644)
		got = nil
	}
	if got != nil {
		if !bytes.Equal(got, want) {
			t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
		}
	}
}

func updateSpojReadme() {
	root := findRepoRoot(&testing.T{})
	outDir := filepath.Join(root, "tests", "spoj", "human", "x", "fs")
	readme := filepath.Join(root, "transpiler", "x", "fs", "SPOJ.md")

	idxs := []int{11, 12, 13, 14, 15}
	total := len(idxs)
	compiled := 0
	rows := []string{"| Index | Name | Status | Duration | Memory |", "|------:|------|:-----:|---------:|-------:|"}
	for _, idx := range idxs {
		name := problemName(root, idx)
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.error", idx))); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.fs", idx))); err == nil {
			status = "✓"
			compiled++
		}
		dur, mem := "", ""
		benchFile := filepath.Join(outDir, fmt.Sprintf("%d.bench", idx))
		if data, err := os.ReadFile(benchFile); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
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
	buf.WriteString("# F# SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}

func problemName(root string, idx int) string {
	path := filepath.Join(root, "tests", "spoj", "human", "x", "mochi", fmt.Sprintf("%d.md", idx))
	data, err := os.ReadFile(path)
	if err != nil {
		return fmt.Sprintf("%d", idx)
	}
	line := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
	line = strings.TrimPrefix(line, "#")
	return strings.TrimSpace(line)
}
