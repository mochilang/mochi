//go:build slow

package cpp_test

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
	cpp "mochi/transpiler/x/cpp"
	"mochi/types"
)

func TestCPPTranspiler_Algorithms_Golden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "CPP")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateAlgorithmsReadme)

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
			codePath := filepath.Join(outDir, rel+".cpp")
			outPath := filepath.Join(outDir, rel+".out")
			errPath := filepath.Join(outDir, rel+".error")
			benchPath := filepath.Join(outDir, rel+".bench")
			os.MkdirAll(filepath.Dir(codePath), 0o755)

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
			cpp.SetBenchMain(bench)
			ast, err := cpp.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ast.Emit()
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			bin := filepath.Join(outDir, strings.ReplaceAll(rel, string(os.PathSeparator), "_"))
			args := []string{codePath, "-std=c++20", "-o", bin}
			if ast.UseSHA256 || ast.UseMD5 {
				args = append(args, "-lcrypto")
			}
			if out, err := exec.Command("g++", args...).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			defer os.Remove(bin)
			cmd := exec.Command(bin)
			envv := append(os.Environ(), "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
			cmd.Env = envv
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)

			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			benchData := got
			var gotOut []byte
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				gotOut = bytes.TrimSpace(got[:idx])
				benchData = got[idx:]
			} else {
				gotOut = got
				benchData = nil
			}
			if len(want) > 0 && !bytes.Equal(gotOut, want) {
				t.Errorf("output mismatch\nGot: %s\nWant: %s", gotOut, want)
			}
			if err := os.WriteFile(outPath, gotOut, 0o644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
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

func updateAlgorithmsReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "CPP")
	readmePath := filepath.Join(root, "transpiler", "x", "cpp", "ALGORITHMS.md")

	names, err := readIndex(srcDir)
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:------:|---------:|-------:|")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave as failure
		} else if _, err := os.Stat(filepath.Join(outDir, name+".cpp")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
			}
			if json.Unmarshal(trimmed, &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	tsRaw, _ := exec.Command("git", "-C", root, "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# C++ Algorithms Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
