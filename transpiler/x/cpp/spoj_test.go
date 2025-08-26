//go:build slow

package cpp_test

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
	cpp "mochi/transpiler/x/cpp"
	"mochi/types"
)

func TestCPPTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "cpp")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpojReadme)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		bi, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < bi
	})
	var filtered []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, base+".in")); err == nil {
			filtered = append(filtered, f)
		}
	}
	files = filtered
	if len(files) == 0 {
		t.Fatalf("no test cases in %s", srcDir)
	}
	if idxStr := os.Getenv("MOCHI_SPOJ_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	var passed, failed int
	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		idx, _ := strconv.Atoi(base)
		testName := fmt.Sprintf("%03d", idx)
		ok := t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".cpp")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")
			benchPath := filepath.Join(outDir, base+".bench")
			inPath := filepath.Join(outDir, base+".in")

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
			cpp.SetBenchMain(true)
			ast, err := cpp.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ast.Emit()
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			bin := filepath.Join(outDir, base)
			args := []string{codePath, "-std=c++20", "-O2", "-o", bin}
			if ast.UseSHA256 || ast.UseMD5 {
				args = append(args, "-lcrypto")
			}
			if out, err := exec.Command("g++", args...).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("compile: %v", err)
			}
			defer os.Remove(bin)
			cmd := exec.Command(bin)
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(inPath); err == nil {
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
				firstFail = base
			}
			break
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateSpojReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "cpp")
	readmePath := filepath.Join(root, "transpiler", "x", "cpp", "SPOJ.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		bi, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < bi
	})
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:------:|---------:|-------:|")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			lines := strings.Split(string(data), "\n")
			if len(lines) > 0 {
				line := strings.TrimSpace(lines[0])
				if strings.HasPrefix(line, "# [") {
					if end := strings.Index(line[3:], "]"); end >= 0 {
						name = line[3 : 3+end]
					}
				}
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			// keep failure
		} else if _, err := os.Stat(filepath.Join(outDir, base+".cpp")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
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
	buf.WriteString("# C++ SPOJ Transpiler Output\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
