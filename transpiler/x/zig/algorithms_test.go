//go:build slow

package zigt_test

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
	zigt "mochi/transpiler/x/zig"
	"mochi/types"
)

// TestZigTranspiler_Algorithms_Golden transpiles Mochi algorithms to Zig,
// executes them, and compares their output with the expected results. The
// generated code and outputs are stored under tests/algorithms/transpiler/Zig.
func TestZigTranspiler_Algorithms_Golden(t *testing.T) {
	if _, err := exec.LookPath("zig"); err != nil {
		t.Skip("zig not installed")
	}
	root := algRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "Zig")
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
	var firstFail string
	for i, src := range files {
		rel := strings.TrimSuffix(names[i], ".mochi")
		name := strings.ReplaceAll(rel, string(os.PathSeparator), "_")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, rel+".zig")
			outPath := filepath.Join(outDir, rel+".out")
			errPath := filepath.Join(outDir, rel+".error")
			benchPath := filepath.Join(outDir, rel+".bench")
			os.MkdirAll(filepath.Dir(codePath), 0o755)

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
			bench := true
			ast, err := zigt.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ast.Emit()
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("zig", "run", codePath, "-lc")
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)

			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), got...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			benchData := got
			progOut := got
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				progOut = bytes.TrimSpace(got[:idx])
				_ = os.WriteFile(outPath, progOut, 0o644)
				benchData = got[idx:]
			} else {
				_ = os.WriteFile(outPath, progOut, 0o644)
				benchData = nil
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if len(want) > 0 {
				if !bytes.Equal(progOut, want) {
					t.Errorf("output mismatch\nGot: %s\nWant: %s", progOut, want)
				}
			}
		})
		if !ok {
			firstFail = rel
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

// algRepoRoot finds the repository root by walking up until go.mod is found.
func algRepoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

// updateAlgorithmsReadme writes the ALGORITHMS.md checklist with current progress.
func updateAlgorithmsReadme() {
	root := algRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "Zig")
	readme := filepath.Join(root, "transpiler", "x", "zig", "ALGORITHMS.md")

	names, err := readIndex(srcDir)
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, name := range names {
		base := strings.TrimSuffix(name, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, base+".zig")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, base+".bench")
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
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, base, status, dur, mem))
	}
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Zig Algorithms Transpiler Output\n\n")
	buf.WriteString("Generated Zig code for Algorithms tasks lives under `tests/algorithms/transpiler/Zig`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Program Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteByte('\n')
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
}
