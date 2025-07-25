//go:build rosetta

package erl_test

import (
	"bytes"
	"encoding/json"
	"flag"
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

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func updateEnabled() bool { return *update }

func TestRosettaTranspile(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("list sources: %v", err)
	}
	sort.Strings(files)
	if len(files) == 0 {
		t.Fatal("no mochi files found")
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	} else {
		max := len(files)
		if v := os.Getenv("ROSETTA_MAX"); v != "" {
			if n, err := strconv.Atoi(v); err == nil && n < max {
				max = n
			}
		}
		files = files[:max]
	}

	var firstFail string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) { runRosetta(t, src, name, outDir) })
		if !ok && firstFail == "" {
			firstFail = name
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func runRosetta(t *testing.T, srcPath, name, outDir string) {
	erlFile := filepath.Join(outDir, name+".erl")
	outFile := filepath.Join(outDir, name+".out")
	errFile := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(srcPath)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("parse error: %v", err)), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("type error: %v", errs[0])), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true"
	erl.SetBenchMain(bench)
	ast, err := erl.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("transpile error: %v", err)), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if updateEnabled() {
		if err := os.WriteFile(erlFile, code, 0o644); err != nil {
			t.Fatalf("write erl: %v", err)
		}
	} else {
		_ = os.WriteFile(erlFile, code, 0o644)
	}

	cmd := exec.Command("escript", erlFile)
	envs := []string{"MOCHI_NOW_SEED=1"}
	if bench {
		envs = append(envs, "MOCHI_BENCHMARK=1")
	}
	cmd.Env = append(os.Environ(), envs...)
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		if updateEnabled() {
			_ = os.WriteFile(errFile, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0o644)
		}
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(buf.Bytes())
	if updateEnabled() {
		_ = os.WriteFile(outFile, got, 0o644)
		_ = os.Remove(errFile)
		return
	}
	if bench {
		_ = os.WriteFile(outFile, got, 0o644)
		_ = os.Remove(errFile)
		return
	}
	want, err := os.ReadFile(outFile)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
	_ = os.Remove(errFile)
}

func countRosetta() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return 0, 0
	}
	total := len(files)
	compiled := 0
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
	readmePath := filepath.Join(root, "transpiler", "x", "erl", "ROSETTA.md")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		return
	}
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
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
	buf.WriteString("# Rosetta Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Erlang code from programs in `tests/rosetta/x/Mochi` lives in `tests/rosetta/transpiler/erl`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateRosettaReadme()
	os.Exit(code)
}
