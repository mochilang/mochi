//go:build slow

package pas_test

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
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

func runSpojCase(t *testing.T, idx int) {
	t.Helper()
	fpc := ensureFPCQuick(t)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "spoj", "x", "pas")
	os.MkdirAll(outDir, 0o755)

	base := fmt.Sprintf("%d", idx)
	src := filepath.Join(root, "tests", "spoj", "x", "mochi", base+".mochi")
	codePath := filepath.Join(outDir, base+".pas")
	outPath := filepath.Join(outDir, base+".out")
	inPath := filepath.Join(outDir, base+".in")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
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
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	exe := filepath.Join(outDir, base)
	if out, err := exec.Command(fpc, codePath, "-o"+exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	start := time.Now()
	out, err := cmd.CombinedOutput()
	dur := time.Since(start)
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)
	_ = os.WriteFile(outPath, got, 0o644)
	benchData := struct {
		DurationUS  int64 `json:"duration_us"`
		MemoryBytes int64 `json:"memory_bytes"`
	}{dur.Microseconds(), 0}
	if b, err := json.Marshal(benchData); err == nil {
		_ = os.WriteFile(benchPath, append(b, '\n'), 0o644)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch: got %s want %s", got, want)
	}
}

func TestPascalTranspiler_SPOJ(t *testing.T) {
	t.Cleanup(updateSPOJChecklist)
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc not installed")
	}
	idx := 1
	if v := os.Getenv("SPOJ_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			idx = n
		}
	}
	runSpojCase(t, idx)
}

func updateSPOJChecklist() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "pas")
	mdPath := filepath.Join(root, "transpiler", "x", "pas", "SPOJ.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	rows := []string{"| Index | Name | Status | Duration | Memory |", "|------:|------|:-----:|---------:|-------:|"}
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			if lines := strings.Split(string(data), "\n"); len(lines) > 0 {
				name = strings.TrimSpace(strings.TrimPrefix(lines[0], "#"))
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, base+".pas")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", base, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Pascal code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/pas`.\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	buf.WriteString("Last updated: " + time.Now().Format("2006-01-02 15:04 MST") + "\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}
