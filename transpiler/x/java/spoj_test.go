//go:build slow

package javatr_test

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
	javatr "mochi/transpiler/x/java"
	"mochi/types"
)

var updateSpojFlag = flag.Bool("update-spoj-java", false, "update golden files")

func spojUpdateEnabled() bool { return *updateSpojFlag }

func runSpojCase(t *testing.T, idx string, srcPath string) {
	t.Helper()
	ensureJava(t)
	root := repoRootDir(t)
	outDir := filepath.Join(root, "tests", "spoj", "x", "java")
	codePath := filepath.Join(outDir, idx+".java")
	outPath := filepath.Join(outDir, idx+".out")
	errPath := filepath.Join(outDir, idx+".error")
	inPath := filepath.Join(outDir, idx+".in")

	bench := os.Getenv("MOCHI_BENCHMARK") != ""
	want, err := os.ReadFile(outPath)
	if err != nil {
		if !spojUpdateEnabled() {
			t.Fatalf("read want: %v", err)
		}
	} else if !bench {
		want = bytes.TrimSpace(want)
	}

	prog, err := parser.Parse(srcPath)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	javatr.SetBenchMain(bench)
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := javatr.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	tmp := t.TempDir()
	mainPath := filepath.Join(tmp, "Main.java")
	if err := os.WriteFile(mainPath, code, 0o644); err != nil {
		t.Fatalf("tmp write: %v", err)
	}
	cmd := exec.Command("javac", mainPath)
	cmd.Dir = tmp
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("javac: %v", err)
	}
	cmd = exec.Command("java", "-cp", tmp, "Main")
	envv := os.Environ()
	envv = append(envv, "MOCHI_ROOT="+root)
	if bench {
		envv = append(envv, "MOCHI_BENCHMARK=1")
	} else {
		envv = append(envv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envv
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err = cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)
	if bench {
		benchPath := filepath.Join(outDir, idx+".bench")
		if spojUpdateEnabled() {
			_ = os.WriteFile(benchPath, got, 0o644)
		}
		return
	}
	if spojUpdateEnabled() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
	} else if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch:\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
	}
}

func TestJavaTranspiler_SPOJ_Golden(t *testing.T) {
	ensureJava(t)
	root := repoRootDir(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "java")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpoj)
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	if v := os.Getenv("SPOJ_INDEX"); v != "" {
		n, err := strconv.Atoi(v)
		if err == nil && n >= 1 && n <= len(files) {
			files = files[n-1 : n]
		} else {
			t.Fatalf("invalid SPOJ_INDEX: %s", v)
		}
	}
	var firstFail string
	for _, src := range files {
		idx := strings.TrimSuffix(filepath.Base(src), ".mochi")
		inPath := filepath.Join(outDir, idx+".in")
		outPath := filepath.Join(outDir, idx+".out")
		if _, err := os.Stat(inPath); err != nil {
			continue
		}
		if _, err := os.Stat(outPath); err != nil {
			continue
		}
		ok := t.Run(idx, func(t *testing.T) { runSpojCase(t, idx, src) })
		if !ok {
			firstFail = idx
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateSpoj() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "java")
	md := filepath.Join(root, "transpiler", "x", "java", "SPOJ.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, f := range files {
		idx := strings.TrimSuffix(filepath.Base(f), ".mochi")
		name := idx
		if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".md"); err == nil {
			if line := firstLine(string(data)); line != "" {
				name = line
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, idx+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, idx+".java")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, idx+".bench")); err == nil {
			if rdur, rmem, ok := parseBench(data); ok {
				dur = formatDuration(rdur)
				mem = formatBytes(rmem)
			}
		}
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Java code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/java`.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	fmt.Fprintf(&buf, "## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func firstLine(s string) string {
	for _, line := range strings.Split(s, "\n") {
		line = strings.TrimSpace(strings.TrimPrefix(line, "#"))
		if line != "" {
			return line
		}
	}
	return ""
}

func parseBench(data []byte) (time.Duration, int64, bool) {
	data = bytes.TrimSpace(data)
	var r struct {
		DurationUS  int64 `json:"duration_us"`
		MemoryBytes int64 `json:"memory_bytes"`
	}
	if json.Unmarshal(data, &r) == nil && r.DurationUS > 0 {
		return time.Duration(r.DurationUS) * time.Microsecond, r.MemoryBytes, true
	}
	return 0, 0, false
}
