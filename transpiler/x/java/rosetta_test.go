//go:build slow

package javatr_test

import (
	"bufio"
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

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

// rosettaIndex optionally selects a single program by 1-based index when
// running the Rosetta tests.
var rosettaIndex = flag.Int("index", 0, "run only the N-th Rosetta program (1-based)")

func runRosettaTask(t *testing.T, name string) {
	root := repoRootDir(t)
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	codePath := filepath.Join(outDir, name+".java")
	wantOut := filepath.Join(outDir, name+".out")
	benchPath := filepath.Join(outDir, name+".bench")
	errPath := filepath.Join(outDir, name+".error")

	bench := os.Getenv("MOCHI_BENCHMARK") == "true"
	javatr.SetBenchMain(bench)

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0644)
		t.Fatalf("parse %s: %v", name, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0644)
		t.Fatalf("type %s: %v", name, errs[0])
	}
	ast, err := javatr.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0644)
		t.Fatalf("transpile %s: %v", name, err)
	}
	code := javatr.Emit(ast)
	if err := os.WriteFile(codePath, code, 0644); err != nil {
		t.Fatalf("write code %s: %v", name, err)
	}

	className := "Main"
	tmp := t.TempDir()
	srcTmp := filepath.Join(tmp, className+".java")
	if err := os.WriteFile(srcTmp, code, 0644); err != nil {
		t.Fatalf("tmp write %s: %v", name, err)
	}
	cmd := exec.Command("javac", "-d", tmp, srcTmp)
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0644)
		t.Fatalf("javac %s: %v", name, err)
	}
	cmd = exec.Command("java", "-cp", tmp, className)
	runEnv := []string{"MOCHI_ROOT=" + root}
	if bench {
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
	} else {
		runEnv = append(runEnv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = append(os.Environ(), runEnv...)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err = cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0644)
		t.Fatalf("run %s: %v", name, err)
	}
	_ = os.Remove(errPath)
	if bench {
		if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
			got = got[idx:]
		}
		if shouldUpdateRosetta() {
			_ = os.WriteFile(benchPath, got, 0644)
		}
		return
	}
	if shouldUpdateRosetta() {
		_ = os.WriteFile(wantOut, append(got, '\n'), 0644)
	} else if want, err := os.ReadFile(wantOut); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
		}
	}
}

func TestJavaTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("javac"); err != nil {
		t.Skip("javac not installed")
	}
	if _, err := exec.LookPath("java"); err != nil {
		t.Skip("java runtime not installed")
	}
	root := repoRootDir(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)

	if *rosettaIndex > 0 {
		if *rosettaIndex < 1 || *rosettaIndex > len(files) {
			t.Fatalf("invalid -index: %d", *rosettaIndex)
		}
		files = files[*rosettaIndex-1 : *rosettaIndex]
	} else if v := os.Getenv("MOCHI_ROSETTA_ONLY"); v != "" {
		files = []string{filepath.Join(root, "tests", "rosetta", "x", "Mochi", v+".mochi")}
	} else if v := os.Getenv("MOCHI_ROSETTA_INDEX"); v != "" {
		idx, err := strconv.Atoi(v)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX %s", v)
		}
		files = files[idx-1 : idx]
	} else if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < len(files) {
			files = files[:n]
		}
	}

	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosetta)
	var firstFail string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		ok := t.Run(name, func(t *testing.T) { runRosettaTask(t, name) })
		if !ok {
			firstFail = name
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateRosetta() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Java")
	readmePath := filepath.Join(root, "transpiler", "x", "java", "ROSETTA.md")
	indexPath := filepath.Join(srcDir, "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		return
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	type entry struct {
		idx  int
		file string
	}
	var entries []entry
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) != 2 {
			continue
		}
		n, err := strconv.Atoi(parts[0])
		if err != nil {
			continue
		}
		entries = append(entries, entry{idx: n, file: parts[1]})
	}
	total := len(entries)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for _, e := range entries {
		name := strings.TrimSuffix(filepath.Base(e.file), ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".java")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				var r struct {
					DurationUS  int64 `json:"duration_us"`
					MemoryBytes int64 `json:"memory_bytes"`
				}
				if json.Unmarshal(data[idx:], &r) == nil && r.DurationUS > 0 {
					dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
					mem = formatBytes(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", e.idx, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# Java Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Java code for programs in `tests/rosetta/x/Mochi`. Each program has a `.java` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	fmt.Fprintf(&buf, "## Rosetta Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0644)
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
