//go:build slow

package scheme_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	scheme "mochi/transpiler/x/scheme"
	"mochi/types"
)

var updateAlgorithmsFlag = flag.Bool("update-algorithms-scheme", false, "update golden files")

func algUpdateEnabled() bool { return *updateAlgorithmsFlag }

func runAlgorithmCase(t *testing.T, file string) {
	t.Helper()
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("scheme not installed")
	}
	root := findRepoRoot2(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", file)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Scheme", filepath.Dir(file))
	os.MkdirAll(outDir, 0o755)
	base := strings.TrimSuffix(filepath.Base(file), ".mochi")
	codePath := filepath.Join(outDir, base+".scm")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	want, err := os.ReadFile(outPath)
	if err != nil && !algUpdateEnabled() {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	expected, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".out")
	expected = bytes.TrimSpace(expected)

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
	scheme.SetBenchMain(true)
	ast, err := scheme.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := scheme.Format(scheme.EmitString(ast))
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	cmd := exec.Command("chibi-scheme", "-q", "-m", "chibi", "-m", "srfi.1", "-m", "srfi.69", "-m", "scheme.sort", "-m", "chibi.string", codePath)
	runEnv := append(os.Environ(), "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
	cmd.Env = runEnv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if len(got) > 0 {
		lines := bytes.Split(got, []byte("\n"))
		got = bytes.Join(filterWarningLines(lines), []byte("\n"))
		got = bytes.TrimSpace(got)
	}
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), got...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	benchData := got
	if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
		progOut := bytes.TrimSpace(got[:idx])
		benchData = got[idx:]
		if algUpdateEnabled() {
			_ = os.WriteFile(outPath, progOut, 0o644)
			_ = os.WriteFile(benchPath, benchData, 0o644)
		}
		if len(expected) > 0 && !bytes.Equal(progOut, expected) {
			t.Errorf("output mismatch for %s\nGot: %s\nWant: %s", file, progOut, expected)
		}
		if !algUpdateEnabled() && len(want) > 0 && !bytes.Equal(progOut, want) {
			t.Errorf("golden mismatch for %s\nGot: %s\nWant: %s", file, progOut, want)
		}
	} else {
		if algUpdateEnabled() {
			_ = os.WriteFile(outPath, got, 0o644)
		}
		if len(expected) > 0 && !bytes.Equal(got, expected) {
			t.Errorf("output mismatch for %s\nGot: %s\nWant: %s", file, got, expected)
		}
		if !algUpdateEnabled() && len(want) > 0 && !bytes.Equal(got, want) {
			t.Errorf("golden mismatch for %s\nGot: %s\nWant: %s", file, got, want)
		}
		benchData = nil
	}
}

func filterWarningLines(lines [][]byte) [][]byte {
	var out [][]byte
	for _, ln := range lines {
		if bytes.HasPrefix(bytes.TrimSpace(ln), []byte("WARNING:")) {
			continue
		}
		out = append(out, ln)
	}
	return out
}

func TestSchemeTranspiler_Algorithms_Golden(t *testing.T) {
	root := findRepoRoot2(t)
	t.Cleanup(updateAlgorithmsChecklist)
	indexPath := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		t.Fatalf("open index: %v", err)
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
	if err := scanner.Err(); err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(entries) == 0 {
		t.Fatal("empty index")
	}
	if v := os.Getenv("MOCHI_ALGORITHMS_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(entries) {
			entries = entries[n-1 : n]
		} else {
			t.Fatalf("invalid MOCHI_ALGORITHMS_INDEX: %s", v)
		}
	}
	for _, e := range entries {
		idxName := fmt.Sprintf("%03d_%s", e.idx, strings.TrimSuffix(filepath.Base(e.file), ".mochi"))
		if ok := t.Run(idxName, func(t *testing.T) { runAlgorithmCase(t, e.file) }); !ok {
			t.Fatalf("first failing program: %s", e.file)
		}
	}
}

func updateAlgorithmsChecklist() {
	root := findRepoRoot2(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Scheme")
	readmePath := filepath.Join(root, "transpiler", "x", "scheme", "ALGORITHMS.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".scm")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				if json.Unmarshal(data[idx:], &r) == nil {
					dur = humanDuration(r.DurationUS)
					mem = humanSize(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Algorithms Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Scheme code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/Scheme`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
