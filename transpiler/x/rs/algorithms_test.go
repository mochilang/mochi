//go:build slow

package rs_test

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
	"unicode"

	"mochi/parser"
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

var updateAlgorithmsFlag = flag.Bool("update-algorithms-rs", false, "update golden files")

func algUpdateEnabled() bool { return *updateAlgorithmsFlag }

func ensureRustc(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
}

func runAlgorithmCase(t *testing.T, file string) {
	t.Helper()
	ensureRustc(t)
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", file)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Rust", filepath.Dir(file))
	os.MkdirAll(outDir, 0o755)
	base := strings.TrimSuffix(filepath.Base(file), ".mochi")
	codePath := filepath.Join(outDir, base+".rs")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	bench := os.Getenv("MOCHI_BENCHMARK") != ""

	want, err := os.ReadFile(outPath)
	if err != nil {
		if !algUpdateEnabled() {
			t.Fatalf("read want: %v", err)
		}
	} else if !bench {
		want = bytes.TrimSpace(want)
	}

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
	gprog, err := rs.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := rs.Emit(gprog)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	tmpDir := t.TempDir()
	bin := filepath.Join(tmpDir, base)
	crate := strings.Map(func(r rune) rune {
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
			return r
		}
		return '_'
	}, base)
	if out, err := exec.Command("rustc", codePath, "-O", "--crate-name", crate, "-o", bin).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, out, 0o644)
		t.Fatalf("rustc: %v", err)
	}

	cmd := exec.Command(bin)
	envv := os.Environ()
	if !bench {
		envv = append(envv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		if algUpdateEnabled() {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		} else {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			t.Fatalf("run: %v", err)
		}
	} else {
		_ = os.Remove(errPath)
	}

	if bench {
		if algUpdateEnabled() {
			outBytes := got
			if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
				_ = os.WriteFile(benchPath, outBytes[idx:], 0o644)
				outBytes = bytes.TrimSpace(outBytes[:idx])
				_ = os.WriteFile(outPath, outBytes, 0o644)
			} else {
				_ = os.WriteFile(benchPath, outBytes, 0o644)
			}
		}
		return
	}

	if algUpdateEnabled() {
		_ = os.WriteFile(outPath, got, 0o644)
		return
	}

	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want---\n%s", file, got, want)
	}
}

func TestRsTranspiler_Algorithms_Golden(t *testing.T) {
	t.Cleanup(updateAlgorithms)
	root := repoRoot(t)
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
	if v := os.Getenv("MOCHI_ALG_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(entries) {
			entries = entries[n-1 : n]
		} else {
			t.Fatalf("invalid MOCHI_ALG_INDEX: %s", v)
		}
	}
	max := len(entries)
	if v := os.Getenv("ALGORITHMS_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < max {
			max = n
		}
	}
	for _, e := range entries[:max] {
		idxName := fmt.Sprintf("%03d_%s", e.idx, strings.TrimSuffix(filepath.Base(e.file), ".mochi"))
		if ok := t.Run(idxName, func(t *testing.T) { runAlgorithmCase(t, e.file) }); !ok {
			t.Fatalf("first failing program: %s", e.file)
		}
	}
}

func updateAlgorithms() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "Rust")
	md := filepath.Join(root, "transpiler", "x", "rs", "ALGORITHMS.md")
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
		name := strings.TrimSuffix(e.file, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, name+".rs")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
				if r.DurationUS > 0 {
					dur = humanDuration(r.DurationUS)
				}
				if r.MemoryBytes > 0 {
					mem = humanSize(r.MemoryBytes)
				}
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", e.idx, name, status, dur, mem))
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	ts := time.Now().In(loc).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Algorithms Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Rust code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/Rust`.\n")
	buf.WriteString("Last updated: " + ts + "\n\n")
	fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}
