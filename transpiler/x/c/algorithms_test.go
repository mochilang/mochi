//go:build slow

package ctrans_test

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

	ctrans "mochi/transpiler/x/c"
)

var updateAlgorithmsFlag = flag.Bool("update-algorithms-c", false, "update golden files")

func algUpdateEnabled() bool { return *updateAlgorithmsFlag }

func runAlgorithmCase(t *testing.T, file string) {
	t.Helper()
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", file)
	outDir := filepath.Join(root, "tests", "algorithms", "x", "C", filepath.Dir(file))
	os.MkdirAll(outDir, 0o755)
	base := strings.TrimSuffix(filepath.Base(file), ".mochi")
	codePath := filepath.Join(outDir, base+".c")
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

	code, err := transpileFile(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}
	cc, err := ctrans.EnsureCC()
	if err != nil {
		t.Fatalf("ensure cc: %v", err)
	}
	exe := filepath.Join(outDir, base)
	args := []string{codePath, "-o", exe}
	if bytes.Contains(code, []byte("<math.h>")) {
		args = append(args, "-lm")
	}
	if bytes.Contains(code, []byte("<gmp.h>")) {
		args = append(args, "-lgmp")
	}
	if out, err := exec.Command(cc, args...).CombinedOutput(); err != nil {
		_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	envv := os.Environ()
	if bench {
		envv = append(envv, "MOCHI_BENCHMARK=1")
	} else {
		envv = append(envv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), got...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	if bench {
		idx := bytes.LastIndexByte(got, '{')
		if algUpdateEnabled() {
			if idx >= 0 {
				_ = os.WriteFile(outPath, bytes.TrimSpace(got[:idx]), 0o644)
				_ = os.WriteFile(benchPath, bytes.TrimSpace(got[idx:]), 0o644)
			} else {
				_ = os.WriteFile(outPath, got, 0o644)
			}
		}
		return
	}
	if algUpdateEnabled() {
		_ = os.WriteFile(outPath, got, 0o644)
		return
	}
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
	}
}

func TestCTranspiler_Algorithms_Golden(t *testing.T) {
	root := repoRoot(t)
	t.Cleanup(updateAlgorithms)
	indexPath := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		t.Fatalf("open index: %v", err)
	}
	defer f.Close()
	type entry struct {
		idx  int
		file string
	}
	var entries []entry
	scanner := bufio.NewScanner(f)
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
		t.Fatalf("empty index")
	}
	if v := os.Getenv("MOCHI_ALG_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(entries) {
			entries = entries[n-1 : n]
		} else {
			t.Fatalf("invalid MOCHI_ALG_INDEX: %s", v)
		}
	}
	for _, e := range entries {
		testName := fmt.Sprintf("%03d_%s", e.idx, strings.ReplaceAll(strings.TrimSuffix(e.file, ".mochi"), string(os.PathSeparator), "_"))
		if ok := t.Run(testName, func(t *testing.T) { runAlgorithmCase(t, e.file) }); !ok {
			t.Fatalf("first failing program: %s", e.file)
		}
	}
}

func updateAlgorithms() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "x", "C")
	mdPath := filepath.Join(root, "transpiler", "x", "c", "ALGORITHMS.md")
	indexPath := filepath.Join(srcDir, "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		return
	}
	defer f.Close()
	type entry struct {
		idx  int
		file string
	}
	var entries []entry
	scanner := bufio.NewScanner(f)
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
	rows = append(rows, "| ---: | --- | :---: | ---: | ---: |")
	for _, e := range entries {
		name := strings.TrimSuffix(e.file, ".mochi")
		status := ""
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = ""
		} else if _, err := os.Stat(filepath.Join(outDir, name+".c")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
				data = data[idx:]
			}
			if json.Unmarshal(data, &r) == nil && r.Dur > 0 {
				dur = humanDur(time.Duration(r.Dur) * time.Microsecond)
				mem = humanSize(r.Mem)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", e.idx, name, status, dur, mem))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 -0700")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# C Algorithms Transpiler Output\n\n")
	buf.WriteString("This directory stores C code generated from Mochi programs in `tests/github/TheAlgorithms/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts)
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(mdPath, buf.Bytes(), 0o644)
}

// humanDur formats a time duration for table output.
