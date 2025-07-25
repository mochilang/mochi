//go:build slow

package ctrans_test

import (
	"bufio"
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

	ctrans "mochi/transpiler/x/c"
)

func TestRosettaTranspilerGolden(t *testing.T) {
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaReadme)

	_ = updateIndex(srcDir)
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		names = []string{only + ".mochi"}
	}
	start := 0
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		start = idx - 1
		names = names[start : start+1]
	}
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		idx := start + i + 1
		if ok := t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) {
			src := filepath.Join(srcDir, nameFile)
			codePath := filepath.Join(outDir, name+".c")
			errPath := filepath.Join(outDir, name+".error")

			code, err := transpileFile(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("%v", err)
			}
			if updateEnabled() {
				norm := normalize(root, code)
				_ = os.WriteFile(codePath, norm, 0o644)
			}
			out, runErr := transpileAndRun(src)
			if runErr != nil {
				_ = os.WriteFile(errPath, []byte(runErr.Error()+"\n"), 0o644)
				t.Fatalf("%v", runErr)
			}
			_ = os.Remove(errPath)
			if updateEnabled() {
				trimmed := bytes.TrimSpace(out)
				_ = os.WriteFile(filepath.Join(outDir, name+".out"), trimmed, 0o644)
				return
			}
			wantPath := filepath.Join(outDir, name+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			got := bytes.TrimSpace(out)
			if !bytes.Equal(got, want) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		}); !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "C")
	readmePath := filepath.Join(root, "transpiler", "x", "c", "ROSETTA.md")
	_ = updateIndex(srcDir)
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 -0700")
			}
		}
	}
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "| ---: | --- | :---: | ---: | ---: |")
	for i, f := range names {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := ""
		dur := ""
		mem := ""
		outPath := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(outPath); err == nil {
			status = "x"
			compiled++
			var res struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
			}
			if json.Unmarshal(trimmed, &res) == nil && res.Dur > 0 {
				dur = humanDur(time.Duration(res.Dur) * time.Microsecond)
				mem = humanSize(res.Mem)
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# C Transpiler Rosetta Output\n\n")
	buf.WriteString("This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	buf.WriteString(fmt.Sprintf("Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func updateIndex(dir string) error {
	pattern := filepath.Join(dir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		return err
	}
	sort.Strings(files)
	var buf bytes.Buffer
	for i, f := range files {
		fmt.Fprintf(&buf, "%d %s\n", i+1, filepath.Base(f))
	}
	return os.WriteFile(filepath.Join(dir, "index.txt"), buf.Bytes(), 0o644)
}

func humanDur(d time.Duration) string {
	if d < time.Millisecond {
		return fmt.Sprintf("%dus", d.Microseconds())
	}
	if d < time.Second {
		return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000)
	}
	if d < time.Minute {
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
	return d.String()
}

func humanSize(n int64) string {
	const unit = 1024
	units := []string{"B", "KB", "MB", "GB", "TB"}
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	val := float64(n)
	exp := 0
	for val >= unit && exp < len(units)-1 {
		val /= unit
		exp++
	}
	return fmt.Sprintf("%.1f %s", val, units[exp])
}
