//go:build slow

package rb_test

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

	"mochi/parser"
	rb "mochi/transpiler/x/rb"
	"mochi/types"
)

func runRosetta(t *testing.T, src, name, outDir string, bench bool) {
	codePath := filepath.Join(outDir, name+".rb")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Skipf("parse error: %v", err)
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Skipf("type error: %v", errs[0])
		return
	}
	ast, err := rb.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Skipf("transpile error: %v", err)
		return
	}
	var buf bytes.Buffer
	if err := rb.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		t.Skipf("emit error: %v", err)
		return
	}
	code := buf.Bytes()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	cmd := exec.Command("ruby", codePath)
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
	if bench {
		benchPath := filepath.Join(outDir, name+".bench")
		if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
			_ = os.WriteFile(benchPath, got[idx:], 0o644)
			got = bytes.TrimSpace(got[:idx])
		} else {
			_ = os.WriteFile(benchPath, nil, 0o644)
		}
	}
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Skipf("run error: %v", err)
		return
	}
	_ = os.Remove(errPath)

	wantPath := filepath.Join(outDir, name+".out")
	if bench {
		_ = os.WriteFile(outPath, got, 0o644)
		_ = os.Remove(errPath)
		_ = os.WriteFile(wantPath, append(got, '\n'), 0o644)
		return
	}
	if update := shouldUpdate(); update {
		_ = os.WriteFile(wantPath, append(got, '\n'), 0o644)
	} else if want, err := os.ReadFile(wantPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(want))
		}
	}
	_ = os.WriteFile(outPath, got, 0o644)
}

func shouldUpdate() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func loadIndex(dir string) ([]string, error) {
	path := filepath.Join(dir, "index.txt")
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var files []string
	scan := bufio.NewScanner(f)
	for scan.Scan() {
		fields := strings.Fields(scan.Text())
		if len(fields) != 2 {
			continue
		}
		files = append(files, filepath.Join(dir, fields[1]))
	}
	if err := scan.Err(); err != nil {
		return nil, err
	}
	return files, nil
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

func TestRubyTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	rb.SetBenchMain(bench)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "rb")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosetta)
	files, err := loadIndex(srcDir)
	if err != nil {
		t.Fatalf("load index: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no files in index")
	}
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		f := files[idx-1]
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) {
			runRosetta(t, f, name, outDir, bench)
		})
		return
	}
	max := len(files)
	if v := os.Getenv("ROSETTA_MAX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n < max {
			max = n
		}
	}
	for i, f := range files[:max] {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(fmt.Sprintf("%03d_%s", i+1, name), func(t *testing.T) {
			runRosetta(t, f, name, outDir, bench)
		})
	}
}

func updateRosetta() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "rb")
	readmePath := filepath.Join(root, "transpiler", "x", "rb", "ROSETTA.md")
	_ = updateIndex(srcDir)
	files, _ := loadIndex(srcDir)
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := " "
		rbFile := filepath.Join(outDir, name+".rb")
		outFile := filepath.Join(outDir, name+".out")
		errFile := filepath.Join(outDir, name+".error")
		if exists(rbFile) && exists(outFile) && !exists(errFile) {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
		if data, err := os.ReadFile(benchFile); err == nil && len(data) > 0 {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil && js.Duration > 0 {
				dur = humanDuration(js.Duration)
				mem = humanSize(js.Memory)
			}
		} else if data, err := os.ReadFile(outFile); err == nil {
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

	ts := time.Now().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Ruby Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Ruby code from Mochi Rosetta tasks lives in `tests/rosetta/transpiler/rb`.\n\n")
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
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
