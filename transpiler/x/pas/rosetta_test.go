//go:build slow

package pas_test

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
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

var update = flag.Bool("update-rosetta-pas", false, "update golden files")

func updateEnabled() bool { return *update }

func repoRootRosetta(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	t.Fatal("go.mod not found")
	return ""
}

func ensureFPCRosetta(t *testing.T) string {
	t.Helper()
	path, err := exec.LookPath("fpc")
	if err != nil {
		t.Skip("fpc not installed")
	}
	return path
}

func runRosettaCase(t *testing.T, name string) {
	t.Helper()
	fpc := ensureFPCRosetta(t)
	root := repoRootRosetta(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Pascal")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	pas.SetBenchMain(bench)
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	codePath := filepath.Join(outDir, name+".pas")
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	exe := filepath.Join(outDir, name)
	if out, err := exec.Command(fpc, codePath, "-o"+exe).CombinedOutput(); err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	cmd := exec.Command(exe)
	if bench {
		cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1")
	}
	if in, err := os.ReadFile(filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(in)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	got := bytes.TrimSpace(out)
	benchPath := filepath.Join(outDir, name+".bench")
	outPath := filepath.Join(outDir, name+".out")
	if bench {
		idx := bytes.LastIndexByte(got, '{')
		if idx >= 0 {
			benchData := bytes.TrimSpace(got[idx:])
			_ = os.WriteFile(benchPath, benchData, 0o644)
			got = bytes.TrimSpace(got[:idx])
		}
	}
	if updateEnabled() {
		_ = os.WriteFile(outPath, got, 0o644)
		return
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("%s output mismatch: got %s want %s", name, got, want)
	}
}

func listRosettaPrograms(t *testing.T) []string {
	t.Helper()
	root := repoRootRosetta(t)
	indexPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		t.Fatalf("open index: %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var names []string
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, strings.TrimSuffix(parts[1], ".mochi"))
		}
	}
	if err := scanner.Err(); err != nil {
		t.Fatalf("read index: %v", err)
	}
	return names
}

func TestPascalTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("fpc"); err != nil {
		t.Skip("fpc not installed")
	}
	t.Cleanup(updateRosettaChecklist)
	programs := listRosettaPrograms(t)
	if len(programs) == 0 {
		t.Fatalf("no rosetta programs found")
	}

	idx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			idx = n
		}
	}
	if idx < 1 || idx > len(programs) {
		t.Fatalf("index %d out of range (1-%d)", idx, len(programs))
	}
	name := programs[idx-1]
	t.Logf("running %s (index %d)", name, idx)
	runRosettaCase(t, name)
}

func updateRosettaChecklist() {
	root := repoRootRosetta(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Pascal")
	readmePath := filepath.Join(root, "transpiler", "x", "pas", "ROSETTA.md")

	_ = updateIndex(srcDir)

	indexPath := filepath.Join(srcDir, "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		return
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var names []string
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, strings.TrimSuffix(parts[1], ".mochi"))
		}
	}
	if err := scanner.Err(); err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, name := range names {
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave blank
		} else if _, err := os.Stat(filepath.Join(outDir, name+".pas")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(outDir, name+".bench")
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
		} else if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
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
	buf.WriteString("# Pascal Rosetta Transpiler\n\n")
	buf.WriteString("Generated Pascal code for Rosetta tasks lives under `tests/rosetta/transpiler/Pascal`.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Checklist (%d/%d) - updated %s\n", compiled, total, ts)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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
