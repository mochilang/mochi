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
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	scheme "mochi/transpiler/x/scheme"
	"mochi/types"
)

var updateSpojFlag = flag.Bool("update-spoj-scheme", false, "update golden files")

func spojUpdateEnabled() bool { return *updateSpojFlag }

func TestSchemeTranspiler_Spoj_Golden(t *testing.T) {
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("scheme not installed")
	}
	root := findRepoRoot2(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "scheme")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSPOJChecklist)

	all, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(all)
	var files []string
	for _, f := range all {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, base+".out")); err == nil {
			files = append(files, f)
		}
	}
	if idxStr := os.Getenv("MOCHI_SPOJ_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(all) {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", idxStr)
		}
		target := filepath.Join(srcDir, fmt.Sprintf("%d.mochi", idx))
		files = []string{target}
	}
	if len(files) == 0 {
		t.Skip("no golden files; set MOCHI_SPOJ_INDEX to run a specific program")
	}

	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(base, func(t *testing.T) {
			runSpojCase(t, src, outDir)
		})
	}
}

func runSpojCase(t *testing.T, src, outDir string) {
	t.Helper()
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".scm")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")
	inPath := filepath.Join(outDir, base+".in")

	want, err := os.ReadFile(outPath)
	if err != nil && !spojUpdateEnabled() {
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
	if data, err := os.ReadFile(inPath); err == nil {
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

	if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
		progOut := bytes.TrimSpace(got[:idx])
		benchData := got[idx:]
		if spojUpdateEnabled() {
			_ = os.WriteFile(outPath, progOut, 0o644)
			_ = os.WriteFile(benchPath, benchData, 0o644)
		}
		if !spojUpdateEnabled() && len(want) > 0 && !bytes.Equal(progOut, want) {
			t.Errorf("output mismatch\nGot: %s\nWant: %s", progOut, want)
		}
	} else {
		if spojUpdateEnabled() {
			_ = os.WriteFile(outPath, got, 0o644)
		}
		if !spojUpdateEnabled() && len(want) > 0 && !bytes.Equal(got, want) {
			t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
		}
	}
}

func updateSPOJChecklist() {
	root := findRepoRoot2(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "scheme")
	readmePath := filepath.Join(root, "transpiler", "x", "scheme", "SPOJ.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	rows := []string{"| Index | Name | Status | Duration | Memory |", "|------:|------|:-----:|---------:|-------:|"}
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			scanner := bufio.NewScanner(bytes.NewReader(data))
			if scanner.Scan() {
				line := strings.TrimSpace(scanner.Text())
				line = strings.TrimPrefix(line, "#")
				line = strings.TrimSpace(line)
				if strings.HasPrefix(line, "[") {
					if idx := strings.Index(line, "]"); idx >= 0 {
						name = line[1:idx]
					}
				} else if line != "" {
					name = line
				}
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, base+".scm")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
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
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", base, name, status, dur, mem))
	}
	ts := time.Now().In(time.FixedZone("GMT+7", 7*60*60)).Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Scheme SPOJ Transpiler Output\n\n")
	buf.WriteString("Generated Scheme code for SPOJ problems under `tests/spoj/x/mochi`.\n\n")
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString("Last updated: " + ts + "\n\n")
	buf.WriteString(strings.Join(rows, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
