//go:build slow

package php_test

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
	php "mochi/transpiler/x/php"
	"mochi/types"
)

// TestPHPTranspiler_SPOJ_Golden transpiles Mochi SPOJ solutions to PHP and
// verifies their output against the expected golden files.
func TestPHPTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "php")
	os.MkdirAll(outDir, 0o755)
	defer updateSpojReadme()

	bench := os.Getenv("MOCHI_BENCHMARK") == "1" || strings.ToLower(os.Getenv("MOCHI_BENCHMARK")) == "true"
	php.SetBenchMain(bench)

	idx := 1
	if s := os.Getenv("MOCHI_SPOJ_INDEX"); s != "" {
		if n, err := strconv.Atoi(s); err == nil && n > 0 {
			idx = n
		}
	}

	src := filepath.Join(srcDir, fmt.Sprintf("%d.mochi", idx))
	if err := runSpojPHP(src, outDir, bench); err != nil {
		t.Fatalf("run case %d: %v", idx, err)
	}
}

func runSpojPHP(src string, outDir string, bench bool) error {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".php")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		return fmt.Errorf("parse: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		return fmt.Errorf("type: %w", errs[0])
	}
	ast, err := php.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		return fmt.Errorf("transpile: %w", err)
	}
	var buf bytes.Buffer
	if err := php.Emit(&buf, ast); err != nil {
		_ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
		return fmt.Errorf("emit: %w", err)
	}
	if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
		return fmt.Errorf("write code: %w", err)
	}

	cmd := exec.Command("php", codePath)
	envv := os.Environ()
	if bench {
		envv = append(envv, "MOCHI_BENCHMARK=1")
	} else {
		envv = append(envv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envv
	if data, err := os.ReadFile(filepath.Join(outDir, base+".in")); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	var benchData []byte
	if bench {
		if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
			benchData = bytes.TrimSpace(got[idx:])
			got = bytes.TrimSpace(got[:idx])
		}
	}
	if benchData != nil {
		_ = os.WriteFile(benchPath, benchData, 0o644)
	}
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		return fmt.Errorf("run: %w", err)
	}
	_ = os.Remove(errPath)

	want, err := os.ReadFile(outPath)
	if err != nil || shouldUpdate() {
		if err := os.WriteFile(outPath, append(got, '\n'), 0o644); err != nil {
			return fmt.Errorf("write out: %w", err)
		}
		return nil
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		_ = os.WriteFile(errPath, out, 0o644)
		return fmt.Errorf("output mismatch: got %s want %s", got, want)
	}
	return nil
}

func updateSpojReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "php")
	readmePath := filepath.Join(root, "transpiler", "x", "php", "SPOJ.md")

	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}

	entries, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(entries)
	total := len(entries)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for _, src := range entries {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := parseSpojName(src)
		status := " "
		if exists(filepath.Join(outDir, base+".error")) {
			status = "error"
		} else if exists(filepath.Join(outDir, base+".php")) {
			status = "✓"
			compiled++
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil && len(data) > 0 {
			var js struct {
				Duration int64 `json:"duration_us"`
				Memory   int64 `json:"memory_bytes"`
			}
			if json.Unmarshal(bytes.TrimSpace(data), &js) == nil && js.Duration > 0 {
				dur = humanDuration(js.Duration)
				mem = humanSize(js.Memory)
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}

	var buf bytes.Buffer
	buf.WriteString("# PHP SPOJ Transpiler Output\n\n")
	buf.WriteString("Generated PHP code from Mochi SPOJ tasks lives in `tests/spoj/x/php`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
func parseSpojName(src string) string {
	mdPath := strings.TrimSuffix(src, ".mochi") + ".md"
	data, err := os.ReadFile(mdPath)
	if err != nil {
		return strings.TrimSuffix(filepath.Base(src), ".mochi")
	}
	scanner := bufio.NewScanner(bytes.NewReader(data))
	for scanner.Scan() {
		line := scanner.Text()
		if idx := strings.Index(line, "problems/"); idx >= 0 {
			rest := line[idx+len("problems/"):]
			if j := strings.Index(rest, ")"); j >= 0 {
				name := rest[:j]
				return strings.TrimSuffix(name, "/")
			}
		}
	}
	return strings.TrimSuffix(filepath.Base(src), ".mochi")
}
