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
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	php "mochi/transpiler/x/php"
	"mochi/types"
)

func TestPHPTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "php")
	os.MkdirAll(outDir, 0o755)
	defer updateRosettaReadme()

	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	php.SetBenchMain(bench)

	indexPath := filepath.Join(srcDir, "index.txt")
	f, err := os.Open(indexPath)
	if err != nil {
		t.Fatalf("open index: %v", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var files []string
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			files = append(files, filepath.Join(srcDir, parts[1]))
		}
	}
	if err := scanner.Err(); err != nil {
		t.Fatalf("scan index: %v", err)
	}

	if len(files) == 0 {
		t.Fatal("no rosetta programs found")
	}

	if s := os.Getenv("MOCHI_ROSETTA_INDEX"); s != "" {
		idx, err := strconv.Atoi(s)
		if err != nil || idx <= 0 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", s)
		}
		files = files[idx-1 : idx]
	} else if s := os.Getenv("ROSETTA_LIMIT"); s != "" {
		if n, err := strconv.Atoi(s); err == nil && n < len(files) {
			files = files[:n]
		}
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if err := runRosettaPHP(src, outDir, bench); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
	}
}

func runRosettaPHP(src string, outDir string, bench bool) error {
	name := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, name+".php")
	outPath := filepath.Join(outDir, name+".out")
	errPath := filepath.Join(outDir, name+".error")

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
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	want, _ := os.ReadFile(outPath)
	want = bytes.TrimSpace(want)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if bench {
		benchPath := filepath.Join(outDir, name+".bench")
		if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
			_ = os.WriteFile(benchPath, got[idx:], 0o644)
		} else {
			_ = os.WriteFile(benchPath, nil, 0o644)
		}
	}
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		return fmt.Errorf("run: %w", err)
	}
	_ = os.Remove(errPath)
	if shouldUpdate() || len(want) == 0 {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return nil
	}
	if !bytes.Equal(got, want) {
		_ = os.WriteFile(errPath, out, 0o644)
		return fmt.Errorf("output mismatch: got %s want %s", got, want)
	}
	_ = os.WriteFile(outPath, got, 0o644)
	return nil
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "php")
	readmePath := filepath.Join(root, "transpiler", "x", "php", "ROSETTA.md")

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

	indexPath := filepath.Join(srcDir, "index.txt")
	idxFile, err := os.Open(indexPath)
	if err != nil {
		return
	}
	defer idxFile.Close()
	scanner := bufio.NewScanner(idxFile)
	var files []string
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			files = append(files, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return
	}
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range files {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		phpFile := filepath.Join(outDir, name+".php")
		outFile := filepath.Join(outDir, name+".out")
		errFile := filepath.Join(outDir, name+".error")
		if exists(phpFile) && exists(outFile) && !exists(errFile) {
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

	var buf bytes.Buffer
	buf.WriteString("# PHP Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated PHP code from Mochi Rosetta tasks lives in `tests/rosetta/transpiler/php`.\n\n")
	if ts != "" {
		fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	}
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func shouldUpdate() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
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
