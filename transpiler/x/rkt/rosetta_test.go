//go:build slow

package rkt_test

import (
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
	rkt "mochi/transpiler/x/rkt"
	"mochi/types"
)

func TestRacketTranspiler_Rosetta(t *testing.T) {
	t.Cleanup(updateRosettaChecklist)
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}

	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Racket")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaChecklist)

	programs := listRosettaPrograms(t)
	if len(programs) == 0 {
		t.Fatalf("no rosetta programs found")
	}

	index := 1
	if s := os.Getenv("ROSETTA_INDEX"); s != "" {
		if n, err := strconv.Atoi(s); err == nil {
			index = n
		}
	}
	if index < 1 || index > len(programs) {
		t.Fatalf("index %d out of range (1-%d)", index, len(programs))
	}
	name := programs[index-1]
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	srcPath := filepath.Join(srcDir, name+".mochi")
	outPath := filepath.Join(outDir, name+".out")
	if _, err := os.Stat(srcPath); err != nil {
		t.Fatalf("missing source for %s", name)
	}
	if err := transpileAndRunRacket(root, srcPath, outPath, outDir, name); err != nil {
		t.Fatalf("%s: %v", name, err)
	}
}

func transpileAndRunRacket(root, srcPath, wantPath, outDir, name string) error {
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeRacketError(outDir, name, fmt.Errorf("parse error: %w", err))
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeRacketError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		return errs[0]
	}
	bench := os.Getenv("MOCHI_BENCHMARK") != ""
	ast, err := rkt.Transpile(prog, env, bench)
	if err != nil {
		writeRacketError(outDir, name, fmt.Errorf("transpile error: %w", err))
		return err
	}
	var buf bytes.Buffer
	if err := rkt.Emit(&buf, ast); err != nil {
		writeRacketError(outDir, name, fmt.Errorf("emit error: %w", err))
		return err
	}
	rktFile := filepath.Join(outDir, name+".rkt")
	if err := os.WriteFile(rktFile, buf.Bytes(), 0o644); err != nil {
		return fmt.Errorf("write rkt: %w", err)
	}
	cmd := exec.Command("racket", rktFile)
	envs := append(os.Environ(), "MOCHI_ROOT="+root)
	if bench {
		envs = append(envs, "MOCHI_BENCHMARK=1")
	} else {
		envs = append(envs, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envs
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		writeRacketError(outDir, name, fmt.Errorf("run error: %v\n%s", err, out))
		return err
	}
	if bench {
		if err := os.WriteFile(filepath.Join(outDir, name+".bench"), got, 0o644); err != nil {
			return fmt.Errorf("write out: %w", err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return nil
	}
	want, _ := os.ReadFile(wantPath)
	want = bytes.TrimSpace(want)
	if len(want) > 0 && !bytes.Equal(got, want) {
		writeRacketError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return fmt.Errorf("output mismatch")
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		return fmt.Errorf("write out: %w", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	return nil
}

func writeRacketError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func listRosettaPrograms(t *testing.T) []string {
	t.Helper()
	root := repoRoot(t)
	idxPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "index.txt")
	data, err := os.ReadFile(idxPath)
	if err == nil {
		lines := strings.Split(strings.TrimSpace(string(data)), "\n")
		programs := make([]string, 0, len(lines))
		for _, line := range lines {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				name := strings.TrimSuffix(fields[1], ".mochi")
				programs = append(programs, name)
			}
		}
		if len(programs) > 0 {
			return programs
		}
	}
	pattern := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	programs := make([]string, len(files))
	for i, f := range files {
		programs[i] = strings.TrimSuffix(filepath.Base(f), ".mochi")
	}
	return programs
}

func updateRosettaChecklist() {
	root := repoRoot(&testing.T{})
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Racket")
	readmePath := filepath.Join(root, "transpiler", "x", "rkt", "ROSETTA.md")

	programs := listRosettaPrograms(&testing.T{})
	total := len(programs)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, name := range programs {
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".rkt")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
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
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	if loc, err := time.LoadLocation("Asia/Bangkok"); err == nil {
		ts = ts.In(loc)
	}
	var buf bytes.Buffer
	buf.WriteString("# Racket Rosetta Transpiler Output\n\n")
	buf.WriteString("This directory holds Racket source code generated by the Mochi transpiler from the programs in `tests/rosetta/x/Mochi`. Each file has the expected runtime output in a matching `.out` file. Compilation or runtime failures are stored in a corresponding `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.Format("2006-01-02 15:04 -0700"))
	fmt.Fprintf(&buf, "## Rosetta Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
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
