//go:build slow

package hs_test

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
	hs "mochi/transpiler/x/hs"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
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

func writeErr(root, name string, err error) {
	path := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell", name+".error")
	_ = os.WriteFile(path, []byte(err.Error()), 0o644)
}

func removeErr(root, name string) {
	os.Remove(filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell", name+".error"))
}

func runRosettaTask(root, name string) error {
	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(root, name, fmt.Errorf("parse: %w", err))
		return fmt.Errorf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(root, name, fmt.Errorf("type: %v", errs[0]))
		return fmt.Errorf("type error: %v", errs[0])
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	hprog, err := hs.Transpile(prog, env, bench)
	if err != nil {
		writeErr(root, name, fmt.Errorf("transpile: %w", err))
		return fmt.Errorf("transpile error: %v", err)
	}
	// Emit without an additional bench wrapper since Transpile already
	// wrapped the main function when benchmarking is enabled.
	code := hs.Emit(hprog, false)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell")
	os.MkdirAll(outDir, 0o755)
	hsFile := filepath.Join(outDir, name+".hs")
	if err := os.WriteFile(hsFile, code, 0o644); err != nil {
		return fmt.Errorf("write hs: %v", err)
	}

	cmd := exec.Command("runhaskell", hsFile)
	runEnv := os.Environ()
	if bench {
		runEnv = append(runEnv, "MOCHI_BENCHMARK=1")
		runEnv = append(runEnv, "GHCRTS=-T")
	} else {
		runEnv = append(runEnv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = runEnv
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	outBytes, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(outBytes)
	if err != nil {
		writeErr(root, name, fmt.Errorf("run: %v\n%s", err, outBytes))
		return fmt.Errorf("run error: %v", err)
	}
	removeErr(root, name)
	if bench {
		benchPath := filepath.Join(outDir, name+".bench")
		outBytes := got
		if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
			outBytes = outBytes[idx:]
		}
		_ = os.WriteFile(benchPath, outBytes, 0o644)
		return nil
	}
	outPath := filepath.Join(outDir, name+".out")
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return nil
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		return fmt.Errorf("read want: %v", err)
	}
	if !bytes.Equal(got, bytes.TrimSpace(want)) {
		return fmt.Errorf("output mismatch for %s.out", name)
	}
	return nil
}

func TestHSTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("runhaskell"); err != nil {
		t.Skip("runhaskell not installed")
	}
	t.Cleanup(updateChecklist)
	root := repoRootDir(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no entries in index")
	}
	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(srcDir, n)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX %s", idxStr)
		}
		files = files[idx-1 : idx]
	} else if idxStr := os.Getenv("ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid ROSETTA_INDEX %s", idxStr)
		}
		files = files[idx-1 : idx]
	} else {
		max := 3
		if v := os.Getenv("ROSETTA_MAX"); v != "" {
			if n, err := strconv.Atoi(v); err == nil && n < len(files) {
				max = n
			} else if err == nil {
				max = n
			}
		}
		if len(files) < max {
			max = len(files)
		}
		files = files[:max]
	}

	var firstErr string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		testName := fmt.Sprintf("%03d_%s", i+1, name)
		ok := t.Run(testName, func(t *testing.T) {
			if err := runRosettaTask(root, name); err != nil {
				firstErr = name
				t.Fatalf("%v", err)
			}
		})
		if !ok {
			break
		}
	}
	if firstErr != "" {
		t.Fatalf("first failing program: %s", firstErr)
	}
}

func updateChecklist() {
	root := repoRootDir(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Haskell")
	readme := filepath.Join(root, "transpiler", "x", "hs", "ROSETTA.md")
	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, n := range names {
		name := strings.TrimSuffix(filepath.Base(n), ".mochi")
		status := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
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
				if json.Unmarshal(data[idx:], &js) == nil {
					if js.Duration > 0 {
						dur = humanDuration(js.Duration)
					}
					if js.Memory > 0 {
						mem = humanSize(js.Memory)
					}
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Rosetta Haskell Transpiler (%d/%d succeeded)\n\n", compiled, total)
	buf.WriteString("Generated Haskell code for Rosetta Mochi programs. Each `.hs` file is in `tests/rosetta/transpiler/Haskell` with matching `.out` output. Failures produce a `.error` file.\n\n")
	out, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc := time.FixedZone("GMT+7", 7*3600)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts.In(loc).Format("2006-01-02 15:04 MST"))
	buf.WriteString("## Checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readme, buf.Bytes(), 0o644)
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
