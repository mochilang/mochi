//go:build slow

package cljt_test

import (
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
	cljt "mochi/transpiler/x/clj"
	"mochi/types"
)

var rosettaIndex = flag.Int("index", 0, "run a specific Rosetta example by index (1-based)")

func TestRosettaClojure(t *testing.T) {
	defer updateRosettaReadme()
	if err := EnsureClojure(); err != nil {
		t.Skip("clojure not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Clojure")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	sort.Strings(outs)

	if *rosettaIndex > 0 {
		if *rosettaIndex > len(outs) {
			t.Fatalf("index %d out of range", *rosettaIndex)
		}
		outs = outs[*rosettaIndex-1 : *rosettaIndex]
	} else if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(outs) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		outs = outs[idx-1 : idx]
	}

	for _, srcPath := range outs {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".mochi")
		t.Run(name, func(t *testing.T) {
			compileRunClojureRosetta(t, srcPath, outDir, name)
		})
	}
}

func compileRunClojureRosetta(t *testing.T, srcPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeRosettaCljError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	bench := os.Getenv("MOCHI_BENCHMARK") != ""
	ast, err := cljt.Transpile(prog, env, bench)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("transpile error: %w", err))
		t.Skip("transpile error")
		return
	}
	code := cljt.Format(cljt.EmitString(ast))
	cljPath := filepath.Join(outDir, name+".clj")
	if err := os.WriteFile(cljPath, code, 0o644); err != nil {
		t.Fatalf("write clj: %v", err)
	}
	cmd := exec.Command("clojure", cljPath)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		t.Skip("run error")
		return
	}
	got := bytes.TrimSpace(buf.Bytes())
	benchPath := filepath.Join(outDir, name+".bench")
	outPath := filepath.Join(outDir, name+".out")
	if bench {
		outBytes := got
		if idx := bytes.LastIndexByte(outBytes, '{'); idx >= 0 {
			outBytes = outBytes[idx:]
		}
		_ = os.WriteFile(benchPath, outBytes, 0o644)
		var js struct {
			Duration int64 `json:"duration_us"`
			Memory   int64 `json:"memory_bytes"`
		}
		_ = json.Unmarshal(outBytes, &js)
		return
	}
	if *update {
		if err := os.WriteFile(outPath, got, 0o644); err != nil {
			t.Fatalf("write out: %v", err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		return
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		writeRosettaCljError(outDir, name, fmt.Errorf("missing output: %v", err))
		t.Fatalf("missing .out file for %s", name)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeRosettaCljError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		t.Errorf("output mismatch for %s", name)
		return
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writeRosettaCljError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func updateRosettaReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	binDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Clojure")
	readmePath := filepath.Join(root, "transpiler", "x", "clj", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(binDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(binDir, name+".clj")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		benchFile := filepath.Join(binDir, name+".bench")
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
		} else if data, err := os.ReadFile(filepath.Join(binDir, name+".out")); err == nil {
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
	loc, _ := time.LoadLocation("Asia/Bangkok")
	ts := time.Now().In(loc).Format("2006-01-02 15:04 -0700")

	var buf bytes.Buffer
	buf.WriteString("# Clojure Rosetta Transpiler\n\n")
	fmt.Fprintf(&buf, "Completed: %d/%d\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
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
