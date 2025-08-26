//go:build slow

package rs_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	rs "mochi/transpiler/x/rs"
	"mochi/types"
)

func TestRustTranspiler_SPOJ_Golden(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "rs")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateSpojReadme)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})
	if len(files) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}

	from, to := 1, math.MaxInt32
	if s := os.Getenv("MOCHI_SPOJ_FROM_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil {
			from = v
		}
	}
	if s := os.Getenv("MOCHI_SPOJ_TO_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil {
			to = v
		}
	}
	var filtered []string
	for _, f := range files {
		idx, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(f), ".mochi"))
		if idx >= from && idx <= to {
			filtered = append(filtered, f)
		}
	}
	files = filtered
	if len(files) == 0 {
		t.Fatalf("no files in range")
	}

	var passed, failed int
	var firstFail string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		idx, _ := strconv.Atoi(base)
		ok := t.Run(fmt.Sprintf("%03d", idx), func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".rs")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")
			benchPath := filepath.Join(outDir, base+".bench")
			inSrc := filepath.Join(srcDir, base+".in")
			inPath := filepath.Join(outDir, base+".in")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
				t.Logf("parse: %v", err)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
				t.Logf("type: %v", errs[0])
				return
			}
			bench := true
			gprog, err := rs.Transpile(prog, env, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Logf("transpile: %v", err)
				return
			}
			code := rs.Emit(gprog)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Logf("write code: %v", err)
				return
			}

			tmpDir := t.TempDir()
			bin := filepath.Join(tmpDir, base)
			crate := strings.Map(func(r rune) rune {
				if r == '_' || r == '-' || ('0' <= r && r <= '9') || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') {
					return r
				}
				return '_'
			}, base)
			if out, err := exec.Command("rustc", codePath, "-O", "--crate-name", crate, "-o", bin).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, append([]byte("compile: "+err.Error()+"\n"), out...), 0o644)
				t.Logf("compile: %v", err)
				return
			}

			cmd := exec.Command(bin)
			cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=1", "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(inSrc); err == nil {
				cmd.Stdin = bytes.NewReader(data)
				_ = os.WriteFile(inPath, data, 0o644)
			}
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Logf("run: %v", err)
				return
			}
			_ = os.Remove(errPath)
			benchData := got
			var gotOut []byte
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				gotOut = bytes.TrimSpace(got[:idx])
				benchData = got[idx:]
			} else {
				gotOut = got
				benchData = nil
			}
			if err := os.WriteFile(outPath, gotOut, 0o644); err != nil {
				t.Logf("write out: %v", err)
				return
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
			if len(want) > 0 && !bytes.Equal(gotOut, want) {
				t.Logf("output mismatch\nGot: %s\nWant: %s", gotOut, want)
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = base
			}
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Logf("first failing program: %s", firstFail)
	}
}

func updateSpojReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "rs")
	readmePath := filepath.Join(root, "transpiler", "x", "rs", "SPOJ.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})
	total := len(files)
	compiled := 0
	var rows []string
	rows = append(rows, "| Index | Name | Status | Duration | Memory |")
	rows = append(rows, "|------:|------|:------:|---------:|-------:|")
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		idx, _ := strconv.Atoi(base)
		name := base
		if data, err := os.ReadFile(filepath.Join(srcDir, base+".md")); err == nil {
			lines := strings.Split(string(data), "\n")
			if len(lines) > 0 {
				line := strings.TrimSpace(lines[0])
				if strings.HasPrefix(line, "# [") {
					if end := strings.Index(line[3:], "]"); end >= 0 {
						name = line[3 : 3+end]
					}
				}
			}
		}
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			// keep failure
		} else if _, err := os.Stat(filepath.Join(outDir, base+".rs")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
			}
			if json.Unmarshal(trimmed, &r) == nil {
				dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
				mem = formatBytes(r.MemoryBytes)
			}
		}
		rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", idx, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Rust code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/rs`.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	buf.WriteString(fmt.Sprintf("## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total))
	for _, r := range rows {
		buf.WriteString(r + "\n")
	}
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
