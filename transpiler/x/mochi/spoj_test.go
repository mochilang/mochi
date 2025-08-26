//go:build slow

package mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestMochiTranspiler_SPOJ_Golden(t *testing.T) {
	root := repoRoot(t)
	t.Cleanup(updateSPOJ)

	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	os.MkdirAll(outDir, 0o755)

	idx := 14
	if s := os.Getenv("SPOJ_INDEX"); s != "" {
		if v, err := strconv.Atoi(s); err == nil && v > 0 {
			idx = v
		}
	}
	base := fmt.Sprintf("%d", idx)
	srcPath := filepath.Join(srcDir, base+".mochi")
	codePath := filepath.Join(outDir, base+".mochi")
	inPath := filepath.Join(outDir, base+".in")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")

	bench := os.Getenv("MOCHI_BENCHMARK") != ""

	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	if !bench {
		want = bytes.TrimSpace(want)
	}

	code, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read src: %v", err)
	}
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	prog, err := parser.Parse(srcPath)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	var buf bytes.Buffer
	env.SetWriter(&buf)
	if data, err := os.ReadFile(inPath); err == nil {
		env.SetReader(bytes.NewReader(data))
	}
	if !bench {
		_ = os.Setenv("MOCHI_NOW_SEED", "1")
	}
	intr := interpreter.New(prog, env, root)
	if err := intr.Run(); err != nil {
		_ = os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(buf.Bytes())

	if bench {
		_ = os.WriteFile(benchPath, got, 0o644)
		return
	}

	if !bytes.Equal(got, want) {
		_ = os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\nGot: %s\nWant: %s", got, want)), 0o644)
		t.Fatalf("output mismatch")
	}
	_ = os.Remove(errPath)
}

func updateSPOJ() {
	root := repoRoot(&testing.T{})
	outDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	md := filepath.Join(root, "transpiler", "x", "mochi", "SPOJ.md")

	status := " "
	dur := ""
	mem := ""
	if _, err := os.Stat(filepath.Join(outDir, "14.error")); err == nil {
		status = "error"
	} else if _, err := os.Stat(filepath.Join(outDir, "14.mochi")); err == nil {
		status = "✓"
	}
	if data, err := os.ReadFile(filepath.Join(outDir, "14.bench")); err == nil {
		var r struct {
			DurationUS  int64 `json:"duration_us"`
			MemoryBytes int64 `json:"memory_bytes"`
		}
		if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
			dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
			mem = formatBytes(r.MemoryBytes)
		}
	}
	name := "Problem 14"
	if data, err := os.ReadFile(filepath.Join(outDir, "14.md")); err == nil {
		if line, _, ok := bytes.Cut(data, []byte("\n")); ok || len(line) > 0 {
			line = bytes.TrimSpace(line)
			if i := bytes.IndexByte(line, '['); i >= 0 {
				if j := bytes.IndexByte(line[i+1:], ']'); j >= 0 {
					name = string(line[i+1 : i+1+j])
				}
			} else if len(line) > 0 && line[0] == '#' {
				name = strings.TrimSpace(string(line[1:]))
			}
		}
	}
	compiled := 0
	if status == "✓" {
		compiled = 1
	}
	loc := time.FixedZone("GMT+7", 7*60*60)
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Mochi code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/mochi`.\n")
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	buf.WriteString("## SPOJ Golden Test Checklist (" + strconv.Itoa(compiled) + "/1)\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	buf.WriteString(fmt.Sprintf("| 14 | %s | %s | %s | %s |\n", name, status, dur, mem))
	_ = os.MkdirAll(filepath.Dir(md), 0o755)
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
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
