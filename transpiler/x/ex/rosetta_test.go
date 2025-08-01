//go:build slow

package ex_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
	"mochi/types"
)

func TestExTranspiler_Rosetta_Golden(t *testing.T) {
	ensureElixir(t)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Elixir")
	os.MkdirAll(outDir, 0o755)

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		pattern := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi")
		files, err := filepath.Glob(pattern)
		if err != nil {
			t.Fatalf("glob: %v", err)
		}
		sort.Strings(files)
		if idx < 1 || idx > len(files) {
			t.Fatalf("MOCHI_ROSETTA_INDEX out of range: %d", idx)
		}
		name := strings.TrimSuffix(filepath.Base(files[idx-1]), ".mochi")
		os.Setenv("MOCHI_ROSETTA_ONLY", name)
	}

	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	pattern := filepath.Join(srcDir, "*.mochi")
	if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		pattern = filepath.Join(srcDir, only+".mochi")
	}
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no test files found: %s", pattern)
	}
	sort.Strings(files)

	update := false
	if f := flag.Lookup("update"); f != nil && f.Value.String() == "true" {
		update = true
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".exs")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		ok := t.Run(name, func(t *testing.T) {
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
			bench := os.Getenv("MOCHI_BENCHMARK") == "true"
			ast, err := ex.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ex.Emit(ast, bench)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			cmd := exec.Command("elixir", codePath)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			if bench {
				cmd.Env = append(os.Environ(), "MOCHI_BENCHMARK=true")
			}
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(stripWarnings(out))
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			got = normalizeOutput(root, got)
			_ = os.WriteFile(outPath, got, 0o644)

			if bench {
				return
			}

			if update {
				if err := os.WriteFile(outPath, got, 0o644); err != nil {
					t.Fatalf("update: %v", err)
				}
				return
			}

			want, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = normalizeOutput(root, want)
			if !bytes.Equal(got, want) {
				t.Fatalf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+".out", got, want)
			}
		})

		if !ok {
			t.Fatalf("first failing program: %s", name)
		}
	}
}

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	tsRE := regexp.MustCompile(`\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z`)
	out = tsRE.ReplaceAllString(out, "2006-01-02T15:04:05Z")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func stripWarnings(b []byte) []byte {
	lines := bytes.Split(b, []byte("\n"))
	var res [][]byte
	skipping := false
	for _, l := range lines {
		trimmed := bytes.TrimSpace(l)
		if bytes.HasPrefix(trimmed, []byte("warning:")) {
			skipping = true
			continue
		}
		if skipping {
			if len(trimmed) == 0 {
				skipping = false
			}
			continue
		}
		res = append(res, l)
	}
	return bytes.Join(res, []byte("\n"))
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Elixir")
	readmePath := filepath.Join(root, "transpiler", "x", "ex", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 -0700")
		}
	}
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "| ---: | --- | :---: | ---: | ---: |")
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		status := ""
		dur := ""
		mem := ""
		outPath := filepath.Join(outDir, name+".out")
		if data, err := os.ReadFile(outPath); err == nil {
			status = "✓"
			compiled++
			var res struct {
				Dur int64 `json:"duration_us"`
				Mem int64 `json:"memory_bytes"`
			}
			trimmed := bytes.TrimSpace(data)
			if idx := bytes.LastIndex(trimmed, []byte("{")); idx >= 0 {
				trimmed = trimmed[idx:]
				if json.Unmarshal(trimmed, &res) == nil && res.Dur > 0 {
					dur = humanDur(time.Duration(res.Dur) * time.Microsecond)
					mem = humanSize(res.Mem)
				}
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# Rosetta Transpiler Progress\n\n")
	buf.WriteString("Generated Elixir code from Mochi Rosetta programs lives in `tests/rosetta/transpiler/Elixir`.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Test Checklist (%d/%d)\n", compiled, total)
	if ts != "" {
		buf.WriteString(fmt.Sprintf("_Last updated: %s_\n", ts))
	}
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func humanDur(d time.Duration) string {
	if d < time.Millisecond {
		return fmt.Sprintf("%dus", d.Microseconds())
	}
	if d < time.Second {
		return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000)
	}
	if d < time.Minute {
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
	return d.String()
}

func humanSize(n int64) string {
	const unit = 1024
	units := []string{"B", "KB", "MB", "GB", "TB"}
	if n < unit {
		return fmt.Sprintf("%d B", n)
	}
	val := float64(n)
	exp := 0
	for val >= unit && exp < len(units)-1 {
		val /= unit
		exp++
	}
	return fmt.Sprintf("%.1f %s", val, units[exp])
}
