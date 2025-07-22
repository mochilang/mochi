//go:build slow

package ex_test

import (
	"bytes"
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

	"mochi/golden"
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

	golden.RunFirstFailure(t, "tests/rosetta/x/Mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".exs")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := ex.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := ex.Emit(ast)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("elixir", codePath)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(stripWarnings(out))
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		got = normalizeOutput(root, got)
		_ = os.WriteFile(outPath, got, 0o644)
		return got, nil
	})
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
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".exs")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".out")); err2 == nil {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Rosetta Transpiler Progress\n\n")
	buf.WriteString("Generated Elixir code from Mochi Rosetta programs lives in `tests/rosetta/transpiler/Elixir`.\n\n")
	fmt.Fprintf(&buf, "## Rosetta Test Checklist (%d/%d)\n", compiled, total)
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := time.Now()
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	buf.WriteString(fmt.Sprintf("_Last updated: %s_\n", ts.Format("2006-01-02 15:04 -0700")))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
