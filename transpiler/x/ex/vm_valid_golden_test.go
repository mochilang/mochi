//go:build slow

package ex_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/golden"
	"mochi/parser"
	ex "mochi/transpiler/x/ex"
	"mochi/types"
)

func ensureElixir(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
}

func TestExTranspiler_VMValid_Golden(t *testing.T) {
	ensureElixir(t)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".exs")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

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
		bench := os.Getenv("MOCHI_BENCHMARK") == "true"
		ast, err := ex.Transpile(prog, env)
		if err != nil {
			_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
			return nil, err
		}
		code := ex.Emit(ast, bench)
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		cmd := exec.Command("elixir", codePath)
		cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
		if bench {
			cmd.Env = append(cmd.Env, "MOCHI_BENCHMARK=true")
		}
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		_ = os.Remove(errPath)
		_ = os.WriteFile(outPath, got, 0o644)
		return got, nil
	})
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	updateRosettaReadme()
	os.Exit(code)
}

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

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	readmePath := filepath.Join(root, "transpiler", "x", "ex", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".exs")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".out")); err2 == nil {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Elixir Transpiler\n\n")
	buf.WriteString("This directory contains a minimal transpiler that converts a very small subset of Mochi into Elixir source code. The generated files live in `tests/transpiler/x/ex`.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
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

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "ex", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI%n%h%n%s").Output()
	ts := ""
	hash := ""
	msg := ""
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), "\n", 3)
		if len(parts) == 3 {
			if t, perr := time.Parse(time.RFC3339, parts[0]); perr == nil {
				if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
					ts = t.In(loc).Format("2006-01-02 15:04 -0700")
				} else {
					ts = t.Format("2006-01-02 15:04 -0700")
				}
			}
			hash = parts[1]
			msg = parts[2]
		}
	}
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".exs")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".out")); err2 == nil {
				compiled++
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	fmt.Fprintf(&buf, "- Commit %s: %s\n", hash, msg)
	fmt.Fprintf(&buf, "- Generated Elixir for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n")
	buf.WriteString("- Improved code emission and type inference\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		sections := strings.Split(string(data), "\n## ")
		count := 0
		for _, sec := range sections {
			if strings.HasPrefix(sec, "Progress") {
				if count >= 4 {
					break
				}
				buf.WriteString("## " + sec)
				if !strings.HasSuffix(sec, "\n") {
					buf.WriteString("\n")
				}
				count++
			}
		}
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
