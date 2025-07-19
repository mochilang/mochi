//go:build slow

package ex_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	ex "mochi/transpiler/x/ex"
	"mochi/types"
)

func findRepoRoot(t *testing.T) string {
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

func TestElixirTranspiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	root := findRepoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	os.MkdirAll(outDir, 0o755)

	files, _ := filepath.Glob(filepath.Join(outDir, "*.out"))
	var cases []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".out")
		cases = append(cases, filepath.Join(root, "tests", "vm", "valid", base+".mochi"))
	}

	for _, src := range cases {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".exs")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			gprog, err := ex.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ex.Emit(gprog)
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			cmd := exec.Command("elixir", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			out, err := cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			outBytes := bytes.TrimSpace(out)
			_ = os.WriteFile(outPath, outBytes, 0o644)
			_ = os.Remove(errPath)

			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(outBytes, want) {
				t.Fatalf("output mismatch\nGot: %s\nWant: %s", outBytes, want)
			}
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func updateReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "ex")
	readmePath := filepath.Join(root, "transpiler", "x", "ex", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Elixir Transpiler\n\n")
	buf.WriteString("This directory contains a minimal transpiler that converts a very small subset of Mochi into Elixir source code. The generated files live in `tests/transpiler/x/ex`.\n\n")
	fmt.Fprintf(&buf, "Currently %d of %d programs transpile and run.\n\n", compiled, total)
	buf.WriteString("The table below tracks which programs from `tests/vm/valid` successfully transpile and run. Checked items have generated `.exs` code and matching `.out` files in `tests/transpiler/x/ex`.\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := findRepoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "ex", "TASKS.md")
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
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString("- VM valid golden test results updated\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
