//go:build slow

package rkt_test

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

	"mochi/parser"
	rkt "mochi/transpiler/x/rkt"
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func TestTranspile_Golden(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rkt")
	os.MkdirAll(outDir, 0o755)
	names := []string{
		"print_hello",
		"string_concat",
		"let_and_print",
		"var_assignment",
		"binary_precedence",
		"basic_compare",
		"math_ops",
		"string_compare",
		"if_then_else_nested",
		"if_else",
		"if_then_else",
		"while_loop",
		"unary_neg",
		"len_string",
		"len_builtin",
		"avg_builtin",
		"append_builtin",
		"count_builtin",
		"sum_builtin",
		"str_builtin",
		"bool_chain",
		"cast_string_to_int",
		"string_contains",
		"string_index",
		"list_index",
		"list_assign",
	}
	for _, name := range names {
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", name, errs[0])
		}
		ast, err := rkt.Transpile(prog, env)
		if err != nil {
			t.Fatalf("transpile %s: %v", name, err)
		}
		var buf bytes.Buffer
		if err := rkt.Emit(&buf, ast); err != nil {
			t.Fatalf("emit %s: %v", name, err)
		}
		rktFile := filepath.Join(outDir, name+".rkt")
		if err := os.WriteFile(rktFile, buf.Bytes(), 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
		cmd := exec.Command("racket", rktFile)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		out, err := cmd.CombinedOutput()
		trimmed := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("run %s: %v", name, err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
		if err != nil {
			t.Fatalf("read want %s: %v", name, err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(trimmed, want) {
			t.Errorf("%s output mismatch:\nGot: %s\nWant: %s", name, trimmed, want)
		}
	}
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := srcDir
	readmePath := filepath.Join(root, "transpiler", "x", "rkt", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".rkt.out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	var buf bytes.Buffer
	buf.WriteString("# Mochi Racket Transpiler\n")
	buf.WriteString("This directory contains the experimental Racket transpiler. Golden tests under `tests/vm/valid` check the generated code and its runtime output.\n")
	fmt.Fprintf(&buf, "\n## Golden Test Checklist (%d/%d)\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskPath := filepath.Join(root, "transpiler", "x", "rkt", "TASKS.md")
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
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := srcDir
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	if len(files) > 100 {
		files = files[:100]
	}
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".rkt.out")); err == nil {
			compiled++
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	fmt.Fprintf(&buf, "- Generated Racket for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist\n\n")
	if data, err := os.ReadFile(taskPath); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskPath, buf.Bytes(), 0o644)
}
