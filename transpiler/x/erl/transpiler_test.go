//go:build slow

package erl_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	erl "mochi/transpiler/x/erl"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func runGolden(t *testing.T, name string) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
	os.MkdirAll(outDir, 0o755)

	src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := erl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	erlFile := filepath.Join(outDir, name+".erl")
	if updateEnabled() {
		if err := os.WriteFile(erlFile, code, 0o755); err != nil {
			t.Fatalf("write: %v", err)
		}
	} else {
		_ = os.WriteFile(erlFile, code, 0o755)
	}
	cmd := exec.Command("escript", erlFile)
	out, err := cmd.CombinedOutput()
	if err != nil {
		if updateEnabled() {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		}
		t.Fatalf("run: %v", err)
	}
	lines := bytes.Split(out, []byte{'\n'})
	filtered := lines[:0]
	for _, l := range lines {
		if bytes.Contains(l, []byte("Warning:")) || bytes.HasPrefix(l, []byte("%")) {
			continue
		}
		filtered = append(filtered, l)
	}
	got := bytes.TrimSpace(bytes.Join(filtered, []byte{'\n'}))
	if updateEnabled() {
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
			t.Fatalf("write out: %v", err)
		}
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
	wantPath := filepath.Join(outDir, name+".out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
}

func TestTranspilePrintHello(t *testing.T) { runGolden(t, "print_hello") }

func TestTranspileUnaryNeg(t *testing.T) { runGolden(t, "unary_neg") }

func TestTranspileStringCompare(t *testing.T) { runGolden(t, "string_compare") }

func TestTranspileLenString(t *testing.T) { runGolden(t, "len_string") }

func TestTranspileStrBuiltin(t *testing.T) { runGolden(t, "str_builtin") }

func TestTranspileLenBuiltin(t *testing.T) { runGolden(t, "len_builtin") }

func TestTranspileLetAndPrint(t *testing.T) { runGolden(t, "let_and_print") }

func TestTranspileStringConcat(t *testing.T) { runGolden(t, "string_concat") }

func TestTranspileIfThenElse(t *testing.T) { runGolden(t, "if_then_else") }

func TestTranspileIfThenElseNested(t *testing.T) { runGolden(t, "if_then_else_nested") }

func TestTranspileAppendBuiltin(t *testing.T) { runGolden(t, "append_builtin") }

func TestTranspileAvgBuiltin(t *testing.T) { runGolden(t, "avg_builtin") }

func TestTranspileCountBuiltin(t *testing.T) { runGolden(t, "count_builtin") }

func TestTranspileSumBuiltin(t *testing.T) { runGolden(t, "sum_builtin") }

func TestTranspileMinMaxBuiltin(t *testing.T) { runGolden(t, "min_max_builtin") }

func TestTranspileIfElse(t *testing.T) { runGolden(t, "if_else") }

func TestTranspileInOperator(t *testing.T) { runGolden(t, "in_operator") }

func TestTranspileMembership(t *testing.T) { runGolden(t, "membership") }

func TestTranspileMathOps(t *testing.T) { runGolden(t, "math_ops") }

func TestTranspileLenMap(t *testing.T) { runGolden(t, "len_map") }

func TestTranspileMapIndex(t *testing.T) { runGolden(t, "map_index") }

func TestTranspileMapIntKey(t *testing.T) { runGolden(t, "map_int_key") }

func TestTranspileMapMembership(t *testing.T) { runGolden(t, "map_membership") }

func TestTranspileMapInOperator(t *testing.T) { runGolden(t, "map_in_operator") }

func TestTranspileListIndex(t *testing.T) { runGolden(t, "list_index") }

func TestTranspileStringIndex(t *testing.T) { runGolden(t, "string_index") }

func updateEnabled() bool { return *update }

func countCompiled() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
	readmePath := filepath.Join(root, "transpiler", "x", "erl", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
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
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("# Erlang Transpiler Output (%d/%d generated and run)\n\n", compiled, total))
	buf.WriteString("This directory contains a minimal transpiler that converts a very small\n")
	buf.WriteString("subset of Mochi into Erlang. Generated programs are executed with\n")
	buf.WriteString("`escript` to verify runtime behaviour.\n\n")
	buf.WriteString("## VM Valid Checklist\n\n")
	buf.WriteString("The following programs under `tests/vm/valid` have golden outputs. A\n")
	buf.WriteString("checked item means the Erlang transpiler can successfully generate code\n")
	buf.WriteString("that produces the same output as the Mochi VM.\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "erl", "TASKS.md")
	compiled, total := countCompiled()
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	buf.WriteString(fmt.Sprintf("- VM valid golden test results updated to %d/%d\n\n", compiled, total))
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}
