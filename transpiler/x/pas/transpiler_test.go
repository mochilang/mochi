//go:build slow

package pas_test

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
	pas "mochi/transpiler/x/pas"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
	t.Helper()
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

func ensureFPCQuick(t *testing.T) string {
	t.Helper()
	path, err := exec.LookPath("fpc")
	if err != nil {
		t.Skip("fpc not installed")
	}
	return path
}

func runCase(t *testing.T, name string) {
	t.Helper()
	fpc := ensureFPCQuick(t)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
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
	ast, err := pas.Transpile(env, prog)
	if err != nil {
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	pasPath := filepath.Join(outDir, name+".pas")
	if err := os.WriteFile(pasPath, code, 0o644); err != nil {
		t.Fatal(err)
	}
	exe := filepath.Join(outDir, name)
	cmd := exec.Command(fpc, pasPath, "-o"+exe)
	if out, err := cmd.CombinedOutput(); err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("compile: %v", err)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
		t.Fatalf("run: %v", err)
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("%s output mismatch: got %s want %s", name, got, want)
	}
}

func TestPascalTranspiler(t *testing.T) {
	for _, tc := range []string{"print_hello", "unary_neg", "math_ops", "let_and_print", "var_assignment", "typed_let", "typed_var", "string_concat", "string_compare", "while_loop", "if_else", "fun_call", "fun_three_args", "bool_chain", "basic_compare", "binary_precedence", "cast_string_to_int", "len_string", "string_contains", "substring_builtin", "short_circuit", "len_builtin", "list_index", "membership", "count_builtin", "avg_builtin", "min_max_builtin", "slice", "list_nested_assign", "append_builtin", "for_loop", "for_list_collection", "break_continue", "in_operator", "cross_join", "cross_join_filter", "cross_join_triple", "left_join", "group_by"} {
		t.Run(tc, func(t *testing.T) { runCase(t, tc) })
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func countCompiled() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".pas")); err == nil {
			compiled++
		}
	}
	return compiled, total
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "pas")
	readmePath := filepath.Join(root, "transpiler", "x", "pas", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".pas")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# Pascal Transpiler\n\n")
	buf.WriteString("This folder contains the experimental Pascal transpiler.\n")
	buf.WriteString("Generated sources for the golden tests live under `tests/transpiler/x/pas`.\n\n")
	fmt.Fprintf(&buf, "## VM Golden Test Checklist (%d/%d)\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	ts := time.Now().Format("2006-01-02 15:04 MST")
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	fmt.Fprintf(&buf, "Last updated: %s\n", ts)
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "pas", "TASKS.md")
	compiled, total := countCompiled()
	out, err := exec.Command("git", "log", "-1", "--format=%cI;%s").Output()
	ts := ""
	msg := "updated"
	if err == nil {
		parts := strings.SplitN(strings.TrimSpace(string(out)), ";", 2)
		if len(parts) == 2 {
			if t, perr := time.Parse(time.RFC3339, parts[0]); perr == nil {
				ts = t.Format("2006-01-02 15:04 MST")
			}
			msg = parts[1]
		}
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- %s (progress %d/%d)\n\n", msg, compiled, total)
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
