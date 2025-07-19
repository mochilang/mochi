//go:build slow

package cobol_test

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
	cobol "mochi/transpiler/x/cobol"
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

func TestTranspile_Golden(t *testing.T) {
	if err := cobol.EnsureCOBOL(); err != nil {
		t.Skip("cobc not installed")
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	os.MkdirAll(outDir, 0o755)

	cases := []string{
		"print_hello",
		"string_concat",
		"unary_neg",
		"var_assignment",
		"typed_let",
		"typed_var",
		"let_and_print",
		"if_else",
		"basic_compare",
		"binary_precedence",
		"math_ops",
		"len_string",
		"if_then_else",
		"if_then_else_nested",
		"for_loop",
		"while_loop",
	}
	for _, name := range cases {
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", name, errs[0])
		}
		ast, err := cobol.Transpile(prog, env)
		if err != nil {
			t.Fatalf("transpile %s: %v", name, err)
		}
		code := cobol.Emit(ast)
		cobPath := filepath.Join(outDir, name+".cob")
		if err := os.WriteFile(cobPath, code, 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
		exe := filepath.Join(outDir, name)
		if out, err := exec.Command("cobc", "-free", cobPath, "-x", "-o", exe).CombinedOutput(); err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("compile %s: %v", name, err)
		}
		out, err := exec.Command(exe).CombinedOutput()
		_ = os.Remove(exe)
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("run %s: %v", name, err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		wantPath := filepath.Join(outDir, name+".out")
		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Fatalf("read want %s: %v", name, err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("%s output mismatch:\nGot: %s\nWant: %s", name, got, want)
		}
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	readmePath := filepath.Join(root, "transpiler", "x", "cobol", "README.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".cob")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	var buf bytes.Buffer
	buf.WriteString("# COBOL Transpiler\n\n")
	buf.WriteString("This directory stores COBOL code generated from Mochi programs in `tests/vm/valid`.\n")
	buf.WriteString("Each program is transpiled and the resulting `.cob` sources are compiled with `cobc` during testing.\n\n")
	fmt.Fprintf(&buf, "Checklist of programs that currently transpile and run (%d/%d):\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "cobol", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(out))
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, ts); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "cobol")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".cob")); err == nil {
			compiled++
		}
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "## Progress (%s)\n", ts)
	fmt.Fprintf(&buf, "- VM valid golden tests %d/%d compiled\n\n", compiled, total)
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
