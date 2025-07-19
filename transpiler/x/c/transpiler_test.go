//go:build slow

package ctrans_test

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
	ctrans "mochi/transpiler/x/c"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func transpileFile(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	ast, err := ctrans.Transpile(env, prog)
	if err != nil {
		return nil, err
	}
	return ast.Emit(), nil
}

func transpileAndRun(src string) ([]byte, error) {
	code, err := transpileFile(src)
	if err != nil {
		return nil, err
	}
	cc, err := ctrans.EnsureCC()
	if err != nil {
		return nil, err
	}
	tmp, err := os.MkdirTemp("", "ctranspile")
	if err != nil {
		return nil, err
	}
	base := filepath.Base(src)
	exe := filepath.Join(tmp, base)
	cFile := filepath.Join(tmp, base+".c")
	if err := os.WriteFile(cFile, code, 0o644); err != nil {
		return nil, err
	}
	if out, err := exec.Command(cc, cFile, "-o", exe).CombinedOutput(); err != nil {
		return nil, fmt.Errorf("compile failed: %v: %s", err, string(out))
	}
	return exec.Command(exe).CombinedOutput()
}

func updateEnabled() bool {
	return *update
}

func normalize(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}

func TestTranspilerGolden(t *testing.T) {
	if _, err := ctrans.EnsureCC(); err != nil {
		t.Skipf("C compiler not installed: %v", err)
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "transpiler", "x", "c")
	files := []string{
		filepath.Join(srcDir, "print_hello.mochi"),
		filepath.Join(srcDir, "unary_neg.mochi"),
		filepath.Join(srcDir, "let_and_print.mochi"),
		filepath.Join(srcDir, "var_assignment.mochi"),
		filepath.Join(srcDir, "while_loop.mochi"),
		filepath.Join(srcDir, "if_else.mochi"),
		filepath.Join(srcDir, "basic_compare.mochi"),
		filepath.Join(srcDir, "binary_precedence.mochi"),
		filepath.Join(srcDir, "for_loop.mochi"),
		filepath.Join(srcDir, "for_list_collection.mochi"),
		filepath.Join(srcDir, "len_builtin.mochi"),
		filepath.Join(srcDir, "len_string.mochi"),
		filepath.Join(srcDir, "math_ops.mochi"),
		filepath.Join(srcDir, "string_compare.mochi"),
		filepath.Join(srcDir, "string_concat.mochi"),
		filepath.Join(srcDir, "if_then_else.mochi"),
		filepath.Join(srcDir, "if_then_else_nested.mochi"),
		filepath.Join(srcDir, "bool_chain.mochi"),
		filepath.Join(srcDir, "break_continue.mochi"),
		filepath.Join(srcDir, "cast_string_to_int.mochi"),
		filepath.Join(srcDir, "fun_three_args.mochi"),
		filepath.Join(srcDir, "typed_let.mochi"),
		filepath.Join(srcDir, "typed_var.mochi"),
		filepath.Join(srcDir, "str_builtin.mochi"),
		filepath.Join(srcDir, "count_builtin.mochi"),
		filepath.Join(srcDir, "append_builtin.mochi"),
		filepath.Join(srcDir, "avg_builtin.mochi"),
		filepath.Join(srcDir, "substring_builtin.mochi"),
		filepath.Join(srcDir, "sum_builtin.mochi"),
	}
	if err := os.MkdirAll(goldenDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		wantOut := filepath.Join(goldenDir, name+".out")
		t.Run(name, func(t *testing.T) {
			code, err := transpileFile(src)
			if err != nil {
				t.Fatalf("transpile: %v", err)
			}
			if updateEnabled() {
				norm := normalize(root, code)
				if err := os.WriteFile(filepath.Join(goldenDir, name+".c"), norm, 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}

			got, runErr := transpileAndRun(src)
			if runErr != nil {
				if updateEnabled() {
					errPath := filepath.Join(goldenDir, name+".error")
					if werr := os.WriteFile(errPath, []byte(runErr.Error()+"\n"), 0o644); werr != nil {
						t.Fatalf("write error file: %v (run error: %v)", werr, runErr)
					}
				}
				t.Fatalf("run: %v", runErr)
			}
			if updateEnabled() {
				_ = os.Remove(filepath.Join(goldenDir, name+".error"))
				trimmed := bytes.TrimSpace(got)
				if err := os.WriteFile(wantOut, trimmed, 0o644); err != nil {
					t.Fatalf("write output: %v", err)
				}
			}
			trimmed := bytes.TrimSpace(got)
			wantData, err := os.ReadFile(wantOut)
			if err != nil {
				t.Fatalf("read expected: %v", err)
			}
			wantData = bytes.TrimSpace(wantData)
			if !bytes.Equal(trimmed, wantData) {
				t.Errorf("output mismatch for %s: got %q want %q", name, trimmed, wantData)
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

func countCompiled() (int, int) {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "c")
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
	outDir := filepath.Join(root, "tests", "transpiler", "x", "c")
	readmePath := filepath.Join(root, "transpiler", "x", "c", "README.md")
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
	buf.WriteString("# C Transpiler Golden Tests\n\n")
	buf.WriteString("This directory stores C translations generated from programs in `tests/vm/valid`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.\n\n")
	buf.WriteString(fmt.Sprintf("Checklist of programs that currently transpile and run (%d/%d):\n", compiled, total))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "c", "TASKS.md")
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
