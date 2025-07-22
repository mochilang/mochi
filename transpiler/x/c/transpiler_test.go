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
		filepath.Join(srcDir, "for_map_collection.mochi"),
		filepath.Join(srcDir, "len_builtin.mochi"),
		filepath.Join(srcDir, "len_string.mochi"),
		filepath.Join(srcDir, "len_map.mochi"),
		filepath.Join(srcDir, "math_ops.mochi"),
		filepath.Join(srcDir, "string_compare.mochi"),
		filepath.Join(srcDir, "string_concat.mochi"),
		filepath.Join(srcDir, "if_then_else.mochi"),
		filepath.Join(srcDir, "if_then_else_nested.mochi"),
		filepath.Join(srcDir, "bool_chain.mochi"),
		filepath.Join(srcDir, "break_continue.mochi"),
		filepath.Join(srcDir, "cast_string_to_int.mochi"),
		filepath.Join(srcDir, "fun_three_args.mochi"),
		filepath.Join(srcDir, "fun_expr_in_let.mochi"),
		filepath.Join(srcDir, "typed_let.mochi"),
		filepath.Join(srcDir, "typed_var.mochi"),
		filepath.Join(srcDir, "str_builtin.mochi"),
		filepath.Join(srcDir, "count_builtin.mochi"),
		filepath.Join(srcDir, "append_builtin.mochi"),
		filepath.Join(srcDir, "avg_builtin.mochi"),
		filepath.Join(srcDir, "substring_builtin.mochi"),
		filepath.Join(srcDir, "json_builtin.mochi"),
		filepath.Join(srcDir, "sum_builtin.mochi"),
		filepath.Join(srcDir, "string_contains.mochi"),
		filepath.Join(srcDir, "string_in_operator.mochi"),
		filepath.Join(srcDir, "list_index.mochi"),
		filepath.Join(srcDir, "string_index.mochi"),
		filepath.Join(srcDir, "list_assign.mochi"),
		filepath.Join(srcDir, "list_nested_assign.mochi"),
		filepath.Join(srcDir, "fun_call.mochi"),
		filepath.Join(srcDir, "tail_recursion.mochi"),
		filepath.Join(srcDir, "string_prefix_slice.mochi"),
		filepath.Join(srcDir, "slice.mochi"),
		filepath.Join(srcDir, "min_max_builtin.mochi"),
		filepath.Join(srcDir, "in_operator.mochi"),
		filepath.Join(srcDir, "in_operator_extended.mochi"),
		filepath.Join(srcDir, "membership.mochi"),
		filepath.Join(srcDir, "list_set_ops.mochi"),
		filepath.Join(srcDir, "short_circuit.mochi"),
		filepath.Join(srcDir, "pure_fold.mochi"),
		filepath.Join(srcDir, "pure_global_fold.mochi"),
		filepath.Join(srcDir, "test_block.mochi"),
		filepath.Join(srcDir, "cast_struct.mochi"),
		filepath.Join(srcDir, "record_assign.mochi"),
		filepath.Join(srcDir, "user_type_literal.mochi"),
		filepath.Join(srcDir, "match_expr.mochi"),
		filepath.Join(srcDir, "match_full.mochi"),
		filepath.Join(srcDir, "values_builtin.mochi"),
		filepath.Join(srcDir, "map_index.mochi"),
		filepath.Join(srcDir, "map_int_key.mochi"),
		filepath.Join(srcDir, "exists_builtin.mochi"),
		filepath.Join(srcDir, "cross_join.mochi"),
		filepath.Join(srcDir, "cross_join_filter.mochi"),
		filepath.Join(srcDir, "cross_join_triple.mochi"),
		filepath.Join(srcDir, "group_by.mochi"),
		filepath.Join(srcDir, "group_by_conditional_sum.mochi"),
		filepath.Join(srcDir, "group_by_join.mochi"),
		filepath.Join(srcDir, "group_by_left_join.mochi"),
		filepath.Join(srcDir, "left_join.mochi"),
		filepath.Join(srcDir, "left_join_multi.mochi"),
		filepath.Join(srcDir, "join_multi.mochi"),
		filepath.Join(srcDir, "group_by_having.mochi"),
		filepath.Join(srcDir, "group_by_sort.mochi"),
		filepath.Join(srcDir, "group_by_multi_join_sort.mochi"),
		filepath.Join(srcDir, "inner_join.mochi"),
		// dataset_sort_take_limit currently unsupported
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
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 -0700")
			}
		}
	}
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
	buf.WriteString(fmt.Sprintf("Checklist of programs that currently transpile and run (%d/%d) - Last updated %s:\n", compiled, total, ts))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "c", "TASKS.md")
	compiled, total := countCompiled()
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
	entry := fmt.Sprintf("## Progress (%s)\n- Commit %s: %s\n- Regenerated golden files - %d/%d vm valid programs passing\n\n", ts, hash, msg, compiled, total)
	if prev, err := os.ReadFile(taskFile); err == nil {
		sections := strings.Split(string(prev), "\n## ")
		count := 0
		for _, sec := range sections {
			if strings.HasPrefix(sec, "Progress") {
				if count >= 4 {
					break
				}
				entry += "## " + sec
				if !strings.HasSuffix(sec, "\n") {
					entry += "\n"
				}
				count++
			}
		}
	}
	_ = os.WriteFile(taskFile, []byte(entry), 0o644)
}
