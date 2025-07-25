//go:build slow

package rb_test

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
	rb "mochi/transpiler/x/rb"
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

func TestTranspilePrograms(t *testing.T) {
	if _, err := exec.LookPath("ruby"); err != nil {
		t.Skip("ruby not installed")
	}
	bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
	rb.SetBenchMain(bench)
	tests := []string{
		"print_hello",
		"append_builtin",
		"avg_builtin",
		"count_builtin",
		"len_builtin",
		"len_string",
		"sum_builtin",
		"let_and_print",
		"if_else",
		"basic_compare",
		"binary_precedence",
		"math_ops",
		"unary_neg",
		"string_compare",
		"string_concat",
		"if_then_else",
		"if_then_else_nested",
		"for_loop",
		"for_list_collection",
		"while_loop",
		"var_assignment",
		"break_continue",
		"list_assign",
		"len_map",
		"map_literal_dynamic",
		"fun_call",
		"fun_three_args",
		"cast_string_to_int",
		"cast_struct",
		"cross_join",
		"cross_join_filter",
		"list_index",
		"map_index",
		"map_assign",
		"typed_let",
		"typed_var",
		"in_operator",
		"map_in_operator",
		"string_in_operator",
		"values_builtin",
		"map_int_key",
		"map_membership",
		"string_contains",
		"bool_chain",
		"for_map_collection",
		"fun_expr_in_let",
		"list_nested_assign",
		"map_nested_assign",
		"membership",
		"min_max_builtin",
		"nested_function",
		"record_assign",
		"short_circuit",
		"slice",
		"str_builtin",
		"tail_recursion",
		"two-sum",
		"match_expr",
		"match_full",
		"partial_application",
		"string_index",
		"string_prefix_slice",
		"substring_builtin",
		"closure",
		"json_builtin",
		"tree_sum",
		"test_block",
		"bench_block",
		"list_set_ops",
		"cross_join_triple",
		"exists_builtin",
		"dataset_sort_take_limit",
		"sort_stable",
		"dataset_where_filter",
		"inner_join",
		"join_multi",
		"in_operator_extended",
		"right_join",
		"left_join_multi",
		"group_by_multi_join",
		"group_by_multi_join_sort",
		"group_by",
		"group_by_join",
		"group_by_left_join",
		"group_by_sort",
		"group_by_conditional_sum",
		"group_by_having",
		"load_yaml",
		"load_jsonl",
		"save_jsonl_stdout",
		"update_stmt",
		"user_type_literal",
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rb")
	os.MkdirAll(outDir, 0o755)
	for _, name := range tests {
		t.Run(name, func(t *testing.T) {
			src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type: %v", errs[0])
			}
			ast, err := rb.Transpile(prog, env)
			if err != nil {
				t.Fatalf("transpile: %v", err)
			}
			var buf bytes.Buffer
			if err := rb.Emit(&buf, ast); err != nil {
				t.Fatalf("emit: %v", err)
			}
			code := buf.Bytes()
			rbFile := filepath.Join(outDir, name+".rb")
			if err := os.WriteFile(rbFile, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			cmd := exec.Command("ruby", rbFile)
			envv := append(os.Environ(), "MOCHI_ROOT="+root)
			if bench {
				envv = append(envv, "MOCHI_BENCHMARK=1")
			} else {
				envv = append(envv, "MOCHI_NOW_SEED=1")
			}
			cmd.Env = envv
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
			wantPath := filepath.Join(root, "tests", "vm", "valid", name+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
				t.Fatal(err)
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
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "rb")
	readmePath := filepath.Join(root, "transpiler", "x", "rb", "README.md")
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
	buf.WriteString("# Mochi Ruby Transpiler\n\n")
	buf.WriteString("This directory contains Ruby code generated by the Mochi transpiler.\n\n")
	fmt.Fprintf(&buf, "## Golden Test Checklist (%d/%d)\n", compiled, total)
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			fmt.Fprintf(&buf, "Last updated: %s\n", t.Format("2006-01-02 15:04 MST"))
		}
	}
	buf.WriteString("\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "rb", "TASKS.md")
	out, _ := exec.Command("git", "log", "-1", "--format=%cI%n%h%n%s").Output()
	parts := strings.SplitN(strings.TrimSpace(string(out)), "\n", 3)
	ts := ""
	hash := ""
	msg := ""
	if len(parts) == 3 {
		ts = parts[0]
		hash = parts[1]
		msg = parts[2]
	}
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	files, _ := filepath.Glob(filepath.Join(root, "tests", "transpiler", "x", "rb", "*.rb"))
	compiled := len(files)
	srcFiles, _ := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	total := len(srcFiles)

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("## Progress (%s)\n", ts))
	if msg != "" {
		buf.WriteString(fmt.Sprintf("- Commit %s: %s\n", hash, msg))
	}
	fmt.Fprintf(&buf, "- Generated Ruby for %d/%d programs\n", compiled, total)
	buf.WriteString("- Updated README checklist and outputs\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
