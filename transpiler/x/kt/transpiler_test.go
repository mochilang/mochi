//go:build slow

package kt_test

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
	kt "mochi/transpiler/x/kt"
	"mochi/types"
)

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

func TestTranspilePrograms(t *testing.T) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		t.Skip("kotlinc not installed")
	}
	cases := []string{
		"append_builtin",
		"avg_builtin",
		"basic_compare",
		"binary_precedence",
		"bool_chain",
		"break_continue",
		"cast_string_to_int",
		"closure",
		"count_builtin",
		"cross_join",
		"for_list_collection",
		"for_loop",
		"for_map_collection",
		"fun_call",
		"fun_expr_in_let",
		"fun_three_args",
		"group_by_multi_join",
		"if_else",
		"if_then_else",
		"if_then_else_nested",
		"len_builtin",
		"len_map",
		"len_string",
		"list_assign",
		"list_index",
		"list_nested_assign",
		"load_jsonl",
		"load_yaml",
		"map_assign",
		"map_in_operator",
		"map_literal_dynamic",
		"map_membership",
		"map_nested_assign",
		"match_expr",
		"match_full",
		"math_ops",
		"membership",
		"min_max_builtin",
		"nested_function",
		"order_by_map",
		"print_hello",
		"pure_fold",
		"pure_global_fold",
		"save_jsonl_stdout",
		"short_circuit",
		"slice",
		"sort_stable",
		"str_builtin",
		"string_compare",
		"string_concat",
		"string_contains",
		"string_in_operator",
		"string_index",
		"string_prefix_slice",
		"substring_builtin",
		"sum_builtin",
		"tail_recursion",
		"two-sum",
		"typed_let",
		"typed_var",
		"update_stmt",
		"unary_neg",
		"user_type_literal",
		"var_assignment",
		"while_loop",
	}
	sort.Strings(cases)
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	os.MkdirAll(outDir, 0o755)
	for _, name := range cases {
		src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("%s: parse: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("%s: type: %v", name, errs[0])
		}
		ast, err := kt.Transpile(env, prog)
		if err != nil {
			t.Fatalf("%s: transpile: %v", name, err)
		}
		code := kt.Emit(ast)
		ktFile := filepath.Join(outDir, name+".kt")
		if err := os.WriteFile(ktFile, code, 0o644); err != nil {
			t.Fatalf("%s: write: %v", name, err)
		}
		jar := filepath.Join(outDir, name+".jar")
		if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("%s: kotlinc: %v", name, err)
		}
		cmd := exec.Command("java", "-jar", jar)
		cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
		out, err := cmd.CombinedOutput()
		got := bytes.TrimSpace(out)
		if err != nil {
			_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
			t.Fatalf("%s: run: %v", name, err)
		}
		_ = os.Remove(filepath.Join(outDir, name+".error"))
		want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
		if err != nil {
			t.Fatalf("%s: read want: %v", name, err)
		}
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Fatalf("%s: output mismatch\nGot: %s\nWant: %s", name, got, want)
		}
		_ = os.Remove(jar)
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	updateTasks()
	kt.UpdateRosettaChecklist()
	os.Exit(code)
}

func updateReadme() {
	root := repoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".kt")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	outTs, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
	ts := ""
	if err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(outTs))); perr == nil {
			if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
				ts = t.In(loc).Format("2006-01-02 15:04 -0700")
			} else {
				ts = t.Format("2006-01-02 15:04 MST")
			}
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Kotlin Transpiler\n\n")
	buf.WriteString("Generated Kotlin sources for golden tests are stored in `tests/transpiler/x/kt`.\n\n")
	if ts != "" {
		buf.WriteString("Last updated: " + ts + "\n\n")
	}
	buf.WriteString("The transpiler currently supports expression programs with `print`, integer and list literals, mutable variables and built-ins `count`, `sum`, `avg`, `len`, `str`, `append`, `min`, `max`, `substring` and `values`.\n\n")
	fmt.Fprintf(&buf, "Completed golden tests: **%d/%d** (auto-generated)\n\n", compiled, total)
	buf.WriteString("### Golden test checklist\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(filepath.Join(root, "transpiler", "x", "kt", "README.md"), buf.Bytes(), 0o644)
}

func updateTasks() {
	root := repoRoot(&testing.T{})
	taskFile := filepath.Join(root, "transpiler", "x", "kt", "TASKS.md")
	out, err := exec.Command("git", "log", "-1", "--date=iso-strict", "--format=%cd").Output()
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
	buf.WriteString(fmt.Sprintf("## VM Golden Progress (%s)\n", ts))
	buf.WriteString("- Regenerated Kotlin golden files and README\n\n")
	if data, err := os.ReadFile(taskFile); err == nil {
		buf.Write(data)
	}
	_ = os.WriteFile(taskFile, buf.Bytes(), 0o644)
}
