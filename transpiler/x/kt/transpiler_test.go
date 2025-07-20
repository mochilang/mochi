//go:build slow

package kt_test

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
		"print_hello",
		"count_builtin",
		"avg_builtin",
		"sum_builtin",
		"len_builtin",
		"len_string",
		"str_builtin",
		"let_and_print",
		"basic_compare",
		"binary_precedence",
		"math_ops",
		"unary_neg",
		"string_compare",
		"string_concat",
		"append_builtin",
		"min_max_builtin",
		"substring_builtin",
		"typed_let",
		"typed_var",
		"var_assignment",
		"len_map",
		"list_index",
		"string_contains",
		"string_in_operator",
		"string_index",
		"bool_chain",
		"if_else",
		"if_then_else",
		"if_then_else_nested",
		"break_continue",
		"for_list_collection",
		"for_loop",
		"while_loop",
		"fun_call",
		"fun_three_args",
		"list_assign",
		"map_assign",
		"slice",
		"string_prefix_slice",
		"cast_string_to_int",
		"for_map_collection",
		"list_nested_assign",
		"map_nested_assign",
		"tail_recursion",
		"two-sum",
	}
	root := repoRoot(t)
	outDir := filepath.Join(root, "tests", "transpiler", "x", "kt")
	os.MkdirAll(outDir, 0o755)
	for _, name := range cases {
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
			ast, err := kt.Transpile(env, prog)
			if err != nil {
				t.Fatalf("transpile: %v", err)
			}
			code := kt.Emit(ast)
			ktFile := filepath.Join(outDir, name+".kt")
			if err := os.WriteFile(ktFile, code, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			jar := filepath.Join(outDir, name+".jar")
			if out, err := exec.Command("kotlinc", ktFile, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("kotlinc: %v", err)
			}
			cmd := exec.Command("java", "-jar", jar)
			cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
			out, err := cmd.CombinedOutput()
			got := bytes.TrimSpace(out)
			if err != nil {
				_ = os.WriteFile(filepath.Join(outDir, name+".error"), out, 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(filepath.Join(outDir, name+".error"))
			want, err := os.ReadFile(filepath.Join(outDir, name+".out"))
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
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
	var buf bytes.Buffer
	buf.WriteString("# Kotlin Transpiler\n\n")
	buf.WriteString("Generated Kotlin sources for golden tests are stored in `tests/transpiler/x/kt`.\n\n")
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
