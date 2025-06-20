//go:build slow

package dartcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	dartcode "mochi/compile/dart"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestDartCompiler_LeetCodeExamples(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	runLeetExample(t, 1, "0\n1")
	runLeetExample(t, 2, "")
	runLeetExample(t, 3, "")
}

func TestDartCompiler_SubsetPrograms(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/dart", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := dartcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.dart")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("dart", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ dart run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	})
}

func TestDartCompiler_GoldenOutput(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/dart", ".mochi", ".dart.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := dartcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestDartCompiler_ValidGoldenOutput(t *testing.T) {
	if err := dartcode.EnsureDart(); err != nil {
		t.Skipf("dart not installed: %v", err)
	}
	files := []string{
		"break_continue",
		"fold_pure_let",
		"for_list_collection",
		"for_loop",
		"for_string_collection",
		"fun_expr_in_let",
		"fun_call",
		"grouped_expr",
		"match_expr",
		"if_else",
		"len_builtin",
		"let_and_print",
		"list_index",
		"list_set",
		"map_index",
		"map_ops",
		"map_set",
		"print_hello",
		"string_index",
		"test_block",
		"two_sum",
		"var_assignment",
		"while_loop",
		"local_recursion",
		"union_inorder",
		"union_match",
		"union_slice",
		"stream_on_emit",
	}
	for _, name := range files {
		src := filepath.Join("tests", "compiler", "valid", name+".mochi")
		runDartGolden(t, src, ".dart.out")
	}
}

func runDartGolden(t *testing.T, src, goldenExt string) {
	t.Helper()
	root := findRoot(t)
	path := filepath.Join(root, src)
	name := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	wantPath := filepath.Join(root, filepath.Dir(src), name+goldenExt)

	prog, err := parser.Parse(path)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := dartcode.New(env)
	got, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	got = bytes.TrimSpace(got)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("failed to read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name+goldenExt, got, want)
	}
}

// runLeetExample compiles the Mochi LeetCode solution for the given ID and runs
// the generated Dart code. If want is non-empty, the program output must match.
func runLeetExample(t *testing.T, id int, want string) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no source for problem %d", id)
	}
	src := files[0]
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	code, err := dartcode.New(env).Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	tmp := t.TempDir()
	file := filepath.Join(tmp, "main.dart")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	cmd := exec.Command("dart", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("dart run error: %v\n%s", err, out)
	}
	if want == "" {
		return
	}
	got := strings.ReplaceAll(string(out), "\r\n", "\n")
	if strings.TrimSpace(got) != want {
		t.Fatalf("unexpected output\nwant:\n%s\n got:\n%s", want, got)
	}
}

func findRoot(t *testing.T) string {
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
