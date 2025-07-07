//go:build archived && slow

package mlir_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	mlir "mochi/archived/x/mlir"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestMLIRCompiler_Hello compiles hello.mochi to MLIR, builds a binary and runs it.
func TestMLIRCompiler_Hello(t *testing.T) {
	if err := mlir.EnsureMLIR(); err != nil {
		t.Skipf("mlir toolchain not installed: %v", err)
	}

	src := filepath.Join("..", "..", "examples", "v0.1", "hello.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}

	c, err := mlir.New()
	if err != nil {
		t.Fatalf("compiler setup: %v", err)
	}

	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}

	dir := t.TempDir()
	mlirFile := filepath.Join(dir, "hello.mlir")
	if err := os.WriteFile(mlirFile, code, 0644); err != nil {
		t.Fatalf("write mlir: %v", err)
	}

	llFile := filepath.Join(dir, "hello.ll")
	cmd := exec.Command("mlir-translate-19", "--mlir-to-llvmir", mlirFile)
	data, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			t.Fatalf("mlir-translate error: %v\n%s", err, string(ee.Stderr))
		}
		t.Fatalf("mlir-translate error: %v", err)
	}
	if err := os.WriteFile(llFile, data, 0644); err != nil {
		t.Fatalf("write ll: %v", err)
	}

	exe := filepath.Join(dir, "hello")
	clang := "clang-19"
	if _, err := exec.LookPath(clang); err != nil {
		clang = "clang"
	}
	if out, err := exec.Command(clang, llFile, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("clang error: %v\n%s", err, out)
	}

	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	if string(out) != "Hello, world\n" {
		t.Fatalf("unexpected output: %q", out)
	}
}

// TestMLIRCompiler_SubsetPrograms compiles a variety of Mochi programs to MLIR
// and ensures the resulting binaries run correctly. The generated MLIR is
// converted to LLVM IR with mlir-translate and compiled using clang.
func TestMLIRCompiler_SubsetPrograms(t *testing.T) {
	if err := mlir.EnsureMLIR(); err != nil {
		t.Skipf("mlir toolchain not installed: %v", err)
	}

	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c, err := mlir.New()
		if err != nil {
			return nil, fmt.Errorf("\u274c compiler setup: %w", err)
		}
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}

		tmp := t.TempDir()
		mlirFile := filepath.Join(tmp, "prog.mlir")
		if err := os.WriteFile(mlirFile, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}

		llFile := filepath.Join(tmp, "prog.ll")
		cmd := exec.Command("mlir-translate-19", "--mlir-to-llvmir", mlirFile)
		data, err := cmd.Output()
		if err != nil {
			if ee, ok := err.(*exec.ExitError); ok {
				return nil, fmt.Errorf("\u274c mlir-translate error: %v\n%s", err, ee.Stderr)
			}
			return nil, fmt.Errorf("\u274c mlir-translate error: %w", err)
		}
		if err := os.WriteFile(llFile, data, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}

		exe := filepath.Join(tmp, "prog")
		clang := "clang-19"
		if _, err := exec.LookPath(clang); err != nil {
			clang = "clang"
		}
		if out, err := exec.Command(clang, llFile, "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c clang error: %w\n%s", err, out)
		}

		cmd = exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
		}
		res := bytes.TrimSpace(out)
		if res == nil {
			res = []byte{}
		}
		return res, nil
	}

	golden.Run(t, "tests/compiler/c", ".mochi", ".out", run)
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", run)
}

// TestMLIRCompiler_GoldenOutput verifies that the generated MLIR matches the
// golden files. It compiles the same subset of programs as the C backend
// golden tests.
func TestMLIRCompiler_GoldenOutput(t *testing.T) {
	if err := mlir.EnsureMLIR(); err != nil {
		t.Skipf("mlir toolchain not installed: %v", err)
	}

	supported := map[string]bool{
		"arithmetic.mochi":              true,
		"avg_builtin.mochi":             true,
		"bool_ops.mochi":                true,
		"break_continue.mochi":          true,
		"count_builtin.mochi":           true,
		"factorial.mochi":               true,
		"fibonacci.mochi":               true,
		"float_literal_precision.mochi": true,
		"float_ops.mochi":               true,
		"fold_pure_let.mochi":           true,
		"for_list_collection.mochi":     true,
		"for_loop.mochi":                true,
		"for_string_collection.mochi":   true,
		"fun_call.mochi":                true,
		"generate_echo.mochi":           true,
		"grouped_expr.mochi":            true,
		"hello_world.mochi":             true,
		"if_else.mochi":                 true,
		"input_builtin.mochi":           true,
		"int_float_add.mochi":           true,
		"len_builtin.mochi":             true,
		"let_and_print.mochi":           true,
		"list_concat.mochi":             true,
		"list_except.mochi":             true,
		"list_float_ops.mochi":          true,
		"list_index.mochi":              true,
		"list_intersect.mochi":          true,
		"list_set.mochi":                true,
		"list_slice.mochi":              true,
		"list_string_param.mochi":       true,
		"list_union.mochi":              true,
		"list_union_all.mochi":          true,
		"match_basic.mochi":             true,
		"match_expr.mochi":              true,
		"matrix_search.mochi":           true,
		"membership.mochi":              true,
		"now_builtin.mochi":             true,
		"print_hello.mochi":             true,
		"reserved_keyword_var.mochi":    true,
		"simple_fn.mochi":               true,
		"str_builtin.mochi":             true,
		"stream_on_emit.mochi":          true,
		"string_concat.mochi":           true,
		"string_for_loop.mochi":         true,
		"string_len.mochi":              true,
		"string_slice.mochi":            true,
		"test_block.mochi":              true,
		"var_assignment.mochi":          true,
		"while_loop.mochi":              true,
	}

	compileFn := func(src string) ([]byte, error) {
		if !supported[filepath.Base(src)] {
			return nil, nil
		}
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c, err := mlir.New()
		if err != nil {
			return nil, fmt.Errorf("\u274c compiler setup: %w", err)
		}
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	run := func(dir string) {
		root := findRepoRoot(t)
		files, err := filepath.Glob(filepath.Join(root, dir, "*.mochi"))
		if err != nil {
			t.Fatalf("list files: %v", err)
		}
		for _, src := range files {
			if !supported[filepath.Base(src)] {
				continue
			}
			name := strings.TrimSuffix(filepath.Base(src), ".mochi")
			t.Run(name, func(t *testing.T) {
				got, err := compileFn(src)
				if err != nil {
					t.Fatalf("process error: %v", err)
				}
				got = normalizeOutput(t, got)
				wantPath := strings.TrimSuffix(src, ".mochi") + ".mlir.out"
				if updateFlag() {
					if err := os.WriteFile(wantPath, got, 0644); err != nil {
						t.Fatalf("write golden: %v", err)
					}
					t.Logf("updated: %s", wantPath)
					return
				}
				want, err := os.ReadFile(wantPath)
				if err != nil {
					t.Fatalf("read golden: %v", err)
				}
				want = bytes.TrimSpace(want)
				if !bytes.Equal(got, want) {
					t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", filepath.Base(wantPath), got, want)
				}
			})
		}
	}

	run("tests/compiler/c")
	run("tests/compiler/valid")
}

func updateFlag() bool {
	f := flag.Lookup("update")
	if f == nil {
		return false
	}
	if v, ok := f.Value.(interface{ Get() any }); ok {
		if b, ok := v.Get().(bool); ok {
			return b
		}
	}
	return false
}

func normalizeOutput(t *testing.T, b []byte) []byte {
	root := findRepoRoot(t)
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	tmpRE := regexp.MustCompile(`/tmp/mochi-mlir-[0-9]+`)
	out = tmpRE.ReplaceAllString(out, "/tmp/mochi-mlir-X")
	out = strings.TrimSpace(out)
	return []byte(out)
}

func findRepoRoot(t *testing.T) string {
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
