//go:build archived && slow

package rscode_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rscode "mochi/archived/x/rust"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestRustCompiler_SubsetPrograms(t *testing.T) {
	if err := rscode.Ensure(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rust", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := rscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c rustc error: %w\n%s", err, out)
		}
		cmd := exec.Command(exe)
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
	})
}

func TestRustCompiler_ValidPrograms(t *testing.T) {
	if err := rscode.Ensure(); err != nil {
		t.Skipf("rust not installed: %v", err)
	}

	files := []string{
		"print_hello",
		"var_assignment",
		"while_loop",
	}

	for _, name := range files {
		src := filepath.Join("tests", "compiler", "valid", name+".mochi")
		t.Run(name, func(t *testing.T) {
			runRustProgram(t, src)
		})
	}

	// Also ensure some of the bundled LeetCode examples still compile and run.
	t.Run("leetcode_1", func(t *testing.T) {
		runRustLeet(t, 1)
	})
	t.Run("leetcode_2", func(t *testing.T) {
		runRustLeet(t, 2)
	})
	t.Run("leetcode_3", func(t *testing.T) {
		runRustLeet(t, 3)
	})
	t.Run("leetcode_4", func(t *testing.T) {
		runRustLeet(t, 4)
	})
	t.Run("leetcode_5", func(t *testing.T) {
		runRustLeet(t, 5)
	})
	t.Run("leetcode_6", func(t *testing.T) {
		runRustLeet(t, 6)
	})
	t.Run("leetcode_7", func(t *testing.T) {
		runRustLeet(t, 7)
	})
	t.Run("leetcode_8", func(t *testing.T) {
		runRustLeet(t, 8)
	})
}

func runRustProgram(t *testing.T, src string) {
	root := findRoot(t)
	path := filepath.Join(root, src)
	prog, err := parser.Parse(path)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := rscode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("rustc error: %v\n%s", err, out)
	}
	cmd := exec.Command(exe)
	if data, err := os.ReadFile(strings.TrimSuffix(path, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	wantPath := strings.TrimSuffix(path, ".mochi") + ".out"
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("failed to read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("unexpected output for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", path, got, want)
	}
}

// runRustLeet compiles and runs all Mochi files in examples/leetcode/<id>.
// If an <file>.out file exists beside the source it is used as the expected
// output. Otherwise the program is just executed to ensure it runs.
func runRustLeet(t *testing.T, id int) {
	root := findRoot(t)
	dir := filepath.Join(root, "examples", "leetcode", fmt.Sprintf("%d", id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			got := compileAndRunRust(t, src)
			wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
			if data, err := os.ReadFile(wantPath); err == nil {
				want := strings.TrimSpace(string(data))
				if got != want {
					t.Fatalf("unexpected output: %q", got)
				}
			} else if id == 1 && name == "two-sum.mochi" {
				if got != "0\n1" {
					t.Fatalf("unexpected output: %q", got)
				}
			}
		})
	}
}

// compileAndRunRust compiles the given Mochi source file to Rust, executes the
// resulting binary and returns its trimmed stdout.
func compileAndRunRust(t *testing.T, src string) string {
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := rscode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rs")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("rustc error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
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

func TestRustCompiler_GoldenOutput(t *testing.T) {
	if err := rscode.Ensure(); err != nil {
		t.Skipf("rust not installed: %v", err)
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
		c := rscode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.rs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "main")
		if out, err := exec.Command("rustc", file, "-O", "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c rustc error: %w\n%s", err, out)
		}
		cmd := exec.Command(exe)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c run error: %w\n%s", err, out)
		}
		got := bytes.TrimSpace(out)

		vmOut, err := runVM(src)
		if err != nil {
			return nil, fmt.Errorf("vm error: %w", err)
		}
		if !bytes.Equal(got, vmOut) {
			return nil, fmt.Errorf("runtime output mismatch\n\n--- Rust ---\n%s\n\n--- VM ---\n%s\n", got, vmOut)
		}

		return bytes.TrimSpace(code), nil
	}

	files := []string{
		"print_hello",
		"var_assignment",
		"while_loop",
	}

	root := findRoot(t)
	for _, name := range files {
		src := filepath.Join(root, "tests", "compiler", "valid", name+".mochi")
		t.Run(name, func(t *testing.T) {
			code, err := run(src)
			if err != nil {
				t.Fatal(err)
			}
			wantPath := filepath.Join(root, "tests", "compiler", "valid", name+".rs.out")
			if shouldUpdate() {
				if err := os.WriteFile(wantPath, code, 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				t.Logf("updated: %s", wantPath)
				return
			}
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			got := bytes.TrimSpace(code)
			if !bytes.Equal(got, bytes.TrimSpace(want)) {
				t.Errorf("generated code mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", wantPath, got, bytes.TrimSpace(want))
			}
		})
	}
}

func runVM(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("run error: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}
