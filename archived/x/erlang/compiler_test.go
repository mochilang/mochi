//go:build archived && slow

package erlcode_test

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	erlcode "mochi/archived/x/erlang"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestErlangCompiler_TwoSum(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := erlcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.erl")
	if err := os.WriteFile(file, code, 0755); err != nil {
		t.Fatalf("write error: %v", err)
	}
	out, err := exec.Command("escript", file).CombinedOutput()
	if err != nil {
		t.Fatalf("escript error: %v\n%s", err, out)
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	var filtered []string
	for _, line := range lines {
		if strings.Contains(line, "Warning") || strings.HasPrefix(line, "/tmp") {
			continue
		}
		filtered = append(filtered, line)
	}
	got := strings.Join(filtered, "\n")
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestErlangCompiler_LeetCode1(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	runLeetExample(t, 1)
}

// runLeetExample compiles and executes all Mochi programs under
// examples/leetcode/<id>. It fails the test if compilation or execution fails.
func runLeetExample(t *testing.T, id int) {
	t.Helper()
	dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, f := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(f))
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(f)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := erlcode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.erl")
			if err := os.WriteFile(file, code, 0755); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("escript", file)
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("escript error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}

func TestErlangCompiler_LeetCodeExamples(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	// Only the two-sum example currently compiles successfully.
	// Later LeetCode problems rely on language features
	// not yet supported by the Erlang backend.
	runLeetExample(t, 1)
}

func TestErlangCompiler_SubsetPrograms(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".out", func(src string) ([]byte, error) {
		erlOut, err := runErlang(src)
		if err != nil {
			return nil, err
		}
		vmOut, err := runVM(src)
		if err != nil {
			return nil, err
		}
		if !bytes.Equal(vmOut, erlOut) {
			return nil, fmt.Errorf("output mismatch\n\n--- VM ---\n%s\n\n--- Erlang ---\n%s\n", vmOut, erlOut)
		}
		if erlOut == nil {
			erlOut = []byte{}
		}
		return erlOut, nil
	})

	golden.Run(t, "tests/compiler/erl", ".mochi", ".out", func(src string) ([]byte, error) {
		erlOut, err := runErlang(src)
		if err != nil {
			return nil, err
		}
		vmOut, err := runVM(src)
		if err != nil {
			return nil, err
		}
		if !bytes.Equal(vmOut, erlOut) {
			return nil, fmt.Errorf("output mismatch\n\n--- VM ---\n%s\n\n--- Erlang ---\n%s\n", vmOut, erlOut)
		}
		if erlOut == nil {
			erlOut = []byte{}
		}
		return erlOut, nil
	})
}

func TestErlangCompiler_GoldenOutput(t *testing.T) {
	run := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := erlcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/compiler/erl_simple", ".mochi", ".erl.out", run)
	golden.Run(t, "tests/compiler/erl", ".mochi", ".erl.out", run)
}

func TestErlangCompiler_ValidVM(t *testing.T) {
	if err := erlcode.EnsureErlang(); err != nil {
		t.Skipf("erlang not installed: %v", err)
	}
	root := findRepoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "compiler", "valid", "*.mochi"))
	if err != nil {
		t.Fatal(err)
	}
	var errs []string
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			erlOut, err := runErlang(src)
			if err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Error(err)
				return
			}
			vmOut, err := runVM(src)
			if err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Error(err)
				return
			}
			if !bytes.Equal(vmOut, erlOut) {
				errs = append(errs, fmt.Sprintf("%s: output mismatch", name))
				t.Errorf("output mismatch\n--- VM ---\n%s\n--- ERL ---\n%s\n", vmOut, erlOut)
			}
		})
	}
	writeErrors(filepath.Join(root, "compile", "x", "erlang"), errs)
}

// runVM executes src using the runtime VM and returns trimmed stdout.
func runVM(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.CompileWithSource(prog, env, string(mustRead(src)))
	if err != nil {
		return nil, fmt.Errorf("vm compile error: %w", err)
	}
	var in io.Reader = os.Stdin
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		in = bytes.NewReader(data)
	}
	var out bytes.Buffer
	m := vm.NewWithIO(p, in, &out)
	if err := m.Run(); err != nil {
		if ve, ok := err.(*vm.VMError); ok {
			return nil, fmt.Errorf("vm run error:\n%s", ve.Format(p))
		}
		return nil, fmt.Errorf("vm run error: %v", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

// runErlang compiles src to Erlang, runs it with escript and returns stdout.
func runErlang(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("❌ parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("❌ type error: %v", errs[0])
	}
	code, err := erlcode.New(env).Compile(prog)
	if err != nil {
		return nil, fmt.Errorf("❌ compile error: %w", err)
	}
	dir, err := os.MkdirTemp("", "erl")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)
	file := filepath.Join(dir, "main.erl")
	if err := os.WriteFile(file, code, 0755); err != nil {
		return nil, fmt.Errorf("write error: %w", err)
	}
	cmd := exec.Command("escript", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("❌ escript error: %w\n%s", err, out)
	}
	return bytes.TrimSpace(out), nil
}

func writeErrors(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var b bytes.Buffer
	b.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		b.WriteString("None\n")
	} else {
		for _, e := range errs {
			b.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, b.Bytes(), 0644)
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

func mustRead(path string) []byte {
	b, _ := os.ReadFile(path)
	return b
}
