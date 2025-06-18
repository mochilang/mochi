//go:build slow

package rbcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	rbcode "mochi/compile/rb"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// compileAndRun compiles src to Ruby and executes the generated program.
// The program's trimmed combined output is returned.
func compileAndRun(t *testing.T, src string) (string, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		return "", fmt.Errorf("compile error: %w", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "main.rb")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return "", err
	}
	cmd := exec.Command("ruby", file)
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	return strings.TrimSpace(string(out)), err
}

// runLeetExample compiles and executes all Mochi files under examples/leetcode/<id>.
func runLeetExample(t *testing.T, id int) {
	dir := filepath.Join("..", "..", "examples", "leetcode", strconv.Itoa(id))
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := fmt.Sprintf("%d/%s", id, filepath.Base(src))
		t.Run(name, func(t *testing.T) {
			out, err := compileAndRun(t, src)
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
		})
	}
}

func TestRBCompiler_TwoSum(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	src := filepath.Join("..", "..", "examples", "leetcode", "1", "two-sum.mochi")
	got, err := compileAndRun(t, src)
	if err != nil {
		t.Fatalf("ruby run error: %v\n%s", err, got)
	}
	if got != "0\n1" {
		t.Fatalf("unexpected output: %q", got)
	}
}

func TestRBCompiler_LeetCodeExamples(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	runLeetExample(t, 1)
	runLeetExample(t, 2)
}

func TestRBCompiler_SubsetPrograms(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/rb", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, err
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.rb")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, err
		}
		cmd := exec.Command("ruby", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("ruby run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestRBCompiler_ValidPrograms(t *testing.T) {
	if err := rbcode.EnsureRuby(); err != nil {
		t.Skipf("ruby not installed: %v", err)
	}
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "compiler", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	skip := map[string]bool{
		"generate_echo":      true,
		"generate_embedding": true,
		"join":               true,
		"stream_on_emit":     true,
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if skip[name] {
			continue
		}
		wantPath := filepath.Join(root, "tests", "compiler", "valid", name+".out")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := rbcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.rb")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run error: %v\n%s", err, out)
			}
			got := bytes.TrimSpace(out)
			want, err := os.ReadFile(wantPath)
			if err != nil {
				t.Fatalf("read golden error: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
			}
		})
	}
}

func TestRBCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/rb", ".mochi", ".rb.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, errs[0]
		}
		code, err := rbcode.New(env).Compile(prog)
		if err != nil {
			return nil, err
		}
		return bytes.TrimSpace(code), nil
	})
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
