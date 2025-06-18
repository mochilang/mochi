//go:build slow

package cobolcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cobolcode "mochi/compile/cobol"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

// TestCobolCompiler_TwoSum compiles the LeetCode two-sum example to COBOL and runs it.
func TestCobolCompiler_TwoSum(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
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
	c := cobolcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	dir := t.TempDir()
	file := filepath.Join(dir, "prog.cob")
	if err := os.WriteFile(file, code, 0644); err != nil {
		t.Fatalf("write error: %v", err)
	}
	exe := filepath.Join(dir, "prog")
	if out, err := exec.Command("cobc", "-free", "-x", file, "-o", exe).CombinedOutput(); err != nil {
		t.Fatalf("cobc error: %v\n%s", err, out)
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		t.Fatalf("run error: %v\n%s", err, out)
	}
	got := bytes.TrimSpace(out)
	if string(got) != "0\n1" {
		t.Fatalf("unexpected output: %s", got)
	}
}

func TestCobolCompiler_SubsetPrograms(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := cobolcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.cob")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "prog")
		if out, err := exec.Command("cobc", "-free", "-x", file, "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c cobc error: %w\n%s", err, out)
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

	golden.Run(t, "tests/compiler/cobol", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := cobolcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "prog.cob")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		exe := filepath.Join(dir, "prog")
		if out, err := exec.Command("cobc", "-free", "-x", file, "-o", exe).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("\u274c cobc error: %w\n%s", err, out)
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

func TestCobolCompiler_GoldenOutput(t *testing.T) {
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".cob.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := cobolcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})

	golden.Run(t, "tests/compiler/cobol", ".mochi", ".cob.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := cobolcode.New(env)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

// TestCobolCompiler_LeetCodeExamples compiles and runs selected LeetCode
// programs from the examples directory. The helper runs all Mochi files under
// examples/leetcode/<id> using the COBOL backend.
func TestCobolCompiler_LeetCodeExamples(t *testing.T) {
	t.Skip("disabled in current environment")
	if err := cobolcode.EnsureCOBOL(); err != nil {
		t.Skipf("cobol not installed: %v", err)
	}
	runExample := func(id int) {
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
				c := cobolcode.New(env)
				out, err := c.CompileAndRun(prog)
				if err != nil {
					t.Fatalf("run error: %v\n%s", err, out)
				}
				if id == 1 {
					got := bytes.TrimSpace(out)
					if string(got) != "0\n1" {
						t.Fatalf("unexpected output: %s", got)
					}
				}
			})
		}
	}

	runExample(1)
	runExample(2)
}
