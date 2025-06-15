package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	pycode "mochi/compile/py"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestPyCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})

	golden.Run(t, "tests/compiler/py", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.py")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("python3", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ python run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestPyCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".py.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})

	golden.Run(t, "tests/compiler/py", ".mochi", ".py.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := pycode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestPyCompiler_LeetCodeExamples(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	for i := 11; i <= 20; i++ {
		dir := filepath.Join("..", "..", "examples", "leetcode", fmt.Sprint(i))
		files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
		if err != nil {
			t.Fatalf("glob error: %v", err)
		}
		for _, f := range files {
			name := fmt.Sprintf("%d/%s", i, filepath.Base(f))
			t.Run(name, func(t *testing.T) {
				prog, err := parser.Parse(f)
				if err != nil {
					t.Fatalf("parse error: %v", err)
				}
				typeEnv := types.NewEnv(nil)
				if errs := types.Check(prog, typeEnv); len(errs) > 0 {
					t.Fatalf("type error: %v", errs[0])
				}
				c := pycode.New(typeEnv)
				code, err := c.Compile(prog)
				if err != nil {
					t.Fatalf("compile error: %v", err)
				}
				tmp := t.TempDir()
				file := filepath.Join(tmp, "main.py")
				if err := os.WriteFile(file, code, 0644); err != nil {
					t.Fatalf("write error: %v", err)
				}
				cmd := exec.Command("python3", file)
				out, err := cmd.CombinedOutput()
				if err != nil {
					t.Fatalf("python run error: %v\n%s", err, out)
				}
				if len(bytes.TrimSpace(out)) != 0 {
					t.Fatalf("unexpected output: %s", out)
				}
			})
		}
	}
}
