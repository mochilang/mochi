package gocode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	gocode "mochi/compile/go"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestGoCompiler_SubsetPrograms(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.go")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("go", "run", file)
		cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ go run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})

	golden.Run(t, "tests/compiler/go", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.go")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("go", "run", file)
		cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ go run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestGoCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".go.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})

	golden.Run(t, "tests/compiler/go", ".mochi", ".go.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		c := gocode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestGoCompiler_LeetCodeExamples(t *testing.T) {
    for i := 1; i <= 92; i++ {
        runExample(t, i)
    }
	runExample(t, 102)
}

func runExample(t *testing.T, i int) {
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
			c := gocode.New(typeEnv)
			code, err := c.Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "main.go")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("go", "run", file)
			cmd.Env = append(os.Environ(), "GO111MODULE=on", "LLM_PROVIDER=echo")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go run error: %v\n%s", err, out)
			}
			// Older examples may print results; just ensure the
			// program executes without error.
			_ = out
		})
	}
}
