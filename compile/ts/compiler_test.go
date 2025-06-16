package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"mochi/bench"
	tscode "mochi/compile/ts"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestTSCompiler_SubsetPrograms(t *testing.T) {
	if err := bench.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
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
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.ts")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("deno", "run", "--quiet", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
	golden.Run(t, "tests/compiler/ts", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.ts")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("deno", "run", "--quiet", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestTSCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".ts.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
	golden.Run(t, "tests/compiler/ts", ".mochi", ".ts.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("❌ parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("❌ type error: %v", errs[0])
		}
		modRoot, _ := mod.FindRoot(filepath.Dir(src))
		if modRoot == "" {
			modRoot = filepath.Dir(src)
		}
		c := tscode.New(typeEnv, modRoot)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("❌ compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}

func TestTSCompiler_LeetCodeExamples(t *testing.T) {
	if err := bench.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	for i := 1; i <= 25; i++ {
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
				c := tscode.New(typeEnv, "")
				code, err := c.Compile(prog)
				if err != nil {
					t.Fatalf("compile error: %v", err)
				}
				tmp := t.TempDir()
				file := filepath.Join(tmp, "main.ts")
				if err := os.WriteFile(file, code, 0644); err != nil {
					t.Fatalf("write error: %v", err)
				}
				cmd := exec.Command("deno", "run", "--quiet", file)
				cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
				out, err := cmd.CombinedOutput()
				if err != nil {
					t.Fatalf("deno run error: %v\n%s", err, out)
				}
				_ = out
			})
		}
	}
}
