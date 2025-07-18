//go:build slow

package tscode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	tscode "mochi/compiler/x/ts"
	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestTSCompiler_SubsetPrograms(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
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
               cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
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
               cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", file)
		cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("❌ deno run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

// TestTSCompiler_GoldenOutput previously compared generated TypeScript with
// golden files and validated behaviour against the Mochi VM. The VM comparison
// and code checks were redundant with the VM golden tests, so this test has
// been removed.

func TestTSCompiler_LeetCodeExamples(t *testing.T) {
	t.Skip("disabled in current environment")
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	for i := 1; i <= 133; i++ {
		runExample(t, i)
	}
	runExample(t, 272)
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
               cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", file)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			if data, err := os.ReadFile(strings.TrimSuffix(f, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("deno run error: %v\n%s", err, out)
			}
			_ = out
		})
	}
}
