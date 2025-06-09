package excode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	excode "mochi/compile/ex"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func TestExCompiler_SubsetPrograms(t *testing.T) {
	if _, err := exec.LookPath("elixir"); err != nil {
		t.Skip("elixir not installed")
	}
	golden.Run(t, "tests/compiler/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := excode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.exs")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("elixir", file)
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c elixir run error: %w\n%s", err, out)
		}
		return bytes.TrimSpace(out), nil
	})
}

func TestExCompiler_GoldenOutput(t *testing.T) {
	golden.Run(t, "tests/compiler/valid", ".mochi", ".ex.out", func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		typeEnv := types.NewEnv(nil)
		if errs := types.Check(prog, typeEnv); len(errs) > 0 {
			return nil, fmt.Errorf("\u274c type error: %v", errs[0])
		}
		c := excode.New(typeEnv)
		code, err := c.Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}
		return bytes.TrimSpace(code), nil
	})
}
